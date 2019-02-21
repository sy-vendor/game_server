%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 订单系统
%%% @end
%%% Created : 03. 一月 2019 11:39
%%%-------------------------------------------------------------------
-module(lib_order_api).
-author("suyang").

%% API
-export([init/3,
    save/2,
    production_order/1,
    loop_order/1,
    accept_order/2,
    delivery_order/2,
    reward_order/2,
    send_order_info/1]).

-include("order.hrl").
-include("role.hrl").
-include("common.hrl").
-include("err_code.hrl").
-include("merge.hrl").
-include("logic.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 登陆初始化
init(_NowTime, _LastLogOutTime, Role) ->
    Sql = io_lib:format(?SQL_ROLE_ORDER_FETCH, [Role#role.role_id]),
    F = fun([OrderId, MapId, OrderType, OrderTime, OrderReward]) ->
        #role_order{order_id = OrderId, map_id = MapId, order_type = OrderType, order_time = OrderTime, order_reward = OrderReward} end,
    case db:get_all(Sql) of
        {ok, List} ->
            OrderList = lists:map(F, List),
            put(?DICT_ROLE_ORDER, OrderList);
        _ ->
            MaxType = lib_merge_api:get_max_type(),
            OrderLv = conf_common:get_order_lv(),
            ?IF(MaxType >= OrderLv, put(?DICT_ROLE_ORDER, []), ignore)
    end.

%% @doc 下线保存
save(_NowTime, Role) ->
    OrderList = get(?DICT_ROLE_ORDER),
    case OrderList of
        _ when is_list(OrderList) andalso OrderList =/= [] ->
            F = fun(RoleOlder, DbOrderListAcc) ->
                #role_order{order_id = OrderId, map_id = MapId, order_type = OrderType, order_time = OrderTime, order_reward = OrderReward} = RoleOlder,
                DbOrder = [Role#role.role_id, OrderId, MapId, OrderType, OrderTime, OrderReward],
                [DbOrder | DbOrderListAcc]
                end,
            DbOrderListAccN = lists:foldl(F, [], OrderList),
            % 删除旧订单
            Sql = io_lib:format(?SQL_ROLE_ORDER_DELETE, [Role#role.role_id]),
            db:execute(Sql),
            util:insert_values(?SQL_ROLE_ORDER_REPLACE_P1, ?SQL_ROLE_ORDER_REPLACE_P2, DbOrderListAccN);
        _ -> ignore
    end.

%% @doc 订单生成
production_order(Role) ->
    MaxType = util_dist:get(?DICT_MAX_TYPE_LOOP, 1),
    OrderLv = conf_common:get_order_lv(),
    production_order(Role, MaxType, OrderLv).
production_order(_Role, MaxType, OrderLv) when MaxType < OrderLv -> ignore;
production_order(Role, _MaxType, _OrderLv) ->
    OrderList = get(?DICT_ROLE_ORDER),
    case OrderList =:= undefined orelse length(OrderList) >= 10 of
        true -> ignore;
        false -> production_order_new(Role, OrderList)
    end.

production_order_new(Role, OrderList) ->
    NowTime = util_time:unixtime(),
    MapIdNew = random_order(),
    OrderIdNew = lib_data_api:get_value(Role, order_next_id, 1),
    lib_data_api:put_value(Role#role.role_id, order_next_id, OrderIdNew + 1),
    [OrderTimeNew, _, _] = conf_order:get_order_time(MapIdNew),
    OrderNew = #role_order{order_id = OrderIdNew, map_id = MapIdNew, order_type = ?ORDER_TYPE_NOTHING, order_time = OrderTimeNew + NowTime},
    OrderListN = [OrderNew | OrderList],
    put(?DICT_ROLE_ORDER, OrderListN),
    send_order_info(Role).

%% @doc 订单检查
loop_order(Role) ->
    NowTime = util_time:unixtime(),
    OrderList = get(?DICT_ROLE_ORDER),
    loop_order(Role, NowTime, OrderList).

loop_order(Role, NowTime, OrderList) ->
    OverOrderList1 = lists:filter(fun(#role_order{order_time = OrderTime, order_type = OrderType}) ->
        NowTime > OrderTime andalso OrderType =:= ?ORDER_TYPE_NOTHING end, OrderList),
    OverOrderList2 = lists:filter(fun(#role_order{order_time = OrderTime, order_type = OrderType}) ->
        NowTime > OrderTime andalso OrderType =:= ?ORDER_TYPE_READY end, OrderList),
    OverOrderList = OverOrderList1 ++ OverOrderList2,
    OrderListN = OrderList -- OverOrderList,
    put(?DICT_ROLE_ORDER, OrderListN),
    case OrderList =:= OrderListN orelse OrderList =:= undefined of
        true -> ignore;
        false -> send_order_info(Role)
    end.

%% @doc 接单
accept_order(Role, Id) ->
    case catch check_accept_order(Id) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11506, #'OrderCommonRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, RoleOrder, OrderList} ->
            OrderListN = lists:keystore(Id, #role_order.order_id, OrderList, RoleOrder),
            put(?DICT_ROLE_ORDER, OrderListN),
            {ok, Bin} = lib_proto:pack(11506, #'OrderCommonRes'{code = ?SUCCESS}),
            lib_send:send_to_role(Role, Bin),
            {ok, Role}
    end.

%% @doc 配送
delivery_order(Role, Id) ->
    case catch check_delivery_order(Id) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11506, #'OrderCommonRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, _PosList, OrderList, RoleOrder} ->
            OrderListN = lists:keystore(Id, #role_order.order_id, OrderList, RoleOrder),
            put(?DICT_ROLE_ORDER, OrderListN),
            {ok, Bin} = lib_proto:pack(11506, #'OrderCommonRes'{code = ?SUCCESS}),
            lib_send:send_to_role(Role, Bin),
            {ok, Role}
    end.

%% @doc 奖励领取
reward_order(Role, Id) ->
    case catch check_reward_order(Id) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11506, #'OrderCommonRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Asset, Reputation, OrderList} ->
            Rate = lib_waiter_api:get_type_add(?WAITER_SKILL_TYPE_ONE),
            AssetN = Asset + trunc(Asset * Rate / 100),
            RoleN = lib_role_asset:add_assets([{?TYPE_COIN, AssetN}, {?TYPE_REPUTATION, Reputation}], Role),
            OrderListN = lists:keydelete(Id, #role_order.order_id, OrderList),
            put(?DICT_ROLE_ORDER, OrderListN),
            {ok, Bin} = lib_proto:pack(11506, #'OrderCommonRes'{code = ?SUCCESS}),
            lib_send:send_to_role(Role, Bin),
            {ok, RoleN}
    end.

send_order_info(Role) ->
    OrderList = get(?DICT_ROLE_ORDER),
    case OrderList of
        _ when is_list(OrderList) andalso OrderList =/= [] ->
            OrderData = switch_to_data(OrderList),
            {ok, Bin} = lib_proto:pack(11502, #'OrderInfoRes'{orderInfo = OrderData}),
            lib_send:send_to_role(Role, Bin);
        _ -> ignore
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 随机生成订单
random_order() ->
    List = conf_order:get_config_list(),
    {Id, _} = util_math:rand_list_one(List, 2),
    Id.

%% @doc 接单检查
check_accept_order(Id) ->
    RoleOrderList = get(?DICT_ROLE_ORDER),
    % 检查功能是否开启
    ?IF(RoleOrderList =:= undefined, erlang:throw({error, ?ERR_COMMON_FUNC_UNOPEND}), ignore),
    % 检查该订单是否超时
    ?IF(lists:keyfind(Id, #role_order.order_id, RoleOrderList) =:= false, erlang:throw({error, ?ERR_COMMON_ORDER_OUTTIME}), ignore),
    RoleOrder = lists:keyfind(Id, #role_order.order_id, RoleOrderList),
    % 检查该订单是否已经接受
    ?IF(RoleOrder#role_order.order_type =:= ?ORDER_TYPE_READY, erlang:throw({error, ?ERR_COMMON_ORDER_ACCEPT}), ignore),
    % 检查是否是已完成订单
    ?IF(RoleOrder#role_order.order_type =:= ?ORDER_TYPE_COMPLETE, erlang:throw({error, ?ERR_COMMON_ORDER_COMMPLET}), ignore),
    [_, ReadTime, _] = conf_order:get_order_time(RoleOrder#role_order.map_id),
    MaxType = lib_merge_api:get_max_type(),
    Asset = conf_building:get_config_production(MaxType),
    RoleOrderN = RoleOrder#role_order{order_type = ?ORDER_TYPE_READY, order_time = ReadTime + util_time:unixtime(), order_reward = Asset},
    {true, RoleOrderN, RoleOrderList}.

%% @doc 配送检查
check_delivery_order(Id) ->
    RoleOrderList = get(?DICT_ROLE_ORDER),
    % 检查功能是否开启
    ?IF(RoleOrderList =:= undefined, erlang:throw({error, ?ERR_COMMON_FUNC_UNOPEND}), ignore),
    % 检查该订单是否超时
    ?IF(lists:keyfind(Id, #role_order.order_id, RoleOrderList) =:= false, erlang:throw({error, ?ERR_COMMON_ORDER_OUTTIME}), ignore),
    RoleOrder = lists:keyfind(Id, #role_order.order_id, RoleOrderList),
    % 检查是否接受该订单
    ?IF(RoleOrder#role_order.order_type =/= ?ORDER_TYPE_READY, erlang:throw({error, ?ERR_COMMON_ORDER_NOACCEPT}), ignore),
    % 检查是否是已完成订单
    ?IF(RoleOrder#role_order.order_type =:= ?ORDER_TYPE_COMPLETE, erlang:throw({error, ?ERR_COMMON_ORDER_COMMPLET}), ignore),
    CostList = conf_order:get_order_material(RoleOrder#role_order.map_id),
    ?IF(CostList =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    [{ConfType1, Num1}, {ConfType2, Num2}, {ConfType3, Num3}] = CostList,
    MaxType = lib_merge_api:get_max_type(),
    ?IF(ConfType1 >= MaxType orelse ConfType2 >= MaxType orelse ConfType3 >= MaxType, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    % 检查食材
    RoleMerge = get(?DICT_ROLE_MAP),
    {Val1, Val2, Val3} = get_type_value(RoleMerge#role_merge.pos, MaxType - ConfType1, MaxType - ConfType2, MaxType - ConfType3),
    ?IF(Val1 >= Num1 andalso Val2 >= Num2 andalso Val3 >= Num3, ignore, erlang:throw({error, ?ERR_COMMON_INSUFFICIENT_MATERIAL})),
    % 获得新的PosList
    PosList1 = del_pos_list(MaxType - ConfType1, Num1, RoleMerge#role_merge.pos),
    PosList2 = del_pos_list(MaxType - ConfType2, Num2, PosList1),
    PosListN = del_pos_list(MaxType - ConfType3, Num3, PosList2),
    RoleMergeN = RoleMerge#role_merge{pos = PosListN, sign = true},
    put(?DICT_ROLE_MAP, RoleMergeN),
    [_, _, DelivertTime] = conf_order:get_order_time(RoleOrder#role_order.map_id),
    RoleOrderN = RoleOrder#role_order{order_type = ?ORDER_TYPE_COMPLETE, order_time = DelivertTime + util_time:unixtime()},
    {true, PosListN, RoleOrderList, RoleOrderN}.

%% @doc 奖励领取检查
check_reward_order(Id) ->
    RoleOrderList = get(?DICT_ROLE_ORDER),
    % 检查功能是否开启
    ?IF(RoleOrderList =:= undefined, erlang:throw({error, ?ERR_COMMON_FUNC_UNOPEND}), ignore),
    % 检查该订单是否超时
    ?IF(lists:keyfind(Id, #role_order.order_id, RoleOrderList) =:= false, erlang:throw({error, ?ERR_COMMON_ORDER_OUTTIME}), ignore),
    RoleOrder = lists:keyfind(Id, #role_order.order_id, RoleOrderList),
    % 检查是否是已完成订单
    ?IF(RoleOrder#role_order.order_type =/= ?ORDER_TYPE_COMPLETE, erlang:throw({error, ?ERR_COMMON_ORDER_COMMPLET}), ignore),
    % 检查是否完成
    ?IF(util_time:unixtime() < RoleOrder#role_order.order_time, erlang:throw({error, ?ERR_COMMON_ORDER_NOCOMMPLET}), ignore),
    Reward = conf_order:get_order_reward(RoleOrder#role_order.map_id),
    ?IF(Reward =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    [RewardTime, Reputation] = Reward,
    Asset = trunc(RewardTime * RoleOrder#role_order.order_reward),
    {true, Asset, Reputation, RoleOrderList}.

get_type_value(PosList, Type1, Type2, Type3) ->
    TypeList1 = lists:filter(fun({_, Type}) -> Type =:= Type1 end, PosList),
    TypeList2 = lists:filter(fun({_, Type}) -> Type =:= Type2 end, PosList),
    TypeList3 = lists:filter(fun({_, Type}) -> Type =:= Type3 end, PosList),
    {length(TypeList1), length(TypeList2), length(TypeList3)}.

del_pos_list(_Val, 0, PosList) -> PosList;
del_pos_list(Val, Num, PosList) ->
    PosListN = case lists:keyfind(Val, 2, PosList) of
                   {Pos, Val} -> lists:keystore(Pos, 1, PosList, {Pos, 0});
                   _ -> PosList
               end,
    del_pos_list(Val, Num - 1, PosListN).


%% @doc 转换协议数据
switch_to_data(OrderList) ->
    F = fun(#role_order{order_id = OrderId, order_time = OrderTime, order_type = OrderType, map_id = MapId}) ->
        #'OrderInfo'{orderId = OrderId, orderTime = OrderTime, orderState = OrderType, mapId = MapId} end,
    lists:map(F, OrderList).