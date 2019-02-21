%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 商店
%%% @end
%%% Created : 18. 十二月 2018 20:17
%%%-------------------------------------------------------------------
-module(lib_shop_api).
-author("suyang").

%% API
-export([go_shopping/2,
    shop_info/1]).

-include("common.hrl").
-include("role.hrl").
-include("err_code.hrl").
-include("merge.hrl").
-include("logic.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 商店购买
go_shopping(Id, Role) ->
    case catch check_shopping(Id, Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11111, #'BuyObjectRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Pos, Cost, MaxType} ->
            % 扣除消耗
            RoleN = lib_role_asset:cost_assets(Cost, Role),
            % 更改数据
            PosList = lib_merge_api:get_pos_list(),
            PosListN = lists:keystore(Pos, 1, PosList, {Pos, Id}),
            RoleMerge = get(?DICT_ROLE_MAP),
            RoleMergeN = RoleMerge#role_merge{pos = PosListN, sign = true},
            put(?DICT_ROLE_MAP, RoleMergeN),
            % 更改购买永久数据
            refresh_shop_record(MaxType, Id, RoleN),
            PosData = lib_merge_api:switch_to_data(Pos, Id),
            {ok, Bin} = lib_proto:pack(11111, #'BuyObjectRes'{posInfo = PosData}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 商店信息请求
shop_info(Role) ->
    ShopList = get_shop_info(Role),
    ShopData = swich_to_data(ShopList),
    {ok, Bin} = lib_proto:pack(11113, #'ShopInfoRes'{shopInfo = ShopData}),
    lib_send:send_to_role(Role, Bin),
    {ok}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 购买检查
check_shopping(Id, Role) ->
    % 判断格子是否还有位置
    PosList = lib_merge_api:get_pos_list(),
    ?IF(lists:keyfind(0, 2, PosList) =:= false, erlang:throw({error, ?ERR_COMMON_MAP_LIMIT}), ignore),
    {Pos, _} = lists:keyfind(0, 2, PosList),
    % 判断是否能购买
    MaxType = lib_merge_api:get_max_type(),
    ?IF(MaxType =< 2, erlang:throw({error, ?ERR_COMMON_BUY_ERR}), ignore),
    CostList = case Id > MaxType - 4 of
                   true ->
                       Arg = conf_building:get_grow_value2(Id),
                       ?IF(Arg =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
                       RecordList = lib_data_api:get_value(Role, record_list_1, []),
                       {Id, Cnt} = ?IF(lists:keyfind(Id, 1, RecordList) =:= false, {Id, 1}, lists:keyfind(Id, 1, RecordList)),
                       {Init, Grow} = Arg,
                       Cost = trunc(Init * math:pow(Grow, Cnt)),
                       [{?TYPE_DIAMOND, Cost}];
                   false ->
                       Arg = conf_building:get_grow_value1(Id),
                       ?IF(Arg =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
                       RecordList = lib_data_api:get_value(Role, record_list_2, []),
                       {Id, Cnt} = ?IF(lists:keyfind(Id, 1, RecordList) =:= false, {Id, 1}, lists:keyfind(Id, 1, RecordList)),
                       {Init, Grow} = Arg,
                       Cost = trunc(Init * math:pow(Grow, Cnt)),
                       [{?TYPE_COIN, Cost}]
               end,
    % 判断消耗
    IsEnough = lib_role_asset:is_asset_enough(CostList, Role),
    ?IF(IsEnough =:= false, erlang:throw({error, ?ERR_COMMON_COST_LIMIT}), ignore),
    {true, Pos, CostList, MaxType}.

%% @doc 获取商店列表
get_shop_info(Role) ->
    MaxType = lib_merge_api:get_max_type(),
    RecordList1 = lib_data_api:get_value(Role, record_list_1, []),
    RecordList2 = lib_data_api:get_value(Role, record_list_2, []),
    ?IF(MaxType =< 2, [], shop_list_info(MaxType, 1, RecordList1, RecordList2, [])).

shop_list_info(MaxType, Index, _RecordList1, _RecordList2, AccList) when Index > MaxType - 2 -> AccList;
shop_list_info(MaxType, Index, RecordList1, RecordList2, AccList) ->
    Args = case Index > MaxType - 4 of
               true ->
                   Arg = conf_building:get_grow_value2(Index),
                   {Index, Cnt} = ?IF(lists:keyfind(Index, 1, RecordList1) =:= false, {Index, 1}, lists:keyfind(Index, 1, RecordList1)),
                   {Init, Grow} = Arg,
                   Cost = trunc(Init * math:pow(Grow, Cnt)),
                   {Index, ?TYPE_DIAMOND, Cost};
               false ->
                   Arg = conf_building:get_grow_value1(Index),
                   {Index, Cnt} = ?IF(lists:keyfind(Index, 1, RecordList2) =:= false, {Index, 1}, lists:keyfind(Index, 1, RecordList2)),
                   {Init, Grow} = Arg,
                   Cost = trunc(Init * math:pow(Grow, Cnt)),
                   {Index, ?TYPE_COIN, Cost}
           end,
    AccListN = [Args | AccList],
    shop_list_info(MaxType, Index + 1, RecordList1, RecordList2, AccListN).

%% @doc 更新最大数据
refresh_shop_record(MaxType, Id, Role) ->
    case Id > MaxType - 4 of
        true ->
            RecordList1 = lib_data_api:get_value(Role, record_list_1, []),
            RecordList1N = case lists:keyfind(Id, 1, RecordList1) of
                               false -> lists:keystore(Id, 1, RecordList1, {Id, 1});
                               {_, Cnt} -> lists:keystore(Id, 1, RecordList1, {Id, Cnt + 1})
                           end,
            lib_data_api:put_value(Role, record_list_1, RecordList1N);
        false ->
            RecordList2 = lib_data_api:get_value(Role, record_list_2, []),
            RecordList2N = case lists:keyfind(Id, 1, RecordList2) of
                               false -> lists:keystore(Id, 1, RecordList2, {Id, 1});
                               {_, Cnt} -> lists:keystore(Id, 1, RecordList2, {Id, Cnt + 1})
                           end,
            lib_data_api:put_value(Role, record_list_2, RecordList2N)
    end.

%% @doc 数据转换
swich_to_data(ShopList) ->
    F = fun({Id, Type, Cost}) -> #'ShopInfo'{id = Id, type = Type, price = Cost} end,
    lists:map(F, ShopList).




