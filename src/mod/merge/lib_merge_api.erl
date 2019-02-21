%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 合并
%%% @end
%%% Created : 18. 十二月 2018 15:05
%%%-------------------------------------------------------------------
-module(lib_merge_api).
-author("suyang").

%% API
-export([init/3, save/2, rand_new_object/1, merge_object/3, recovery_object/2, send_merge_info/1]).

-export([get_max_type/0, get_pos_list/0, switch_to_data/1, switch_to_data/2, check_role_asset/1]).

-include("merge.hrl").
-include("role.hrl").
-include("common.hrl").
-include("err_code.hrl").
-include("logic.hrl").
-include("order.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 登陆初始化
init(_NowTime, _LastLogOutTime, Role) ->
    Sql = io_lib:format(?SQL_ROLE_MERGE_FETCH, [Role#role.role_id]),
    case db:get_all(Sql) of
        {ok, [[Pos, MaxType, NextType, Worth]]} ->
            RoleMerge = #role_merge{pos = util_type:bitstring_to_term(Pos), max_type = MaxType, next_type = NextType, worth = Worth},
            put(?DICT_ROLE_MAP, RoleMerge),
            MaxTypeN = ?IF(MaxType =:= 0, 1, MaxType),
            put(?DICT_MAX_TYPE_LOOP, MaxTypeN);
        _ ->
            Arg = conf_building:get_grow_value1(1),
            {Init, _} = ?IF(Arg == undefined, {0, 0}, Arg),
            RoleMerge = #role_merge{pos = init_pos_list(), next_type = 1, worth = Init},
            put(?DICT_ROLE_MAP, RoleMerge),
            put(?DICT_MAX_TYPE_LOOP, 0)
    end.

init_pos_list() ->
    MaxPos = conf_common:get_max_pos(),
    init_pos_list(MaxPos, []).
init_pos_list(0, PosList) -> PosList;
init_pos_list(MaxPos, PosList) ->
    PosListN = [{MaxPos, 0} | PosList],
    init_pos_list(MaxPos - 1, PosListN).


%% @doc 下线保存
save(_NowTime, Role) ->
    RoleMerge = get(?DICT_ROLE_MAP),
    case RoleMerge of
        #role_merge{pos = Pos, max_type = MaxType, next_type = NextType, worth = Worth, sign = Sign} when Sign =:= true ->
            DbRoleMerge = [[Role#role.role_id, util_type:term_to_bitstring(Pos), MaxType, NextType, Worth]],
            util:insert_values(?SQL_ROLE_MERGE_REPLACE_P1, ?SQL_ROLE_MERGE_REPLACE_P2, DbRoleMerge),
            RoleMergeN = RoleMerge#role_merge{sign = false},
            put(?DICT_ROLE_MAP, RoleMergeN);
        _ -> ignore
    end.

%% @doc 随机生成
rand_new_object(Role) ->
    case catch check_rand_new(Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11102, #'ProduceNewRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, RoleMerge, Cost, NextType, Pos, _Worth, NewObject, PosId} ->
            % 扣除消耗
            Rate = lib_waiter_api:get_type_add(?WAITER_SKILL_TYPE_FOUR),
            CoinN = Cost - trunc(Cost * Rate),
            RoleN = lib_role_asset:cost_assets([{?TYPE_COIN, CoinN}], Role),
            RoleMergeN = RoleMerge#role_merge{pos = Pos, worth = Cost, sign = true},
            put(?DICT_ROLE_MAP, RoleMergeN),
            % 数据记录
            RecordList = lib_data_api:get_value(RoleN, record_list, []),
            {_, Val} = ?IF(lists:keyfind(NewObject, 1, RecordList) =:= false, {NewObject, 0}, lists:keyfind(NewObject, 1, RecordList)),
            RecordListN = lists:keystore(NewObject, 1, RecordList, {NewObject, Val + 1}),
            lib_data_api:put_value(RoleN#role.role_id, record_list, RecordListN),
            PosData = switch_to_data(PosId, NewObject),
            {ok, Bin} = lib_proto:pack(11102, #'ProduceNewRes'{posInfo = PosData, nextType = NextType, nextPrice = Cost}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 合并与交换
merge_object(PosOne, PosTwo, Role) ->
    case PosOne =< conf_common:get_max_pos() andalso PosTwo =< conf_common:get_max_pos() andalso PosTwo =/= PosOne andalso PosOne =/= 0 andalso PosTwo =/= 0 of
        false ->
            {ok, Bin} = lib_proto:pack(11104, #'MergeExchangeRes'{code = ?FAIL}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        true ->
            RoleMerge = get(?DICT_ROLE_MAP),
            #role_merge{pos = Pos, max_type = MaxType} = RoleMerge,
            {PosN, MaxTypeN} = get_new_pos(Pos, PosOne, PosTwo, MaxType),
            NextType = ?IF(MaxTypeN =< 4, 1, MaxTypeN - 4),
            Cost = get_object_price(Role, NextType),
            RoleMergeN = RoleMerge#role_merge{max_type = MaxTypeN, pos = PosN, next_type = NextType, worth = Cost, sign = true},
            put(?DICT_ROLE_MAP, RoleMergeN),
            % 判断外卖系统是否开启
            OrderList = get(?DICT_ROLE_ORDER),
            ?IF(MaxTypeN =:= conf_common:get_order_lv() andalso OrderList =:= undefined, put(?DICT_ROLE_ORDER, []), ignore),
            PosData = switch_to_data(PosN),
            {ok, Bin} = lib_proto:pack(11104, #'MergeExchangeRes'{posInfo = PosData, nextType = NextType, nextPrice = Cost}),
            lib_send:send_to_role(Role, Bin),
            {ok, Role}
    end.

%% @doc 回收
recovery_object(Pos, Role) ->
    case catch check_recovery_object(Pos) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11106, #'RecoveryObjectRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Val, RoleMerge, PosList} ->
            RoleN = Role#role{coin = Role#role.coin + Val},
            % 数据处理
            PosListN = lists:keystore(Pos, 1, PosList, {Pos, 0}),
            RoleMergeN = RoleMerge#role_merge{pos = PosListN, sign = true},
            put(?DICT_ROLE_MAP, RoleMergeN),
            % 协议处理
            PosData = switch_to_data(PosListN),
            {ok, Bin} = lib_proto:pack(11106, #'RecoveryObjectRes'{posInfo = PosData}),
            lib_send:send_to_role(Role, Bin),
            {ok, RoleN}
    end.

%% @doc 推送合并个人信息
send_merge_info(Role) ->
    RoleMerge = get(?DICT_ROLE_MAP),
    RoleMergeN = ?IF(is_record(RoleMerge, role_merge), RoleMerge, #role_merge{}),
    #role_merge{pos = Pos, next_type = NextType, worth = Worth} = RoleMergeN,
    % 协议处理
    PosData = switch_to_data(Pos),
    {ok, Bin} = lib_proto:pack(11109, #'PosInfoRes'{posInfo = PosData, nextPrice = Worth, nextType = NextType}),
    lib_send:send_to_role(Role, Bin),
    {ok}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 随机生成检查
check_rand_new(Role) ->
    RoleMerge = get(?DICT_ROLE_MAP),
    ?IF(is_record(RoleMerge, role_merge), ignore, erlang:throw({error, ?ERR_COMMON_SYS})),
    #role_merge{max_type = MaxType, next_type = NextType, pos = Pos, worth = Worth} = RoleMerge,
    NewObject = ?IF(NextType =:= 0, 1, NextType),
    RecordList = lib_data_api:get_value(Role, record_list, []),
    {_, Val} = ?IF(lists:keyfind(NewObject, 1, RecordList) =:= false, {NewObject, 1}, lists:keyfind(NewObject, 1, RecordList)),
    Args = conf_building:get_grow_value1(NewObject),
    ?IF(Args =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    {InitCost, GrowVal} = Args,
    Cost = trunc(InitCost * math:pow(GrowVal, Val - 1)),
    #role{coin = Coin} = Role,
    % 判断消耗是否足够
    ?IF(Coin < Cost, erlang:throw({error, ?ERR_COMMON_COST_LIMIT}), ignore),
    NextTypeN = ?IF(MaxType =< 4, 1, MaxType - 4),
    % 判断地图是否填满
    ?IF(lists:keyfind(0, 2, Pos) =:= false, erlang:throw({error, ?ERR_COMMON_MAP_LIMIT}), ignore),
    {PosId, 0} = lists:keyfind(0, 2, Pos),
    PosN = lists:keystore(PosId, 1, Pos, {PosId, NewObject}),
    WorthN = Worth + Cost,
    {true, RoleMerge, Cost, NextTypeN, PosN, WorthN, NewObject, PosId}.

%% @doc 获取新的Pos
get_new_pos(Pos, PosOne, PosTwo, MaxType) ->
    case check_merge_object(Pos, PosOne, PosTwo) of
        {diff, 0, _} -> {Pos, MaxType};
        {diff, PosValOne, PosValTwo} ->
            PosN = lists:keystore(PosOne, 1, Pos, {PosOne, PosValTwo}),
            {lists:keystore(PosTwo, 1, PosN, {PosTwo, PosValOne}), MaxType};
        {same, PosvalTwo} ->
            PosN = lists:keystore(PosOne, 1, Pos, {PosOne, 0}),
            MaxTypeN = ?IF(PosvalTwo + 1 > MaxType, PosvalTwo + 1, MaxType),
            {lists:keystore(PosTwo, 1, PosN, {PosTwo, PosvalTwo + 1}), MaxTypeN}
    end.

check_merge_object(Pos, PosOne, PosTwo) ->
    {PosOne, PosValOne} = ?IF(lists:keyfind(PosOne, 1, Pos) =:= false, {PosOne, 0}, lists:keyfind(PosOne, 1, Pos)),
    {PosTwo, PosValTwo} = ?IF(lists:keyfind(PosTwo, 1, Pos) =:= false, {PosTwo, 0}, lists:keyfind(PosTwo, 1, Pos)),
    ?IF(PosValOne =:= PosValTwo, {same, PosValTwo}, {diff, PosValOne, PosValTwo}).

%% @doc 回收检查
check_recovery_object(Pos) ->
    RoleMerge = get(?DICT_ROLE_MAP),
    ?IF(is_record(RoleMerge, role_merge), ignore, erlang:throw({error, ?ERR_COMMON_SYS})),
    #role_merge{pos = PosList} = RoleMerge,
    % 检查该点是否有类型
    ?IF(lists:keyfind(Pos, 1, PosList) =:= false, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    {Pos, Type} = lists:keyfind(Pos, 1, PosList),
    Val = conf_building:get_recovery_config(Type),
    ?IF(Val =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    {true, Val, RoleMerge, PosList}.


%% @doc 获取MaxType
get_max_type() ->
    RoleMerge = get(?DICT_ROLE_MAP),
    ?IF(is_record(RoleMerge, role_merge), RoleMerge#role_merge.max_type, 0).

%% @doc 获取PosList
get_pos_list() ->
    RoleMerge = get(?DICT_ROLE_MAP),
    ?IF(is_record(RoleMerge, role_merge), RoleMerge#role_merge.pos, []).

%% @doc 转换协议数据
switch_to_data(Pos) ->
    F = fun({PosId, PosType}) -> #'PosInfo'{pos = PosId, type = PosType} end,
    lists:map(F, Pos).
switch_to_data(PosId, NewObject) ->
    #'PosInfo'{pos = PosId, type = NewObject}.

%% @doc 秒循环检查
check_role_asset(Role) ->
    MaxType = util_dist:get(?DICT_MAX_TYPE_LOOP, 0),
    Value = conf_building:get_config_production(MaxType),
    update_role_asset(Value, Role).

%% @doc 更新State
update_role_asset(0, Role) -> Role;
update_role_asset(Coin, Role) ->
    Rate = lib_waiter_api:get_type_add(?WAITER_SKILL_TYPE_TWO),
    CoinN = trunc(Coin + trunc(Coin * Rate)),
    lib_role_asset:add_assets([{?TYPE_COIN, CoinN}], Role).

%% @doc 获取价格
get_object_price(Role, NextType) ->
    RecordList = lib_data_api:get_value(Role, record_list, []),
    {_, Val} = ?IF(lists:keyfind(NextType, 1, RecordList) =:= false, {NextType, 1}, lists:keyfind(NextType, 1, RecordList)),
    {InitCost, GrowVal} = conf_building:get_grow_value1(NextType),
    trunc(InitCost * math:pow(GrowVal, Val - 1)).

