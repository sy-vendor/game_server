%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 服务员
%%% @end
%%% Created : 03. 一月 2019 14:26
%%%-------------------------------------------------------------------
-module(lib_waiter_api).
-author("suyang").

%% API
-export([init/3,
    save/2,
    send_waiter_info/1,
    unlock_waiter/2,
    intimate_waiter/2,
    get_type_add/1]).

-include("common.hrl").
-include("role.hrl").
-include("waiter.hrl").
-include("logic.hrl").
-include("err_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 登陆初始化
init(_NowTime, _LastLogOutTime, Role) ->
    Sql = io_lib:format(?SQL_ROLE_WAITER_FETCH, [Role#role.role_id]),
    F = fun([Id, Type, Intimate, SkillLv]) ->
        #waiter{id = Id, type = Type, intimate = Intimate, skill_lv = SkillLv} end,
    case db:get_all(Sql) of
        {ok, List} ->
            OrderList = lists:map(F, List),
            put(?DICT_WAITER_LIST, OrderList);
        _ -> ignore
    end.

%% @doc 下线保存
save(_NowTime, Role) ->
    WaiterList = get(?DICT_WAITER_LIST),
    case WaiterList of
        _ when is_list(WaiterList) andalso WaiterList =/= [] ->
            F = fun(RoleWaiter, {WaiterListAcc, DbWaiterListAcc}) ->
                #waiter{id = Id, type = Type, intimate = Intimate, skill_lv = SkillLv, sign = Sign} = RoleWaiter,
                case Sign of
                    false -> {[RoleWaiter | WaiterListAcc], DbWaiterListAcc};
                    true ->
                        DbWaiter = [Role#role.role_id, Id, Type, Intimate, SkillLv],
                        {[RoleWaiter#waiter{sign = false} | WaiterListAcc], [DbWaiter | DbWaiterListAcc]}
                end
                end,
            {WaiterListAccN, DbWaiterListAccN} = lists:foldl(F, {[], []}, WaiterList),
            put(?DICT_WAITER_LIST, WaiterListAccN),
            util:insert_values(?SQL_ROLE_WAITER_REPLACE_P1, ?SQL_ROLE_WAITER_REPLACE_P2, DbWaiterListAccN);
        _ -> ignore
    end.

%% @doc 推送服务员信息
send_waiter_info(Role) ->
    WaiterList = get(?DICT_WAITER_LIST),
    case WaiterList of
        _ when is_list(WaiterList) andalso WaiterList =/= [] ->
            WaiterData = switch_to_data(WaiterList),
            {ok, Bin} = lib_proto:pack(11602, #'WaiterInfoRes'{waiterInfo = WaiterData}),
            lib_send:send_to_role(Role, Bin);
        _ -> ignore
    end.
send_waiter_info(Waiter, Role) ->
    #waiter{id = Id, type = Type, intimate = Intimate, skill_lv = SkillLv} = Waiter,
    WaiterData = #'WaiterInfo'{id = Id, type = Type, intimate = Intimate, skillLv = SkillLv},
    {ok, Bin} = lib_proto:pack(11606, #'WaiterIntimateRes'{waiterInfo = WaiterData}),
    lib_send:send_to_role(Role, Bin).

%% @doc 服务员解锁
unlock_waiter(Type, Role) ->
    case catch check_unlock_waiter(Type, Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11604, #'WaiterUnlockRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, OnlyId, WaiterList, Cost} ->
            % 扣除消耗
            RoleN = lib_role_asset:cost_assets(Cost, Role),
            Waiter = #waiter{id = OnlyId, type = Type, skill_lv = 1, sign = true},
            WaiterListN = [Waiter | WaiterList],
            put(?DICT_WAITER_LIST, WaiterListN),
            {ok, Bin} = lib_proto:pack(11604, #'WaiterUnlockRes'{code = ?SUCCESS}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 打赏
intimate_waiter(Id, Role) ->
    case catch check_intimate_waiter(Id, Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11606, #'WaiterIntimateRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, WaiterList, Cost, Intimate, Waiter, IntimacyLimit} ->
            % 扣除消耗
            RoleN = lib_role_asset:cost_assets(Cost, Role),
            WaiterN = Waiter#waiter{intimate = Waiter#waiter.intimate + Intimate, sign = true},
            WaiterNew =
            case WaiterN#waiter.intimate >= IntimacyLimit of
                true -> WaiterN#waiter{intimate = WaiterN#waiter.intimate - IntimacyLimit, skill_lv = WaiterN#waiter.skill_lv + 1, sign = true};
                false -> WaiterN
            end,
            WaiterListN = lists:keystore(Id, #waiter.id, WaiterList, WaiterNew),
            put(?DICT_WAITER_LIST, WaiterListN),
            send_waiter_info(WaiterNew, RoleN),
            {ok, RoleN}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 转换协议数据
switch_to_data(WaiterList) ->
    F = fun(#waiter{id = Id, type = Type, intimate = Intimate, skill_lv = SkillLv}) ->
        #'WaiterInfo'{id = Id, type = Type, intimate = Intimate, skillLv = SkillLv} end,
    lists:map(F, WaiterList).

%% @doc 服务员解锁检查
check_unlock_waiter(Type, Role) ->
    WaiterList = get(?DICT_WAITER_LIST),
    WaiterListN = ?IF(WaiterList =:= undefined, [], WaiterList),
    OnlyId = ?IF(length(WaiterListN) =:= 0, 1, length(WaiterListN) + 1),
    % 检查上一级Type服务员是否解锁
    case Type of
        1 -> ignore;
        _ ->
            ?IF(lists:keyfind(Type - 1, #waiter.type, WaiterListN) =:= false, erlang:throw({error, ?ERR_COMMON_WAITER_UNLOCK}), ignore)
    end,
    % 检查消耗是否足够
    Cost = conf_waiter:get_waiter_unlock(Type),
    ?IF(Cost =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    IsEnough = lib_role_asset:is_asset_enough([{?TYPE_COIN, Cost}], Role),
    ?IF(IsEnough =:= false, erlang:throw({error, ?ERR_COMMON_COST_LIMIT}), ignore),
    {true, OnlyId, WaiterListN, [{?TYPE_COIN, Cost}]}.

%% @doc 打赏检查
check_intimate_waiter(Id, Role) ->
    WaiterList = get(?DICT_WAITER_LIST),
    WaiterListN = ?IF(WaiterList =:= undefined, [], WaiterList),
    ?IF(WaiterListN =:= [], erlang:throw({error, ?ERR_COMMON_WAITER_NOEXIST}), ignore),
    ?IF(lists:keyfind(Id, #waiter.id, WaiterListN) =:= false, erlang:throw({error, ?ERR_COMMON_WAITER_NOEXIST}), ignore),
    Waiter = lists:keyfind(Id, #waiter.id, WaiterListN),
    ConfIntimacy = conf_skill:get_skill_intimacy(Waiter#waiter.type, Waiter#waiter.skill_lv),
    ?IF(ConfIntimacy =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    [IntimacyLimit, IntimacyOnce, RewardConsume] = ConfIntimacy,
    IsEnough = lib_role_asset:is_asset_enough([{?TYPE_COIN, RewardConsume}], Role),
    ?IF(IsEnough =:= false, erlang:throw({error, ?ERR_COMMON_COST_LIMIT}), ignore),
    {true, WaiterListN, [{?TYPE_COIN, RewardConsume}], IntimacyOnce, Waiter, IntimacyLimit}.

%% @doc 获取类型加成属性
get_type_add(Type) ->
    WaiterList = util_dist:get(?DICT_WAITER_LIST, []),
    WaiterListN = lists:filter(fun(#waiter{type = WaiterType}) -> WaiterType =:= Type end, WaiterList),
    lists:foldl(fun(Waiter, Sum) -> sum_type(Waiter, Sum) end, 0, WaiterListN).

sum_type(#waiter{type = Type, skill_lv = SkillLv}, Sum) ->
    Rate = conf_skill:get_skill_add(Type, SkillLv),
    Rate + Sum.
