%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 加速系统
%%% @end
%%% Created : 04. 一月 2019 16:36
%%%-------------------------------------------------------------------
-module(lib_speed_api).
-author("suyang").

%% API
-export([speed_up/2]).

-export([loop_speed_up/1,
    speed_up_state/1]).

-include("logic.hrl").
-include("common.hrl").
-include("role.hrl").
-include("err_code.hrl").
-include("configure.hrl").
-include("merge.hrl").

-define(USE_TYPE_DIAMON, 1).
-define(USE_TYPE_ADD, 2).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 加速信息请求
%%send_speed_up_info(Role) ->
%%    SpeedTime = lib_data_api:get_value(Role, speed_up_time, 0),
%%    {ok, Bin} = lib_proto:pack(11702, #'SpeedUpStateRes'{end_time = SpeedTime}),
%%    lib_send:send_to_role(Role, Bin).

%% @doc 加速请求
speed_up(Type, Role) ->
    case catch check_speed_up(Type, Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11704, #'SpeedUpRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Cost, SpeedTime} ->
            % 扣除消耗
            RoleN = lib_role_asset:cost_assets(Cost, Role),
            % 加速时间奖励
            MaxType = util_dist:get(?DICT_MAX_TYPE_LOOP, 0),
            Value = conf_building:get_config_production(MaxType),
            CoinReward = trunc(SpeedTime * Value),
            RoleNew = lib_role_asset:add_assets([{?TYPE_COIN, CoinReward}], RoleN),
            ?IF(Type =:= ?USE_TYPE_DIAMON, lib_daily_api:increment(RoleNew, speed_cnt_time), lib_daily_api:increment(RoleNew, speed_cnt_time_1)),
            {ok, Bin} = lib_proto:pack(11704, #'SpeedUpRes'{code = ?SUCCESS, coin = CoinReward}),
            lib_send:send_to_role(RoleNew, Bin),
            {ok, RoleNew}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 加速检查
check_speed_up(Type, Role) ->
    % 配置表判断
    Conf = conf_common:get_speed_conf(),
    ConfAddTime = conf_common:get_speed_time(),
    ?IF(Conf =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    ?IF(ConfAddTime =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    [ConfCnt, Cost, ConfTime] = Conf,
    % 消耗检查
    ?IF(Role#role.diamond < Cost, erlang:throw({error, ?ERR_COMMON_COST_LIMIT}), ignore),
    % 次数判断
    case Type of
        ?USE_TYPE_DIAMON ->
            Cnt = lib_daily_api:get_count(Role, speed_cnt_time, 0),
            ?IF(Cnt > ConfCnt, erlang:throw({error, ?ERR_COMMON_CNT_LIMIT}), ignore);
        _ ->
            Cnt = lib_daily_api:get_count(Role, speed_cnt_time_1, 0),
            ?IF(Cnt > ConfCnt, erlang:throw({error, ?ERR_COMMON_CNT_LIMIT}), ignore)
    end,
    CostList = case Type of
                   ?USE_TYPE_ADD -> [];
                   ?USE_TYPE_DIAMON -> [{?TYPE_DIAMOND, Cost}];
                   _ -> erlang:throw({error, ?ERR_COMMON_SYS})
               end,
    TimeN = case Type of
                ?USE_TYPE_ADD -> ConfAddTime;
                ?USE_TYPE_DIAMON -> ConfTime;
                _ -> 0
            end,
    {true, CostList, TimeN}.

%% @doc 秒循环检查
loop_speed_up(RoleId) ->
    Now = util_time:unixtime(),
    SpeedTime = lib_data_api:get_value(RoleId, speed_up_time, 0),
    case SpeedTime =/= 0 andalso Now >= SpeedTime of
        true -> lib_data_api:put_value(RoleId, speed_up_time, 0);
        false -> ignore
    end.

%% @doc 加速状态获取
speed_up_state(Role) ->
    SpeedTime = lib_data_api:get_value(Role, speed_up_time, 0),
    case SpeedTime of
        0 -> false;
        _ -> true
    end.
