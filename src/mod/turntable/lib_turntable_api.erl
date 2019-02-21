%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 转盘
%%% @end
%%% Created : 19. 十二月 2018 18:14
%%%-------------------------------------------------------------------
-module(lib_turntable_api).
-author("suyang").

%% API
-export([init/3, play_turntable/1, watch_ad_reward/1, daily_reset/1, send_turntable_info/1]).

-include("common.hrl").
-include("role.hrl").
-include("err_code.hrl").
-include("logic.hrl").

-define(TYPE_MONEY_ASSET, 1).
-define(TYPE_MONEY_DIAMOND, 2).

-define(TYPE_ACT_NORMAL, 0).
-define(TYPE_ACT_AD, 1).

-define(TURNTABLE_CNT_DAILY, 5).
-define(EVER_AD_TIME, 15).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 登陆初始化
init(NowTime, LastLogOutTime, Role) ->
    case util_time:is_same_day(NowTime, LastLogOutTime) of
        true -> ignore;
        false -> daily_reset(Role#role.role_id)
    end,
    ok.

daily_reset(RoleId) ->
    lib_daily_api:set_count(RoleId, turntable_cnt, ?TURNTABLE_CNT_DAILY).

%% @doc 转盘
play_turntable(Role) ->
    case catch check_turntable(Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11304, #'PlayTurntableRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Pos, Reward, MoneynType, ActType} ->
            lib_daily_api:set_special_info(Role#role.role_id, watch_ad_reward, []),
            % 改变数据
            lib_daily_api:decrement(Role#role.role_id, turntable_cnt),
            % 获取奖励
            RoleN = case ActType of
                        ?TYPE_ACT_NORMAL ->
                            send_turntable_reward(Reward, MoneynType, Role);
                        ?TYPE_ACT_AD ->
                            lib_daily_api:set_special_info(Role#role.role_id, watch_ad_reward, [MoneynType, Reward, util_time:unixtime()]),
                            Role;
                        _ -> Role
                    end,
            % 发送协议
            TurntableCnt = lib_daily_api:get_count(Role, turntable_cnt, 0),
            {ok, Bin} = lib_proto:pack(11304, #'PlayTurntableRes'{pos = Pos, turntableCnt = TurntableCnt}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 观看广告领取
watch_ad_reward(Role) ->
    case catch check_ad_reward(Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11306, #'AdRewardRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true} ->
            % 发送奖励
            RoleN = lib_role_asset:add_assets([{?TYPE_DIAMOND, 20}], Role),
            % 更改数据
            lib_daily_api:increment(Role, ad_reward_cnt),
            {ok, Bin} = lib_proto:pack(11306, #'AdRewardRes'{code = ?SUCCESS}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 获取转盘次数
send_turntable_info(Role) ->
    TurntableCnt = lib_daily_api:get_count(Role, turntable_cnt, 0),
    {ok, Bin} = lib_proto:pack(11302, #'TurntableInfoRes'{turntableCnt = TurntableCnt}),
    lib_send:send_to_role(Role, Bin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 转盘检查
check_turntable(Role) ->
    TurntableCnt = lib_daily_api:get_count(Role, turntable_cnt, 0),
    % 判断是否还是次数
    ?IF(TurntableCnt =< 0, erlang:throw({error, ?ERR_COMMON_CNT_LIMIT}), ignore),
    TurntableList = get_turntable_list(),
    {Pos, Reward, _, MoneynType, ActType} = util_math:rand_list_one(TurntableList, 3),
    {true, Pos, Reward, MoneynType, ActType}.

send_turntable_reward(Reward, MoneynType, Role) when MoneynType =:= ?TYPE_MONEY_ASSET ->
    lib_role_asset:add_assets([{?TYPE_COIN, Reward}], Role);
send_turntable_reward(Reward, MoneynType, Role) when MoneynType =:= ?TYPE_MONEY_DIAMOND ->
    lib_role_asset:add_assets([{?TYPE_DIAMOND, Reward}], Role);
send_turntable_reward(_Reward, _MoneynType, Role) ->
    Role.

get_turntable_list() -> [].

%% @doc 广告奖励检查
check_ad_reward(Role) ->
    Cnt = lib_daily_api:get_count(Role, ad_reward_cnt, 0),
    % 判断是否还是次数
    ?IF(Cnt >= 3, erlang:throw({error, ?ERR_COMMON_CNT_LIMIT}), ignore),
    {true}.