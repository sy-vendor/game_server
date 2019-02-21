%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 离线奖励处理
%%% @end
%%% Created : 04. 一月 2019 17:46
%%%-------------------------------------------------------------------
-module(lib_offline_api).
-author("suyang").

%% API
-export([init/3,
    offline_reward/2,
    offline_reward_info/1]).

-include("role.hrl").
-include("common.hrl").
-include("logic.hrl").

-define(TYPE_NORMAL, 1).    %% 普通领取
-define(TYPE_DOUBLE, 2).    %% 双倍领取

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 登陆初始化
init(_NowTime, _LastLogOutTime, Role) ->
    Time = calc_offline_time(Role),
    MaxType = lib_merge_api:get_max_type(),
    Asset = conf_building:get_config_offline(MaxType),
    AssetN = trunc(Asset * Time),
    RoleN = lib_role_asset:add_assets([{?TYPE_COIN, AssetN}], Role),
    {ok, RoleN}.

%% @doc 离线奖励信息请求
offline_reward_info(Role) ->
    Time = calc_offline_time(Role),
    MaxType = lib_merge_api:get_max_type(),
    Asset = conf_building:get_config_offline(MaxType),
    AssetN = trunc(Asset * Time),
    {ok, Bin} = lib_proto:pack(11009, #'OfflineInfoRes'{code = ?SUCCESS, coin = AssetN}),
    lib_send:send_to_role(Role, Bin),
    {ok, Role}.

%% @doc 离线奖励领取请求
offline_reward(Type, Role) ->
    Time = calc_offline_time(Role),
    MaxType = lib_merge_api:get_max_type(),
    Asset = conf_building:get_config_offline(MaxType),
    Args = lib_daily_api:get_special_info(Role, offline_reward_args),
    offline_reward(Type, Asset, Time, Role, Args).

offline_reward(_Type, _Asset, _Time, Role, ?SUCCESS) ->
    send_offline_reward_state(?FAIL, Role),
    {ok, Role};
offline_reward(_Type, _Asset, Time, Role, _Args) when Time =:= 0 ->
    send_offline_reward_state(?FAIL, Role),
    {ok, Role};
offline_reward(?TYPE_NORMAL, _Asset, _Time, Role, _Args) ->
    lib_daily_api:set_special_info(Role, offline_reward_args, ?SUCCESS),
    send_offline_reward_state(?SUCCESS, Role),
    {ok, Role};
offline_reward(?TYPE_DOUBLE, Asset, Time, Role, _Args) ->
    AssetN = trunc(Asset * Time),
    lib_daily_api:set_special_info(Role, offline_reward_args, ?SUCCESS),
    RoleN = lib_role_asset:add_assets([{?TYPE_COIN, AssetN}], Role),
    send_offline_reward_state(?SUCCESS, RoleN),
    {ok, RoleN};
offline_reward(_Type, _Asset, _Time, Role, _Args) ->
    send_offline_reward_state(?FAIL, Role),
    {ok, Role}.

%% @doc 离线奖励领取返回
send_offline_reward_state(Arg, Role) ->
    {ok, Bin} = lib_proto:pack(11007, #'OfflineRewardRes'{code = Arg}),
    lib_send:send_to_role(Role, Bin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 计算离线时间
calc_offline_time(Role) ->
    #role{last_logout_time = LastLogoutTime, last_login_time = LastLoginTime} = Role,
    case LastLogoutTime of
        0 -> 0;
        _ -> ?IF(LastLoginTime >= LastLogoutTime, LastLoginTime - LastLogoutTime, 0)
    end.