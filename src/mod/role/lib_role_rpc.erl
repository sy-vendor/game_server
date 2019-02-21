%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十二月 2018 13:22
%%%-------------------------------------------------------------------
-module(lib_role_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("common.hrl").
-include("conn.hrl").
-include("role.hrl").
-include("logic.hrl").

%% 玩家信息请求
handle(11001, #'RoleInfoReq'{}, Role) ->
    #role{account = Account, url = Url, role_name = RoleName, role_lv = RoleLv, diamond = Diamond, coin = Coin, sex = Sex, last_login_time = LastLoginTime} = Role,
    RoleInfo = #'RoleInfo'{tokenId = Account, headUrl = Url, name = RoleName, lv = RoleLv, diamond = Diamond, coin = Coin, gender = Sex, lastLogoutTime = LastLoginTime},
    Data = #'RoleInfoRes'{roleInfo = RoleInfo},
    {ok, Bin} = lib_proto:pack(11002, Data),
    lib_send:send_to_role(Role, Bin),
    {ok};
%% 离线奖励请求
handle(11006, #'OfflineRewardReq'{type = Type}, Role) ->
    lib_offline_api:offline_reward(Type, Role);
%% 离线奖励信息请求
handle(11008, #'OfflineInfoReq'{}, Role) ->
    lib_offline_api:offline_reward_info(Role);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?DEBUG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.








