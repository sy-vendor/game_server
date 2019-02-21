%%%-------------------------------------------------------------------
%%% @author Su Yang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 协议处理映射表
%%% @end
%%% Created : 07. 十二月 2018 21:29
%%%-------------------------------------------------------------------
-module(mapping).
-author("Su Yang").

%% API
-export([module/2, get/1]).

%% @spec module(Type, Cmd) -> {ok, Type, Caller, Proto, ModName} | {ok, Proto, ModName} | {error, Reason}
%% Type = atom()
%% Cmd = int()
%% NeedAuth = bool()
%% Caller = connector | object
%% Proto = atom()
%% ModName = atom()
%% @doc 模块映射信息
module(Type, Cmd) -> code(Type, trunc(Cmd / 100)).

%% @doc
code(game_server, 100) -> {ok, false, connector, logic, lib_account_rpc};
code(game_server, 110) -> {ok, true, object, logic, lib_role_rpc};
code(game_server, 111) -> {ok, true, object, logic, lib_merge_rpc};
code(game_server, 112) -> {ok, true, object, logic, lib_sign_rpc};
code(game_server, 113) -> {ok, true, object, logic, lib_turntable_rpc};
code(game_server, 114) -> {ok, true, object, logic, lib_invite_rpc};
code(game_server, 115) -> {ok, true, object, logic, lib_order_rpc};
code(game_server, 116) -> {ok, true, object, logic, lib_waiter_rpc};
code(game_server, 117) -> {ok, true, object, logic, lib_speed_rpc};
code(game_server, 118) -> {ok, false, connector, logic, lib_account_rpc};

code(Type, Code) -> {error, {Type, Code}}.

%% @spec get(Cmd) -> MsgName | undefined
%% Cmd = int()
%% MsgName = atom()
%% @doc 注册名映射
get(10001) -> 'HeartReq';
get(10003) -> 'LoginReq';
get(11001) -> 'RoleInfoReq';
get(11004) -> 'BoxRewardReq';
get(11006) -> 'OfflineRewardReq';
get(11008) -> 'OfflineInfoReq';
get(11101) -> 'ProduceNewReq';
get(11103) -> 'MergeExchangeReq';
get(11105) -> 'RecoveryObjectReq';
get(11108) -> 'PosInfoReq';
get(11110) -> 'BuyObjectReq';
get(11112) -> 'ShopInfoReq';
get(11201) -> 'SignInfoReq';
get(11203) -> 'SignRewardReq';
get(11301) -> 'TurntableInfoReq';
get(11303) -> 'PlayTurntableReq';
get(11305) -> 'AdRewardReq';
get(11307) -> 'AdAddCntStartReq';
get(11308) -> 'AdAddCntEndReq';
get(11401) -> 'InviteInfoReq';
get(11403) -> 'InviteRewardReq';
get(11501) -> 'OrderInfoReq';
get(11503) -> 'OrderAcceptReq';
get(11504) -> 'OrderDeliveryReq';
get(11505) -> 'OrderRewardReq';
get(11601) -> 'WaiterInfoReq';
get(11603) -> 'WaiterUnlockReq';
get(11605) -> 'WaiterIntimateReq';
get(11701) -> 'SpeedUpStateReq';
get(11703) -> 'SpeedUpReq';
get(11801) -> 'WechatLoginReq';
get(_Cmd) -> undefined.
