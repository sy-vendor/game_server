%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十二月 2018 17:02
%%%-------------------------------------------------------------------
-module(lib_turntable_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("common.hrl").
-include("role.hrl").
-include("logic.hrl").

%% 次数请求
handle(11301, #'TurntableInfoReq'{}, Role) ->
    lib_turntable_api:send_turntable_info(Role),
    {ok};
%% 转盘请求
handle(11303, #'PlayTurntableReq'{}, Role) ->
    lib_turntable_api:play_turntable(Role);
%% 看广告领取请求
handle(11305, #'AdRewardReq'{}, Role) ->
    lib_turntable_api:watch_ad_reward(Role);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.