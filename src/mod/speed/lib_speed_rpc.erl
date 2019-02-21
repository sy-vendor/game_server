%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 加速协议处理
%%% @end
%%% Created : 04. 一月 2019 17:15
%%%-------------------------------------------------------------------
-module(lib_speed_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("role.hrl").
-include("common.hrl").
-include("logic.hrl").

%% 加速状态请求
%%handle(11701, #'SpeedUpStateReq'{}, Role) ->
%%    lib_speed_api:send_speed_up_info(Role),
%%    {ok};
%% 加速请求
handle(11703, #'SpeedUpReq'{type = Type}, Role) ->
    lib_speed_api:speed_up(Type, Role);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.
