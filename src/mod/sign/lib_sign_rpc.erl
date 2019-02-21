%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 签到协议处理
%%% @end
%%% Created : 19. 十二月 2018 11:02
%%%-------------------------------------------------------------------
-module(lib_sign_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("common.hrl").
-include("role.hrl").
-include("logic.hrl").


handle(11201, #'SignInfoReq'{}, Role) ->
    lib_sign_api:send_sign_info(Role);
handle(11203, #'SignRewardReq'{type = Type}, Role) ->
    lib_sign_api:sign_reward(Type, Role);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.