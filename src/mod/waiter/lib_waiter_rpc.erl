%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 服务员协议处理
%%% @end
%%% Created : 03. 一月 2019 18:08
%%%-------------------------------------------------------------------
-module(lib_waiter_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("logic.hrl").
-include("err_code.hrl").
-include("common.hrl").
-include("role.hrl").

%% @doc 服务员信息请求
handle(11601, #'WaiterInfoReq'{}, Role) ->
    lib_waiter_api:send_waiter_info(Role);
%% @doc 服务员解锁请求
handle(11603, #'WaiterUnlockReq'{type = Type}, Role) ->
    lib_waiter_api:unlock_waiter(Type, Role);
%% @doc 服务员打赏
handle(11605, #'WaiterIntimateReq'{id = Id}, Role) ->
    lib_waiter_api:intimate_waiter(Id, Role);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.
