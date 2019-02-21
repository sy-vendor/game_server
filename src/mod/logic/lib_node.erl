%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 游戏节点
%%% @end
%%% Created : 13. 十二月 2018 11:24
%%%-------------------------------------------------------------------
-module(lib_node).
-author("suyang").

%% API
-export([stop/0]).

-include("common.hrl").


%% @doc 关闭节点
stop() ->
    % 将当前节点角色踢下线
    lib_role_api:stop_all(),
    % 关闭节点通用处理
    shutdown_public_handle(),
    ?INFO("server shutdown, node ~w ...", [node()]),
    ok.

%% 关服通用处理
shutdown_public_handle() ->
    ok.