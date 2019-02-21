%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 中央节点
%%% @end
%%% Created : 26. 十二月 2018 17:33
%%%-------------------------------------------------------------------
-module(lib_center_node).
-author("suyang").

%% API
-export([stop/0]).

-include("common.hrl").


%% @doc 关闭节点
stop() ->
    % 关闭节点通用处理
    shutdown_public_handle(),
    ?INFO("center  server shutdown, node ~w ...", [node()]),
    ok.

%% 关服通用处理
shutdown_public_handle() ->
    ok.