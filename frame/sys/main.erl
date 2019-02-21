%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十二月 2018 13:04
%%%-------------------------------------------------------------------
-module(main).
-author("suyang").

%% API
-export([start/0,
    stop/1,
    get_all_app/0,
    stop_from_shell/0,
    stop_center_from_shell/0,
    stop_center/1]).

-define(APPS, [sasl, game]).
-include("common.hrl").

%% @doc 启动系统
start() ->
    boot_misc:start_applications(?APPS).

%% @spec stop() -> ok
%% @doc 关闭当前节点
stop(_Type) ->
    ?INFO("close node: ~w", [node()]),
    %% 停止接受新的连接
    sys_listener:stop(),
    lib_node:stop(),
    boot_misc:stop_applications(?APPS),
    erlang:halt().

%% @spec stop_from_shell() -> ok
%% @doc 从shell执行关机操作
stop_from_shell() ->
    [M] = init:get_plain_arguments(),
    Master = list_to_atom(M),
    ?INFO("server is close....: ~w", [Master]),
    Reply = rpc:call(Master, main, stop, [normal]), %% 最后关闭主节点
    ?INFO("stop result: ~w", [Reply]),
    erlang:halt().

%% @spec stop_center_from_shell() -> ok
%% @doc 从shell执行关机操作
stop_center_from_shell() ->
    [M] = init:get_plain_arguments(),
    Master = list_to_atom(M),
    ?INFO("server is close....: ~w", [Master]),
    Reply = rpc:call(Master, main, stop_center, [normal]), %% 最后关闭主节点
    ?INFO("stop result: ~w", [Reply]),
    erlang:halt().

%% @spec stop() -> ok
%% @doc 关闭中央节点
stop_center(_Type) ->
    ?INFO("close node: ~w", [node()]),
    %% 停止接受新的连接
    sys_listener:stop(),
    lib_center_node:stop(),
    boot_misc:stop_applications(?APPS),
    erlang:halt().

%% 获取启动的app
get_all_app() ->
    ?APPS.
