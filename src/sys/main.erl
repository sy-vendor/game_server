%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 外部入口
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(main).
-author("sy").

%% API
-export([start/0, stop_from_shell/0, stop_from_shell_backup/0, stop/1, stop_cross/0, stop_cross_from_shell/0, get_all_app/0]).

-define(APPS, [sasl, game]).
-include("common.hrl").

%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
%% @doc 启动系统
start() ->
  boot_misc:start_applications(?APPS).

%% @spec stop_from_shell() -> ok
%% @doc 从shell执行关机操作
stop_from_shell() ->
  [M] = init:get_plain_arguments(),
  Master = list_to_atom(M),
  ?INFO("将要关闭服务器: ~w", [Master]),
  Reply = rpc:call(Master, main, stop, [normal]), %% 最后关闭主节点
  ?INFO("stop result: ~w", [Reply]),
  erlang:halt().

%% @spec stop_from_shell_backup() -> ok
stop_from_shell_backup() ->
  [M] = init:get_plain_arguments(),
  Master = list_to_atom(M),
  ?INFO("将要关闭服务器: ~w", [Master]),
  Reply = rpc:call(Master, main, stop, [backup]), %% 最后关闭主节点
  ?INFO("stop result: ~w", [Reply]),
  erlang:halt().

%% @spec stop() -> ok
%% @doc 关闭当前节点
stop(_Type) ->
  ?INFO("关闭节点: ~w", [node()]),
  sys_listener:stop(), %% 停止接受新的连接
  boot_misc:stop_applications(?APPS),
  application:stop(mnesia),
  erlang:halt().

%% 中央服关机
%% @spec stop_cross_from_shell() -> ok
%% @doc 从shell执行中央服关机操作
stop_cross_from_shell() ->
  [M] = init:get_plain_arguments(),
  Master = list_to_atom(M),
  Reply = rpc:call(Master, main, stop_cross, []), %% 最后关闭主节点
  ?INFO("stop result: ~w", [Reply]),
  erlang:halt().

stop_cross() ->
  ?INFO("关闭节点: ~w", [node()]),
  boot_misc:stop_applications(?APPS),
  application:stop(mnesia),
  erlang:halt().

%% 获取启动的app
get_all_app() ->
  ?APPS.
