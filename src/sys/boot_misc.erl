%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 杂项(ERL应用启动相关)
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(boot_misc).
-author("sy").

%% API
-export([start_applications/1, stop_applications/1, swap_sup_child/1, start_sup_child/3]).

-include("common.hrl").

%% @spec start_applications(Apps) -> ok
%% Apps = list()
%% @doc 按次序启动app
start_applications(Apps) ->
  manage_applications(
    fun lists:foldl/3,
    fun application:start/1,
    fun application:stop/1,
    already_started,
    cannot_start_application,
    Apps
  ).

%% @spec stop_applications(Apps) -> ok
%% Apps = list()
%% @doc 按启动时相反的次序关闭app
stop_applications(Apps) ->
  manage_applications(
    fun lists:foldr/3,
    fun application:stop/1,
    fun application:start/1,
    not_started,
    cannot_stop_application,
    Apps
  ).

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
  F = fun(App, Acc) ->
    case Do(App) of
      ok -> [App | Acc];
      {error, {SkipError, _}} -> Acc;
      {error, Reason} ->
        lists:foreach(Undo, Acc),
        throw({error, {ErrorTag, App, Reason}})
    end
      end,
  Iterate(F, [], Apps),
  ok.

%% @spec start_sup_child(M, F, A) -> {ok, Pid} | {error, Reason}
%% {M, F, A} = mfa()
%% Pid = pid()
%% 启动supervisor的子进程
start_sup_child(M, F, A) ->
  ?INFO("正在启动[~w]", [M]),
  case erlang:apply(M, F, A) of
    {ok, Pid} when is_pid(Pid) ->
      ?INFO("完成启动[~w]", [M]),
      {ok, Pid};
    Err ->
      ?INFO("启动出错[~w]: ~w", [M, Err]),
      Err
  end.

%% 处理监控树列表
swap_sup_child(L) ->
  lists:map(fun({Id, {M, F, A}, StartType, StartCount, Type, Mod}) ->  {Id, {boot_misc, start_sup_child, [M, F, A]}, StartType, StartCount, Type, Mod};
    ({Id, {M, F, A}}) ->  {Id, {boot_misc, start_sup_child, [M, F, A]}, transient, 100000, worker, [Id]}
            end, L).