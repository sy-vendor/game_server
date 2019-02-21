%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十二月 2018 13:21
%%%-------------------------------------------------------------------
-module(boot_misc).
-author("suyang").

%% API
-export([start_applications/1,
    stop_applications/1,
    start_sup_child/3,
    swap_sup_child/1]).

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
    case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        Err -> ?ERROR_MSG("start up error Model[~w]: ~w", [M, Err])
    end.

%% 处理监控树列表
swap_sup_child(L) ->
    lists:map(fun({Id, {M, F, A}, StartType, StartCount, Type, Mod}) ->
        {Id, {boot_misc, start_sup_child, [M, F, A]}, StartType, StartCount, Type, Mod};
        ({Id, {M, F, A}}) -> {Id, {boot_misc, start_sup_child, [M, F, A]}, transient, 100000, worker, [Id]} end, L).