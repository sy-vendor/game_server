%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 游戏节点启动模版
%%% @end
%%% Created : 29. 9月 2019 19:30
%%%-------------------------------------------------------------------

-module(tpl_node_logic).
-include("common.hrl").

-export([
    start/0,
    stop/0
]).

%% =====================================================================
%% API
%% =====================================================================

%% @doc 启动游戏节点
start() ->
    ?ERROR_MSG("server startup, node ~w ...", [node()]),
    %% ========= 公共系统 =========
    ok = start_log(),                                     %% 日志系统
    ok = start_make_log(),                                %% 日志文件
    %% ========= 分界线 =========
    ok = init_public_handle(),                            %% 通用公共调用处理(放在活动管理进程后)
    ok.

%% @doc 关闭游戏节点
stop() ->
    % 关服通用处理
    shutdown_public_handle(),
    ?ERROR_MSG("server shutdown, node ~w ...", [node()]),
    timer:sleep(10000),
    ok.

%% =====================================================================
%% Internal Functions
%% =====================================================================

%% 公共数据处理
init_public_handle() ->
    ok.

%% 关服通用处理
shutdown_public_handle() ->
    ok.

%% 日志
start_log() ->
    {ok, _} = supervisor:start_child(sup,
        {svr_log, {svr_log, start_link, []},
            permanent, 10000, worker, [svr_log]}),
    ok.

%% 日志文件进程
start_make_log() ->
    {ok, _} = supervisor:start_child(sup,
        {svr_make_log, {svr_make_log, start_link, []},
            permanent, 10000, worker, [svr_make_log]}),
    ok.




