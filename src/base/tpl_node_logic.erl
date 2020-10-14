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
    ok = start_global_data(),                             %% 全局数据
    ok = start_timer_logic(),                             %% 全局定时器
    ok = start_log(),                                     %% 日志系统
    ok = start_make_log(),                                %% 日志文件
    ok = start_filter(),                                  %% 过滤系统
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
    ?TRY_CATCH(svr_logic:save()),
    ?TRY_CATCH(svr_make_log:stop()),
    ok.

%% 开启全局键值对
start_global_data() ->
    {ok, _} = supervisor:start_child(sup,
        {svr_global_data, {svr_global_data, start_link, []},
            permanent, 10000, worker, [svr_global_data]}),
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

%% 开启定时器监控树
start_timer_logic() ->
    {ok, _} = supervisor:start_child(sup,
        {sup_logic_timer, {sup_logic_timer, start_link, []},
            permanent, infinity, supervisor, [sup_logic_timer]}),
    ok.

%% 过滤系统
start_filter() ->
    {ok, _} = supervisor:start_child(sup,
        {svr_filter, {svr_filter, start_link, []},
            permanent, 10000, worker, [svr_filter]}),
    ok.




