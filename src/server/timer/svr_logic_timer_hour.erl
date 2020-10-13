%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 游戏服定时器(按时计算)
%%% @end
%%% Created : 29. 9月 2019 11:41
%%%-------------------------------------------------------------------

-module(svr_logic_timer_hour).
-behaviour(gen_statem).
-include("common.hrl").

-export([
    p/0,
    call/1,
    cast/1,
    start_link/0
]).

-export([
    init/1,
    callback_mode/0,
    handle_event/4, 
    terminate/3,
    code_change/4
]).

%% 休眠间隔(按时计算)
-define(TIMEOUT, 3600).

%% =============================================================================
%% API
%% =============================================================================

%% @doc 启动进程
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%% @doc 进程ID
p() ->
    dist:whereis_name(local, ?MODULE).

%% @doc call函数
call(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_statem:call(Pid, Msg);
        _ ->
            {error, err_common_service_not_start}
    end.

%% @doc cast函数
cast(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_statem:cast(Pid, Msg);
        _ ->
            ignore
    end.

%% =============================================================================
%% Gen_statem Callbacks
%% =============================================================================

init([]) ->
    NowTime = time:unixtime(),
    Rem = NowTime rem ?TIMEOUT,
    Timeout = ?iif(Rem =:= 0, 0, ?TIMEOUT - Rem),
    Action = {timeout, Timeout * 1000, {wait_time_out}},
    {ok, working, [], Action}.

callback_mode() -> 
    handle_event_function.

handle_event(Event, Msg, StateName, State) ->
    try
        do_handle_event(Event, Msg, StateName, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Msg, State, Reason, erlang:get_stacktrace()]),
            {keep_state_and_data}
    end.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% 处理定时业务
do_handle_event(timeout, {wait_time_out}, working, State) ->
    case catch do_handle() of
        ok ->
            ?ERROR_MSG("logic_timer_hour normal working...~n");
        Err ->
            ?ERROR_MSG("logic_timer_hour err: ~p~n", [Err])
    end,
    Action = {timeout, ?TIMEOUT * 1000, {wait_time_out}},
    {next_state, working, State, Action};
%% 默认匹配
do_handle_event(_Event, _Msg, _StateName, _State) ->
    keep_state_and_data.

%% 定时任务操作
%% 必须spawn一个进程处理，防止出错导致定时器停止
do_handle() ->
    ok.