%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 9月 2019 11:03
%%%-------------------------------------------------------------------
-module(svr_logic_timer_daily).
-author("Admin").

-behaviour(gen_statem).

-include("common.hrl").

%% API
-export([p/0, call/1, cast/1, start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(ss_state, {}).

%% 休眠间隔(按天计算)
-define(TIMEOUT, 86400).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

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

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  NowTime = time:unixtime(),
  TodayNow = time:get_secs_from_midnight(NowTime),
  Timeout = ?iif(TodayNow =:= 0, 0, ?TIMEOUT - TodayNow),
  Action = {timeout, Timeout * 1000, {wait_time_out}},
  {ok, working, [], Action}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #ss_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(EventType, EventContent, StateName, State) ->
  try
    do_handle_event(EventType, EventContent, StateName, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, EventType, State, Reason, erlang:get_stacktrace()]),
      {keep_state_and_data}
  end.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #ss_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #ss_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 处理定时业务
do_handle_event(timeout, {wait_time_out}, working, State) ->
  case catch do_handle() of
    ok ->
      ?ERROR_MSG("logic_timer_daily normal working...~n");
    Err ->
      ?ERROR_MSG("logic_timer_daily err: ~p~n", [Err])
  end,
  Action = {timeout, ?TIMEOUT * 1000, {wait_time_out}},
  {next_state, working, State, Action};
%% 默认匹配
do_handle_event(_Event, _Msg, _StateName, _State) ->
  keep_state_and_data.

%% 定时任务操作
%% 必须spawn一个进程处理，防止出错导致定时器停止
do_handle() ->
  ?ERROR_MSG("timer_logic_daily refresh begin ..."),
  % 全局数据重置逻辑
  spawn(fun() -> ?TRY_CATCH(svr_global_data:daily_clear()) end),
  ?ERROR_MSG("timer_logic_daily refresh done.", []),
  ok.