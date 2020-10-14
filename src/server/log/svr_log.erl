%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 后台统计进程
%%% @end
%%% Created : 28. 9月 2019 10:56
%%%-------------------------------------------------------------------
-module(svr_log).
-author("sy").

-include("common.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  i/0,
  p/0,
  cast/1,
  call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(LOOP_TIME, 1000).   %% 基本loop时间(毫秒)

-record(svr_log_state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 进程信息
i() ->
  call({info}).

%% @doc 进程Id
p() ->
  dist:whereis_name(local, ?MODULE).

%% @doc call函数
call(Msg) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:call(Pid, Msg);
    _ ->
      {error, err_common_service_not_start}
  end.

%% @doc cast函数
cast(Msg) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, Msg);
    _ ->
      ignore
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #svr_log_state{}} | {ok, State :: #svr_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:process_flag(trap_exit, true),
  erlang:send_after(?LOOP_TIME, self(), {loop, 0}),
  {ok, #svr_log_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #svr_log_state{}) ->
  {reply, Reply :: term(), NewState :: #svr_log_state{}} |
  {reply, Reply :: term(), NewState :: #svr_log_state{}, timeout() | hibernate} |
  {noreply, NewState :: #svr_log_state{}} |
  {noreply, NewState :: #svr_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #svr_log_state{}} |
  {stop, Reason :: term(), NewState :: #svr_log_state{}}).
handle_call(Request, From, State = #svr_log_state{}) ->
  try
    do_handle_call(Request, From, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, from ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Request, From, State, Reason, erlang:get_stacktrace()]),
      {reply, {error, 255}, State}
  end.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #svr_log_state{}) ->
  {noreply, NewState :: #svr_log_state{}} |
  {noreply, NewState :: #svr_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #svr_log_state{}}).
handle_cast(Request, State = #svr_log_state{}) ->
  try
    do_handle_cast(Request, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Request, State, Reason, erlang:get_stacktrace()]),
      {noreply, State}
  end.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #svr_log_state{}) ->
  {noreply, NewState :: #svr_log_state{}} |
  {noreply, NewState :: #svr_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #svr_log_state{}}).
handle_info(Info, State = #svr_log_state{}) ->
  try
    do_handle_cast(Info, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, info ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
      {noreply, State}
  end.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #svr_log_state{}) -> term()).
terminate(_Reason, _State = #svr_log_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #svr_log_state{},
    Extra :: term()) ->
  {ok, NewState :: #svr_log_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #svr_log_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 默认匹配
do_handle_call(_R, _FROM, State) ->
  {reply, ok, State}.

% 循环处理
do_handle_cast({loop, Count}, State) ->
  erlang:send_after(?LOOP_TIME, self(), {loop, Count + 1}),
  {noreply, State};
%% 默认匹配
do_handle_cast(_Reason, State) ->
  ?ERROR_MSG("svr_log do_handle_cast is not match:~w", [_Reason]),
  {noreply, State}.