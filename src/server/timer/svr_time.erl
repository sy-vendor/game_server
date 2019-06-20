%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 时间
%%% @end
%%% Created : 21. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_time).
-author("sy").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  now/0,
  now_seconds/0,
  now_milseconds/0,
  now_usseconds/0,
  get_time_offset/0,
  set_time_offset/1,
  set_time_offset_node/2,
  add_time_offset/1,
  universal_time_dst/0,
  cpu_time/0,
  info/0
]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(CLOCK, 100).
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% 获取当前时间
now() ->
  [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
  Now.

%% 秒
now_seconds()->
  [{timer, {Now, _}}] = ets:lookup(ets_timer, timer),
  {MegaSecs, Secs, _MicroSecs} = Now,
  MegaSecs * 1000000 + Secs.

%% 毫秒
now_milseconds() ->
  {M, S, Ms} = svr_time:now(),
  (M * 1000000000000 + S * 1000000 + Ms) div 1000.

%% 微秒
now_usseconds() ->
  {M, S, Ms} = svr_time:now(),
  M * 1000000000000 + S * 1000000 + Ms.

%% 获取时间
get_time_offset() ->
  Time = gen_server:call(?MODULE, {get_time_offset}),
  Time.

%% 设置时间
set_time_offset(Seconds) when is_integer(Seconds) ->
  gen_server:cast(?MODULE, {set_time_offset, Seconds}),
  ok;
set_time_offset(Date) when is_list(Date)->
  {MegaSec, Sec, _MicroSec} = os:timestamp(),
  Now = MegaSec * 1000000 + Sec,
  Seconds = date_to_seconds(Date),
  [rpc:cast(NodeName, ?MODULE, set_time_offset_node, [NodeName, Seconds-Now]) || #node{name = NodeName} <- svr_node:get_logic_node()].

set_time_offset_node(_, Info) ->
  gen_server:cast(?MODULE, {set_time_offset, Info}).

%% 增加秒数
add_time_offset(Seconds) ->
  gen_server:cast(?MODULE, {add_time_offset, Seconds}),
  ok.

%% 通用时间
universal_time_dst() ->
  [{dst, DiffTime}] = ets:lookup(ets_timer, dst),
  DiffTime.

%% 机器时间
cpu_time() ->
  [{timer, {_, Wallclock}}] = ets:lookup(ets_timer, timer),
  Wallclock.

%% 进程信息
info() ->
  [ets:info(ets_timer), ets:tab2list(ets_timer)].

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:process_flag(trap_exit, true),
  put(dic_time_offset, 0),
  ets:new(ets_timer, [set, public, named_table]),
  ets:insert(ets_timer, {timer, {erlang:timestamp(), 0}}),
  Day = {{2000, 1, 1}, {0, 0, 0}},
  [UTC] = calendar:local_time_to_universal_time_dst(Day),
  Tick1 = calendar:datetime_to_gregorian_seconds(Day),
  Tick2 = calendar:datetime_to_gregorian_seconds(UTC),
  ets:insert(ets_timer, {dst, Tick2 - Tick1}),
  erlang:send_after(?CLOCK, self(), {event, clock}),
  {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
%% 获取当前时间
handle_call({get_time_offset}, _,  State) ->
  Offset = get(dic_time_offset),
  {reply, Offset, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
%% 设置当前时间位移,秒单位
handle_cast({set_time_offset, Seconds}, State) ->
  put(dic_time_offset, Seconds),
  {noreply, State};
%% 增加当前时间位移,秒单位
handle_cast({add_time_offset, Seconds}, State) ->
  Offset = get(dic_time_offset),
  put(dic_time_offset, Offset + Seconds),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
%% 定时更新
handle_info({event, clock}, State) ->
  erlang:send_after(?CLOCK, self(), {event, clock}),
  {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
  {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
  Offset = get(dic_time_offset),
  NewTimeCall = {MegaSecs, Secs + Offset, _MicroSecs},
  ets:insert(ets_timer, {timer, {NewTimeCall, Time_Since_Last_Call}}),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 最低精确到秒
date_to_seconds(Date) ->
  Dates = string:tokens(type:object_to_list(Date), "-/ :"),
  [Year, Month, Day, Hours, Mins, Sec | _] = Dates,
  [UTC] = calendar:local_time_to_universal_time_dst({{to_integer(Year), to_integer(Month), to_integer(Day)}, {to_integer(Hours), to_integer(Mins), to_integer(Sec)}}),
  Tick = calendar:datetime_to_gregorian_seconds(UTC),
  Tick - ?DIFF_SECONDS_0000_1900.

to_integer(Msg) when is_integer(Msg) ->
  Msg;
to_integer(Msg) when is_binary(Msg) ->
  Msg2 = binary_to_list(Msg),
  list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) ->
  list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) ->
  round(Msg);
to_integer(_Msg) ->
  erlang:throw(other_value).