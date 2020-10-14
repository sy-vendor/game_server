%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% TCP监听处理
%%% @end
%%% Created : 29. 9月 2019 15:31
%%%-------------------------------------------------------------------
-module(sys_listener).
-author("sy").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(sys_listener_state, {}).

-define(TCP_OPTIONS, [
  binary
  ,{packet, 0}
  ,{active, false}
  ,{reuseaddr, true}
  ,{nodelay, false}
  ,{delay_send, true}
  ,{exit_on_close, false}
  ,{send_timeout, 10000}
  ,{send_timeout_close, false}
]).

-define(TCP_ACCEPTOR_NUM, 12).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec start_link(Port) -> {ok, Pid} | ignore | {error, Error}
%% Port = integer()
%% @doc 开启连接监听服务
start_link(Port) ->
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []) of
    {ok, P} -> {ok, P};
    _Err ->
      ?ERR("sys listener error:~w", [_Err]),
      _Err
  end.

%% @spec stop() -> ok
%% @doc 关闭连接监听服务
stop() ->
  ?INFO("[~w] Closing...", [?MODULE]),
  supervisor:terminate_child(sup, sup_acceptor),
  supervisor:terminate_child(sup, sys_listener),
  ?INFO("[~w] it has been closed...", [?MODULE]),
  ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #sys_listener_state{}} | {ok, State :: #sys_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Port]) ->
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSock} ->
      ?INFO("[~w] Listening port success:~w", [?MODULE, Port]),
      start_acceptor(?TCP_ACCEPTOR_NUM, LSock),
      ?INFO("[~w] Startup complete", [?MODULE]),
      {ok, state};
    {error, Reason}->
      ?ERR("[~w] Monitor failed~w:~w", [?MODULE, Port, Reason]),
      {stop, listen_failure, state}
  end.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #sys_listener_state{}) ->
  {reply, Reply :: term(), NewState :: #sys_listener_state{}} |
  {reply, Reply :: term(), NewState :: #sys_listener_state{}, timeout() | hibernate} |
  {noreply, NewState :: #sys_listener_state{}} |
  {noreply, NewState :: #sys_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #sys_listener_state{}} |
  {stop, Reason :: term(), NewState :: #sys_listener_state{}}).
handle_call(_Request, _From, State = #sys_listener_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #sys_listener_state{}) ->
  {noreply, NewState :: #sys_listener_state{}} |
  {noreply, NewState :: #sys_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sys_listener_state{}}).
handle_cast(_Request, State = #sys_listener_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #sys_listener_state{}) ->
  {noreply, NewState :: #sys_listener_state{}} |
  {noreply, NewState :: #sys_listener_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sys_listener_state{}}).
handle_info(_Info, State = #sys_listener_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #sys_listener_state{}) -> term()).
terminate(_Reason, _State = #sys_listener_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #sys_listener_state{},
    Extra :: term()) ->
  {ok, NewState :: #sys_listener_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #sys_listener_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_acceptor(0, _LSock)-> ok;
start_acceptor(N, LSock)->
  {ok, Pid} = supervisor:start_child(sup_acceptor, [LSock]),
  Pid ! {event, start},
  start_acceptor(N - 1, LSock).