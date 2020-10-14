%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% TCP Acceptor
%%% @end
%%% Created : 29. 9月 2019 17:58
%%%-------------------------------------------------------------------
-module(sys_acceptor).
-author("sy").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(sys_acceptor_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 启动acceptor
start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #sys_acceptor_state{}} | {ok, State :: #sys_acceptor_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([LSock]) ->
  ?INFO("[~w] sys_acceptor start...", [?MODULE]),
  self() ! loop,
  {ok, {LSock}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #sys_acceptor_state{}) ->
  {reply, Reply :: term(), NewState :: #sys_acceptor_state{}} |
  {reply, Reply :: term(), NewState :: #sys_acceptor_state{}, timeout() | hibernate} |
  {noreply, NewState :: #sys_acceptor_state{}} |
  {noreply, NewState :: #sys_acceptor_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #sys_acceptor_state{}} |
  {stop, Reason :: term(), NewState :: #sys_acceptor_state{}}).
handle_call(_Request, _From, State = #sys_acceptor_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #sys_acceptor_state{}) ->
  {noreply, NewState :: #sys_acceptor_state{}} |
  {noreply, NewState :: #sys_acceptor_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sys_acceptor_state{}}).
handle_cast(_Request, State = #sys_acceptor_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #sys_acceptor_state{}) ->
  {noreply, NewState :: #sys_acceptor_state{}} |
  {noreply, NewState :: #sys_acceptor_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sys_acceptor_state{}}).
handle_info(loop, State = {LSock}) ->
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end)),
      self() ! loop, %% 继续等待下一个
      {noreply, State};
    {error, Reason} ->
      ?ERR("accept socket connection error:~w", [Reason]),
      {noreply, State}
  end;
handle_info(_Info, State = #sys_acceptor_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #sys_acceptor_state{}) -> term()).
terminate(_Reason, _State = #sys_acceptor_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #sys_acceptor_state{},
    Extra :: term()) ->
  {ok, NewState :: #sys_acceptor_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #sys_acceptor_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 接受一个连接
accept(Socket) ->
  case gen_tcp:recv(Socket, 23, 10000) of
    {ok, Data} ->
      create(Data, Socket);
    Else ->
      Peers = inet:peernames(Socket),
      Sock = inet:socknames(Socket),
      ?INFO("obtain shake hands fail:~w,~w, ~w", [Peers, Sock, Else]),
      gen_tcp:close(Socket)
  end.

create(Else, Socket) ->
  Peers = inet:peernames(Socket),
  Sock = inet:socknames(Socket),
  ?INFO("shake hands fail:~w,~w, ~w", [Peers, Sock, Else]),
  gen_tcp:close(Socket).