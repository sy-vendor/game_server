%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% TCP Acceptor
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(sys_acceptor).
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

-export([start_link/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================

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

%% @doc 启动acceptor
start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

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
init([LSock]) ->
  ?INFO("[~w] 已启动...", [?MODULE]),
  self() ! loop,
  {ok, {LSock}}.

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
handle_info(loop, State = {LSock}) ->
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end)),
      self() ! loop, %% 继续等待下一个
      {noreply, State};
    {error, Reason} ->
      ?ERR("接受socket连接时发生了未预料的错误:~w", [Reason]),
      {noreply, State}
  end;
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
%% 接受一个连接
accept(Socket) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Data} ->
      create(Data, Socket);
    Else ->
      Peers = inet:peernames(Socket),
      Sock = inet:socknames(Socket),
      ?INFO("获取握手信息失败1:~w,~w~w", [Peers, Sock, Else]),
      gen_tcp:close(Socket)
  end.

create(<<"game server connect ~~~">>, Socket) -> create_conn(Socket);
create(Else, Socket) ->
  Peers = inet:peernames(Socket),
  Sock = inet:socknames(Socket),
  ?INFO("握手失败:~w,~w~w", [Peers, Sock, Else]),
  gen_tcp:close(Socket).

%% 创建连接进程
create_conn(Socket) ->
  try
    {ok, {Ip, Port}} = inet:peername(Socket),
    {ok, Pid} = sys_conn:create(Socket, Ip, Port),
    ok = gen_tcp:controlling_process(Socket, Pid)
  catch
    T:X ->
      ?ERR("建立连接失败[~w : ~w](~w)", [T, X, erlang:get_stacktrace()]),
      gen_tcp:close(Socket)
  end.