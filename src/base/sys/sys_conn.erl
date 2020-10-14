%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 连接器(负责收发数据,用来控制角色进程或其它进程)
%%% @end
%%% Created : 14. 10月 2019 14:18
%%%-------------------------------------------------------------------
-module(sys_conn).
-author("sy").

-behaviour(gen_server).

%% API
-export([start_link/0, create/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(sys_conn_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @spec create(Socket, Ip, Port) -> {ok, Pid} | ignore | {error, Error}
%% Socket = port()
%% Ip = binary()
%% Port = int()
%% @doc 创建一个连接器
create(Socket, Ip, Port) ->
  gen_server:start(?MODULE, [Socket, Ip, Port], []).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #sys_conn_state{}} | {ok, State :: #sys_conn_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #sys_conn_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #sys_conn_state{}) ->
  {reply, Reply :: term(), NewState :: #sys_conn_state{}} |
  {reply, Reply :: term(), NewState :: #sys_conn_state{}, timeout() | hibernate} |
  {noreply, NewState :: #sys_conn_state{}} |
  {noreply, NewState :: #sys_conn_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #sys_conn_state{}} |
  {stop, Reason :: term(), NewState :: #sys_conn_state{}}).
handle_call(_Request, _From, State = #sys_conn_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #sys_conn_state{}) ->
  {noreply, NewState :: #sys_conn_state{}} |
  {noreply, NewState :: #sys_conn_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sys_conn_state{}}).
handle_cast(_Request, State = #sys_conn_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #sys_conn_state{}) ->
  {noreply, NewState :: #sys_conn_state{}} |
  {noreply, NewState :: #sys_conn_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sys_conn_state{}}).
handle_info(_Info, State = #sys_conn_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #sys_conn_state{}) -> term()).
terminate(_Reason, _State = #sys_conn_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #sys_conn_state{},
    Extra :: term()) ->
  {ok, NewState :: #sys_conn_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #sys_conn_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
