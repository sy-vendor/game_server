%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% TCP监听处理
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(sys_listener).
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

-export([stop/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @spec stop() -> ok
%% @doc 关闭连接监听服务
stop() ->
  ?INFO("[~w] 正在关闭...", [?MODULE]),
  case util:is_logic_server() of
    true ->
      supervisor:terminate_child(sup_master, sup_acceptor),
      supervisor:terminate_child(sup_master, sys_listener);
    _ -> ignore
  end,
  ?INFO("[~w] 已经关闭...", [?MODULE]),
  ok.

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

-spec(start_link(Port :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%% @doc 开启连接监听服务
start_link(Port) ->
  case gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []) of
    {ok, P} -> {ok, P};
    _Err ->
      ?ERR("~w", [_Err]),
      _Err
  end.

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
init([Port]) ->
  case gen_tcp:listen(Port, config:get_tcp_options()) of
    {ok, LSock} ->
      ?INFO("[~w] 成功监听到端口:~w", [?MODULE, Port]),
      start_acceptor(config:get_acceptor_num(), LSock),
      ?INFO("[~w] 启动完成", [?MODULE]),
      {ok, state};
    {error, Reason}->
      ?ERR("[~w] 无法监听到~w:~w", [?MODULE, Port, Reason]),
      {stop, listen_failure, state}
  end.

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
start_acceptor(0, _LSock)-> ok;
start_acceptor(N, LSock)->
  {ok, Pid} = supervisor:start_child(sup_acceptor, [LSock]),
  Pid ! {event, start},
  start_acceptor(N - 1, LSock).