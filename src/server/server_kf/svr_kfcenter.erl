%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 跨服中心节点[100, 200, 300, 301, ...]
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_kfcenter).
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

-export([restart_node/1, node_connect/3, get_all_node/0, apply_to_all_node/3, apply_to_all_node/4, i/0, p/0, cast/1, call/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("kf.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 通知重启
restart_node(Node) ->
  case lib_kfcenter:apply_call(Node, init, restart, []) of
    ok -> 1;
    _ -> 0
  end.

%% @doc 添加跨服节点
node_connect(Node, Platform, ServerNum) ->
  cast({node_connect, [Node, Platform, ServerNum]}).

%% @doc 获取所有节点
get_all_node() ->
  call({get_all_node}).

%% @doc 通知所有节点执行
apply_to_all_node(Module, Method, Args) ->
  cast({apply_to_all_node, [Module, Method, Args]}).

%% @doc 通知所有节点执行
%% @param: TimeOut:毫秒
apply_to_all_node(Module, Method, Args, TimeOut) ->
  cast({apply_to_all_node, [Module, Method, Args, TimeOut]}).

%% @doc 获取进程信息
i() ->
  call({info}).

%% @doc 获取进程ID
p() ->
  dist:whereis_name(local, ?MODULE).

%% @doc call
call(Request) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:call(?MODULE, Request);
    _ ->
      {error, ?ERR_COMMON_SERVICE_NOT_START}
  end.

%% @doc cast
cast(Request) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(?MODULE, Request);
    _ ->
      ignore
  end.

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
  process_flag(trap_exit, true),
  lib_kfcenter:init(),
  {ok, #kfcenter_state{}}.

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
%% 获取进程信息
handle_call({info}, _From, State) ->
  Reply = lib_kfcenter:info(),
  {reply, Reply, State};
%% 获取所有节点
handle_call({get_all_node}, _From, State) ->
  Reply = lib_kfcenter:get_client_nodes(),
  {reply, Reply, State};
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
%% 刷新跨服中心连接列表
handle_cast({refresh_kf_connect}, State) ->
  lib_kfcenter:refresh_kf_connect(),
  {noreply, State};
%% 游戏服加入
handle_cast({node_connect, [Node, Platform, ServerNum]}, State) ->
  lib_kfcenter:node_connect(Node, Platform, ServerNum),
  {noreply, State};
%% 游戏服退出
handle_cast({node_down, [Node, InfoList]}, State) ->
  lib_kfcenter:node_down(Node, InfoList),
  {noreply, State};
%% 通知所有节点
handle_cast({apply_to_all_node, [Module, Method, Args]}, State) ->
  lib_kfcenter:apply_to_all_node(Module, Method, Args),
  {noreply, State};
handle_cast({apply_to_all_node, [Module, Method, Args, TimeOut]}, State) ->
  lib_kfcenter:apply_to_all_node(Module, Method, Args, TimeOut),
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
handle_info(Info, State) ->
  try
    handle_cast(Info, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
      {noreply, State}
  end.

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
