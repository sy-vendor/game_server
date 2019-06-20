%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 跨服通信节点[只在0线启动]
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_kfclient).
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

-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4, apply_kfgroup/3, is_kfcenter_connect/0, is_node_connect/1, ping_kfcenter/0,
  ping_kfcenter_once/0, refresh_kf_connect/1, node_connect/1, i/0, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("kf.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc CALL发送到跨服节点调用
apply_call(Module, Fun, Args) ->
  call({apply_call, [Module, Fun, Args]}).

%% @doc CALL发送到跨服节点调用
apply_call(KfNode, Module, Fun, Args) ->
  call({apply_call, [KfNode, Module, Fun, Args]}).

%% @doc CAST发送到跨服节点调用
apply_cast(Module, Fun, Args) ->
  cast({apply_cast, [Module, Fun, Args]}).

%% @doc CAST发送到跨服节点调用
apply_cast(KfNode, Module, Fun, Args) ->
  cast({apply_cast, [KfNode, Module, Fun, Args]}).

%% @doc CAST发送到跨服分组节点调用
apply_kfgroup(Module, Fun, Args) ->
  cast({apply_kfgroup, [Module, Fun, Args]}).

%% @doc 是否正常连接跨服中
is_kfcenter_connect() ->
  Node = svr_node:get_kfclient_node(),
  case node() =:= Node of
    true -> % 判断是否跨服通信节点[0节点]
      call({is_kfcenter_connect});
    false when Node =/= none, Node =/= 0, Node =/= undefined ->
      case catch rpc:call(Node, ?MODULE, is_kfcenter_connect, []) of
        true -> true;
        _ -> false
      end;
    false -> false
  end.

%% @doc 检测是否连接节点
is_node_connect(TarNode) ->
  Node = svr_node:get_kfclient_node(),
  case node() =:= Node of
    true -> % 判断是否跨服通信节点[0节点]
      call({is_node_connect, [TarNode]});
    false when Node =/= none, Node =/= 0, Node =/= undefined ->
      case catch rpc:call(Node, ?MODULE, is_connect_kfcenter, [TarNode]) of
        true -> true;
        _ -> false
      end;
    false -> false
  end.

%% @doc 定时连接跨服中心 -- 定时PING
ping_kfcenter() ->
  cast({ping_kfcenter}).

%% @doc 连接跨服中心 -- 只执行一次
ping_kfcenter_once() ->
  cast({ping_kfcenter_once}).

%% @doc 刷新跨服中心连接集群信息
refresh_kf_connect(NodeList) ->
  cast({refresh_kf_connect, [NodeList]}).

%% @doc 连接跨服节点
node_connect(Node) ->
  cast({node_connect, [Node]}).

%% @doc 获取进程信息
i() ->
  call({info}).

%% @doc 获取进程ID
p() ->
  dist:whereis_name(local, ?MODULE).

%% @doc call函数
call(Request) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:call(?MODULE, Request);
    _ ->
      {error, ?ERR_COMMON_SERVICE_NOT_START}
  end.

%% @doc cast函数
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
  State = lib_kfclient:init(),
  {ok, State}.

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
%% CALL发送到跨服节点调用
handle_call({apply_call, [Module, Fun, Args]}, _From, State) ->
  Reply = rpc:call(State#kfclient_state.kfcenter_name, Module, Fun, Args),
  {reply, Reply, State};
%% CALL发送到跨服节点调用
handle_call({apply_call, [Node, Module, Fun, Args]}, _From, State) ->
  Reply = case Node of
            _ when Node =/= none, Node =/= 0, Node =/= undefined -> rpc:call(Node, Module, Fun, Args);
            _ -> noconnect
          end,
  {reply, Reply, State};
%% 获取进程状态
handle_call({info}, _From, State) ->
  IsConnect = lists:member(State#kfclient_state.kfcenter_name, nodes(hidden)),
  Info = [
    {is_connect_kf, IsConnect},
    {state, State}
  ],
  {reply, Info, State};
%% 检查是否与跨服中心管理节点连接
handle_call({is_kfcenter_connect}, _From, State) ->
  IsConnect = lib_kfclient:is_node_connect(State#kfclient_state.kfcenter_name, State),
  {reply, IsConnect, State};
% 检查节点是否连通状态
handle_call({is_node_connect, [Node]}, _From, State) ->
  IsConnect = lib_kfclient:is_node_connect(Node, State),
  {reply, IsConnect, State};
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
%% CAST发送到跨服节点调用（在100线上调用）
handle_cast({apply_cast, [Module, Fun, Args]}, State) ->
  rpc:cast(State#kfclient_state.kfcenter_name, Module, Fun, Args),
  {noreply, State};
%% CAST发送到跨服节点调用（在任意指定跨服节点上调用）
handle_cast({apply_cast, [Node, Module, Fun, Args]}, State) ->
  case Node of
    _ when Node =/= none, Node =/= 0, Node =/= undefined -> rpc:cast(Node, Module, Fun, Args);
    _ -> ignore
  end,
  {noreply, State};
%% CAST发送到跨服分组（200线）节点调用
handle_cast({apply_kfgroup, [Module, Fun, Args]}, State) ->
  rpc:cast(State#kfclient_state.kfcenter_name, lib_kfgroup, apply_kfgroup, [Module, Fun, Args]),
  {noreply, State};
%% 定时连接跨服中心 -- 定时PING
handle_cast({ping_kfcenter}, State) ->
  erlang:send_after(?PING_KFCENTER_INTERVAL, self(), {ping_kfcenter}),
  lib_kfclient:ping_kfcenter(State),
  {noreply, State};
%% 连接跨服中心 -- 只执行一次
handle_cast({ping_kfcenter_once}, State) ->
  lib_kfclient:ping_kfcenter(State),
  {noreply, State};
%% 刷新跨服中心连接集群信息
handle_cast({refresh_kf_connect, [NodeList]}, State) ->
  lib_kfclient:refresh_kf_connect(State#kfclient_state.connected, NodeList),
  {noreply, State};
%% 连接跨服节点
handle_cast({node_connect, [Node]}, State) ->
  StateN = lib_kfclient:node_connect(Node, State),
  {noreply, StateN};
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
