%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 中央通讯接口
%%% @end
%%% Created : 26. 十二月 2018 14:48
%%%-------------------------------------------------------------------
-module(svr_server).
-author("suyang").

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

-export([ready/3,
    call/4,
    cast/4,
    is_center/0,
    is_connect/1,
    get_node_type/0,
    set_ver/1]).

-define(SERVER, ?MODULE).

-record(state, {
    nodes = [],  %% 中央服节点列表
    ver          %% 当前系统版本
}).

-record(server_node, {
    type,        %% 中央服节点类型
    node,        %% 中央服节点名称
    mpid,        %% 进程pid
    mref         %% 进程监控ref
}).

-include("common.hrl").
-include("center.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 当中央服节点启动后会通知当前节点
ready(Type, Node, Mpid) ->
    gen_server:cast(?MODULE, {ready, Type, Node, Mpid}).

%% @spec call(Type, M, F, A) ->
%% Type = atom()
%% M = atom()
%% F = atom()
%% A = term
%% @doc 对中央服发起同步调用，
%% Type: 中央服结点类型, 具体看cross.hrl的cross_type_xx
call(Type, M, F, A) when is_atom(Type) ->
    case get_server_node(Type) of
        #server_node{node = Node} ->
            rpc:call(Node, M, F, A);
        _ ->
            {error, cross_not_ready}
    end.

%% cast(Type, M, F, A) ->
%% Type = atom()
%% M = atom()
%% F = atom()
%% A = term()
%% @doc 对中央服发起异步调用
%% Type: 中央服结点类型
cast(Type, M, F, A) when is_atom(Type) ->
    case get_server_node(Type) of
        #server_node{node = Node} ->
            rpc:cast(Node, M, F, A);
        _ ->
            {error, center_not_ready}
    end.

%% 设置版本号
set_ver(Ver) ->
    gen_server:cast(?MODULE, {set_ver, Ver}).

%% @spec is_connect(Type) -> {true, {Node, Pid}} | false
%% Type =atom()
%% Node = atom()
%% Pid = pid()
%% @doc 判断是否连接了Type中央结点
%% Type: 中央服结点类型
is_connect(Type) ->
    case get_server_node(Type) of
        #server_node{node = Node, mpid = Pid} when is_pid(Pid) ->
            {true, {Node, Pid}};
        _ ->
            false
    end.

%% @spec is_center() -> false | true
%% 判断本节点是否中央服
is_center() ->
    case get_node_type() of
        ?SERVER_TYPE_CENTER -> true;
        _ -> false
    end.

%% @spec get_node_type() -> atom()
%% 获取节点类型
%% Type: 中央服结点类型
get_node_type() ->
    sys_env:get(node_type).

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
    State = #state{ver = sys_env:get(version)},
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
handle_cast({set_ver, Ver}, State) ->
    ?INFO("set the version number to：~w", [Ver]),
    {noreply, State#state{ver = Ver}};
handle_cast({ready, Type, Node, Pid}, State = #state{nodes = Nodes, ver = _LocalVer})  ->
    case get_server_node(Type) of
        undefined ->
            ?INFO("has been with the Central Service [type = ~w, node = ~w]establish connection", [Type, Node]),
            Mref = erlang:monitor(process, Pid),
            NewNodes = put_server_node(Nodes, Type, Node, Pid, Mref),
            erlang:send(Pid, {ready, self(), []}),
            {noreply, State#state{nodes = NewNodes}};
        #server_node{node = Node, mpid = Pid} ->
            ?INFO("has been with the Central Service[type = ~w, node = ~w]establish connection, repeated requests", [Type, Node]),
            erlang:send(Pid, {ready, self(), []}),
            {noreply, State};
        #server_node{node = OldNode} ->
            ?INFO("has been with the Central Service[type = ~w, node = ~w]establish connection, the new request was discarded[node = ~w]", [Type, OldNode, Node]),
            {noreply, State}
    end;
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
handle_info({'DOWN', Mref, _Type, _Object, Reason}, State = #state{nodes = Nodes}) ->
    case lists:keyfind(Mref, #server_node.mref, Nodes) of
        #server_node{type = NodeType, node = Node} ->
            ?INFO("center server[type=~w, node=~w] connection disconnected: ~w", [NodeType, Node, Reason]),
            NewNodes = delete_node(NodeType, Nodes),
            demonitor(Mref),
            {noreply, State#state{nodes = NewNodes}};
        _ ->
            ?ERROR_MSG("receive a message that the central node is disconnected，but there is no central node.[mref=~w]", [Mref]),
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
%% @doc 获取中央的node
get_server_node(Type) ->
    case sys_env:get(server) of
        Server when is_list(Server) ->
            case lists:keyfind(Type, #server_node.type, Server) of
                Node = #server_node{} -> Node;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

%% @doc 设置新的node
put_server_node(Nodes, Type, Node, Pid, Mref) ->
    ServerNode = #server_node{type = Type, node = Node, mpid = Pid, mref = Mref},
    NewNodes = [ServerNode | delete_node(Type, Nodes)],
    sys_env:set(server, NewNodes),
    NewNodes.

%% @doc 删除结点
delete_node(Type, Nodes) ->
    NewNodes = lists:keydelete(Type, #server_node.type, Nodes),
    sys_env:set(server, NewNodes),
    NewNodes.