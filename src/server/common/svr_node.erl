%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 游戏节点分布式管理
%%% @end
%%% Created : 06. 10月 2019 11:12
%%%-------------------------------------------------------------------

-module(svr_node).
-behaviour(gen_server).
-include("common.hrl").

%% EXPORT API
-export([
    i/0,
    p/0,
    call/1,
    cast/1,
    start_link/1,
    start_link/3,
    get_node_id/0,
    get_node/0,
    add_node/6,
    del_node/1,
    hide_node/1,
    show_node/1,
    rpc_cast_other/3,
    rpc_cast_logic/3,
    get_logic_node/0,
    get_other_logic_node/0,
    get_other_node/0,
    get_all_node/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    do_handle_call/3,
    do_handle_cast/2
]).

%% 进程状态
-record(state, {
    id = 0,           % 节点ID
    ip = <<>>,        % 节点IP
    port = 0,         % 节点端口号
    name = <<>>,      % 节点名称
    cookie = <<>>,    % 节点Cookie
    time = 0          % 更新时间戳(秒)
}).

%% =============================================================================
%% API
%% =============================================================================

%% @doc 启动进程
start_link([Ip, Port, Id]) ->
    io:format("svr node start link Ip:~w, Port:~w, Id:~w~n", [Ip, Port, Id]),
    start_link(Ip, Port, Id).
start_link(Ip, Port, Id) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Ip, Port, Id], []).

%% @doc 查询当前节点ID
get_node_id() ->
    call({get_node_id}).

%% @doc 查询当前节点
get_node() ->
    call({get_node}).

%% @doc 新节点加入
add_node(Id, Name, Ip, Port, Cookie, Time) ->
    cast({add_node, [Id, Name, Ip, Port, Cookie, Time]}).

%% @doc 删除节点
del_node(Id) ->
    cast({del_node, [Id]}).

%% @doc 隐藏节点
hide_node(Id) ->
    cast({hide_node, [Id]}).

%% @doc 显示节点
show_node(Id) ->
    cast({show_node, [Id]}).

%% @doc 对其他节点进行rpc_cast
rpc_cast_other(Mod, Fun, Args) ->
    NodeList = get_other_node(),
    [rpc:cast(Node, Mod, Fun, Args) || #node{name = Node} <- NodeList].

%% @doc 对逻辑节点进行rpc_cast
rpc_cast_logic(Mod, Fun, Args) ->
    NodeList = get_logic_node(),
    [rpc:cast(Node, Mod, Fun, Args) || #node{name = Node} <- NodeList].

%% @doc 获取所有逻辑节点列表
get_logic_node() ->
    NodeList = get_all_node(),
    [Node || #node{id = Id} = Node <- NodeList, Id >= ?NODE_ID_GAME].

%% @doc 获取除本节点外的其它逻辑节点列表
get_other_logic_node() ->
    NodeList = get_other_node(),
    [Node || #node{id = Id} = Node <- NodeList, Id >= ?NODE_ID_GAME].

%% @doc 获取除本节点外的其它节点列表
get_other_node() ->
    NodeList = get_all_node(),
    SelfNode = node(),
    [Node || #node{name = OtherNode} = Node <- NodeList, OtherNode =/= SelfNode].

%% @doc 获取所有的节点列表
get_all_node() ->
    ets:tab2list(?ETS_NODE).

%% @doc 进程信息
i() ->
    call({info}).
    
%% @doc 进程ID
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

%% =============================================================================
%% Gen_server Callbacks
%% =============================================================================

init([Ip, Port, Id]) ->
    State = init_data(Ip, Port, Id),
    {ok, State}.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, from ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Request, From, State, Reason, erlang:get_stacktrace()]),
            {reply, {error, 255}, State}
    end.

handle_cast(Request, State) ->
    try
        do_handle_cast(Request, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Request, State, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_cast(Info, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, info ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% 获取节点ID
do_handle_call({get_node_id}, _FROM, State) ->
    #state{id = Id} = State,
    {reply, Id, State};
%% 获取节点ID
do_handle_call({get_node}, _FROM, State) ->
    #state{id = Id, ip = Ip, port = Port, name = Name, cookie = Cookie, time = Time} = State,
    Node = #node{id = Id, name = Name, ip = Ip, port = Port, cookie = Cookie, time = Time},
    {reply, Node, State};
%% 默认匹配
do_handle_call(_R, _FROM, State) ->
    {reply, ok, State}.

%% 新节点加入
do_handle_cast({add_node, [Id, Name, Ip, Port, Cookie, Time]}, State) ->
    Node = #node{id = Id, name = Name, ip = Ip, port = Port, cookie = Cookie, time = Time},
    ets:insert(?ETS_NODE, Node),
    {noreply, State};
%% 删除节点信息
do_handle_cast({del_node, [Id]}, State) ->
    ets:delete(?ETS_NODE, Id),
    {noreply, State};
%% 隐藏节点
do_handle_cast({hide_node, [Id]}, State) ->
    case ets:lookup(?ETS_NODE, Id) of
        [Node] -> ets:insert(?ETS_NODE, Node#node{state = ?NODE_HIDDEN});
        _ -> skip
    end,
    {noreply, State};
%% 显示节点
do_handle_cast({show_node, [Id]}, State) ->
    case ets:lookup(?ETS_NODE, Id) of
        [Node] -> ets:insert(?ETS_NODE, Node#node{state = ?NODE_SHOW});
        _ -> skip
    end,
    {noreply, State};
%% 处理新节点加入事件
do_handle_cast({node_up, Name}, State) ->
    #state{id = Id, ip = Ip, port = Port, name = Name, cookie = Cookie, time = NowTime} = State,
    rpc:cast(Name, ?MODULE, add_node, [Id, Name, Ip, Port, Cookie, NowTime]),
    {noreply, State};
%% 处理节点关闭事件
do_handle_cast({node_down, Name}, State) ->
    case ets:match_object(?ETS_NODE, #node{name = Name, _ = '_'}) of
        [_] -> ets:match_delete(?ETS_NODE, #node{name = Name, _ = '_'});
        _ -> ignore
    end,
    {noreply, State};
%% 默认匹配
do_handle_cast(_Reason, State) ->
    {noreply, State}.

%% 初始化进程信息
init_data(Ip, Port, Id) ->
    NowTime = time:unixtime(),
    NodeName = node(),
    NodeCookie = erlang:get_cookie(),
    State = #state{id = Id, ip = Ip, port = Port, name = NodeName, cookie = NodeCookie, time = NowTime},
    % 登记节点信息
    add_node_into_db(Id, Ip, Port, NodeName, NodeCookie, NowTime),
    EtsNode = #node{id = Id, ip = Ip, port = Port, name = NodeName, cookie = NodeCookie, time = NowTime},
    ets:insert(?ETS_NODE, EtsNode),
    % 更新服务器状态
    ets:insert(?ETS_SERVER_STATE, #server_state{name = node_id, value = Id}),
    % 通知其他节点加入
    rpc_call_add_node(State),
    State.

%% 新增节点信息
-define(SQL_NODE_REPLACE, <<"replace into `node` (`id`, `ip`, `port`, `name`, `cookie`, `time`) values(~p, '~s', ~p, '~s', '~s', ~p)">>).
add_node_into_db(Id, Ip, Port, Name, Cookie, Time) ->
    Sql = io_lib:format(?SQL_NODE_REPLACE, [Id, Ip, Port, Name, Cookie, Time]),
    ?DB:execute(?POOL_GAME, Sql).

%% 删除节点信息
-define(SQL_NODE_DELETE, <<"delete from `node` where `id`=~p">>).
del_node_from_db(Id) ->
    Sql = io_lib:format(?SQL_NODE_DELETE, [Id]),
    ?DB:execute(?POOL_GAME, Sql).

%% 通知其他节点加入
-define(SQL_NODE_GET, <<"select `id`, `ip`, `port`, `name`, `cookie`, `state`, `time` from `node` order by `id`">>).
rpc_call_add_node(NodeState) ->
    Sql = io_lib:format(?SQL_NODE_GET, []),
    case ?DB:get_all(?POOL_GAME, Sql) of
        [] -> ignore;
        NodeList ->
            F = fun([Id, IpBin, Port, NameBin, CookieBin, State, Time]) ->
                Name = type:object_to_atom(NameBin),
                Cookie = type:object_to_atom(CookieBin),
                Ip = type:object_to_list(IpBin),
                #state{id = LId, name = LName, ip = LIp, port = LPort, cookie = LCookie, time = LTime} = NodeState,
                case Id =/= LId of % 自己不写入和不通知
                    false -> ignore;
                    true ->
                        case net_adm:ping(Name) of
                            pong ->
                                OtherNode = #node{id = Id, ip = Ip, port = Port, name = Name, cookie = Cookie, time = Time, state = State},
                                ets:insert(?ETS_NODE, OtherNode),
                                % 通知已有的节点添加自己信息
                                rpc:call(Name, ?MODULE, add_node, [LId, LName, LIp, LPort, LCookie, LTime]);
                            pang ->
                                del_node_from_db(Id)
                        end
                end
            end,
            lists:foreach(F, NodeList)
    end.