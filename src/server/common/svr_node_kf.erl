%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 跨服节点分布式管理(只在跨服线启动)
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_node_kf).
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

-export([get_node/0, get_node_id/0, get_all_node/0, add_node/6, del_node/1, hide_node/1, show_node/1, is_kfcenter_node/0, i/0, p/0, cast/1, call/1]).

-define(SERVER, ?MODULE).

%% 进程状态
-record(state, {
  id = 0,           %% 节点ID
  ip = <<>>,        %% 节点IP
  port = 0,         %% 节点端口号
  name = <<>>,      %% 节点名称
  cookie = <<>>,    %% 节点Cookie
  time = 0          %% 更新时间戳(秒)
}).

-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
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

%% @doc 获取所有的节点列表
get_all_node() ->
  ets:tab2list(?ETS_NODE).

%% @doc 判断是否跨服中心节点
is_kfcenter_node() ->
  case ets:lookup(?ETS_SERVER_STATE, main_kfcenter) of
    [#server_state{value = IsMain}|_] -> IsMain;
    _ -> false
  end.

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
      {error, ?ERR_COMMON_SERVICE_NOT_START}
  end.

%% @doc cast函数
cast(Msg) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, Msg);
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

%% @doc 启动进程
start_link([Ip, Port, Id]) ->
  start_link(Ip, Port, Id).
start_link(Ip, Port, Id) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Ip, Port, Id], []).

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
init([Ip, Port, Id]) ->
  State = init_data(Ip, Port, Id),
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
%% 获取节点ID
handle_call({get_node_id}, _From, State) ->
  #state{id = Id} = State,
  {reply, Id, State};
%% 获取节点ID
handle_call({get_node}, _From, State) ->
  #state{id = Id, ip = Ip, port = Port, name = Name, cookie = Cookie, time = Time} = State,
  Node = #node{id = Id, name = Name, ip = Ip, port = Port, cookie = Cookie, time = Time},
  {reply, Node, State};
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
%% 新节点加入
handle_cast({add_node, [Id, Name, Ip, Port, Cookie, Time]}, State) ->
  Node = #node{id = Id, name = Name, ip = Ip, port = Port, cookie = Cookie, time = Time},
  ets:insert(?ETS_NODE, Node),
  {noreply, State};
%% 删除节点信息
handle_cast({del_node, [Id]}, State) ->
  ets:delete(?ETS_NODE, Id),
  {noreply, State};
%% 隐藏节点
handle_cast({hide_node, [Id]}, State) ->
  case ets:lookup(?ETS_NODE, Id) of
    [Node] -> ets:insert(?ETS_NODE, Node#node{state = ?NODE_HIDDEN});
    _ -> skip
  end,
  {noreply, State};
%% 显示节点
handle_cast({show_node, [Id]}, State) ->
  case ets:lookup(?ETS_NODE, Id) of
    [Node] -> ets:insert(?ETS_NODE, Node#node{state = ?NODE_SHOW});
    _ -> skip
  end,
  {noreply, State};
%% 处理新节点加入事件
handle_cast({node_up, Name}, State) ->
  #state{id = Id, ip = Ip, port = Port, name = Name, cookie = Cookie, time = NowTime} = State,
  rpc:cast(Name, ?MODULE, add_node, [Id, Name, Ip, Port, Cookie, NowTime]),
  {noreply, State};
%% 处理节点关闭事件
handle_cast({node_down, Name}, State) ->
  case ets:match_object(?ETS_NODE, #node{name = Name, _ = '_'}) of
    [_] -> ets:match_delete(?ETS_NODE, #node{name = Name, _ = '_'});
    _ -> ignore
  end,
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
%% 初始化进程信息
init_data(Ip, Port, Id) ->
  NowTime = time:unixtime(),
  NodeName = node(),
  NodeCookie = erlang:get_cookie(),
  State = #state{id = Id, ip = Ip, port = Port, name = NodeName, cookie = NodeCookie, time = NowTime},
  % 登记节点信息
  add_node_into_db(Id, Ip, Port, NodeName, NodeCookie, NowTime),
  % 更新服务器状态
  ets:insert(?ETS_SERVER_STATE, #server_state{name = node_id, value = Id}),
  % 通知其他节点加入
  rpc_call_add_node(State),
  State.

%% 新增节点信息
-define(SQL_NODE_REPLACE, <<"replace into `node_kf` (`id`, `ip`, `port`, `name`, `cookie`, `time`) values(~p, '~s', ~p, '~s', '~s', ~p)">>).
add_node_into_db(Id, Ip, Port, Name, Cookie, Time) ->
  Sql = io_lib:format(?SQL_NODE_REPLACE, [Id, Ip, Port, Name, Cookie, Time]),
  ?DB:execute(?POOL_GAME, Sql).

%% 删除节点信息
-define(SQL_NODE_DELETE, <<"delete from `node_kf` where `id`=~p">>).
del_node_from_db(Id) ->
  Sql = io_lib:format(?SQL_NODE_DELETE, [Id]),
  ?DB:execute(?POOL_GAME, Sql).

%% 通知其他节点加入
-define(SQL_NODE_GET, <<"select `id`, `ip`, `port`, `name`, `cookie`, `state`, `time` from `node_kf` order by `id`">>).
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
        case Id =/= LId of % 自己不通知
          false ->
            OtherNode = #node{id = Id, ip = Ip, port = Port, name = Name, cookie = Cookie, time = Time, state = State},
            ets:insert(?ETS_NODE, OtherNode);
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