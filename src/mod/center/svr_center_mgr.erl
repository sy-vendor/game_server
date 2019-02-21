%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 中央节点管理
%%% @end
%%% Created : 26. 十二月 2018 14:05
%%%-------------------------------------------------------------------
-module(svr_center_mgr).
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

-export([i/0,
    p/0,
    cast/1,
    call/1,
    cast/4,
    cast/5,
    call/5,
    get_srv_data/0,
    get_srv_data/1,
    update_srv_data/1,
    display_all/0,
    print_disconnected/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("center.hrl").
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 进程信息
i() ->
    call({info}).

%% @doc 进程ID
p() ->
    util_dist:whereis_name(local, ?MODULE).

%% @doc call函数
call(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, server_not_start}
    end.

%% @doc cast函数
cast(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        _ ->
            ignore
    end.

%% @spec cast(all, M, F, A) -> ok
%% @doc 广播消息到所有节点
cast(all, M, F, A) ->
    gen_server:cast(?MODULE, {cast, all, M, F, A}).

%% @spec cast(node, SrvId, M, F, A) -> ok
%% @doc 发送异步消息到指定srv_id的远端服务器
cast(node, SrvId, M, F, A) ->
    case lib_center:get_srv(SrvId) of
        #server_srv{node = Node, mpid = Mpid} when is_pid(Mpid) ->
            rpc:cast(Node, M, F, A);
        %% 目标服务器没连接到中央服
        _ ->
            ?ERROR_MSG("center not connect[~s]，unable to send this asynchronous message[~w, ~w, ~w]", [SrvId, M, F, A])
    end.

%% @spec call(node, SrvId, M, F, A) -> ok | {error, Reason}
%% @doc 发送同步消息到指定srv_id的远端服务器
call(node, SrvId, M, F, A) ->
    case lib_center:get_srv(SrvId) of
        #server_srv{node = Node, mpid = Mpid} when is_pid(Mpid) ->
            rpc:call(Node, M, F, A);
        %% 目标服务器没连接到中央服
        _ ->
            ?ERROR_MSG("center not connect[~s]，unable to send this synchronization message[~w, ~w, ~w]", [SrvId, M, F, A]),
            {error, not_connected}
    end.

%% @spec get_srv_data() -> [#server_srv_data{} | ..]
%% 获取所有的server_srv数据（无论是否连接上的）
get_srv_data() ->
    case sys_env:get(?SERVER_SRV_DATA_LIST) of
        L when is_list(L) -> L;
        _ -> []
    end.

%% @spec get_srv_data(SrvId)
%% 获取指定的cross_srv_data
get_srv_data(SrvId) ->
    lists:keyfind(SrvId, #server_srv_data.srv_id, get_srv_data()).

%% @spec save_srv_data(SrvList)
%% SrvList = [#server_srv_data{} | ..]
%% 保存所有server_srv_data
save_srv_data(SrvList) ->
    sys_env:save(?SERVER_SRV_DATA_LIST, SrvList).

%% @spec update_srv_data(SrvDataList)
%% SrvDataList = [#server_srv_data{}]
%% 重新加载server_srv_data
update_srv_data(SrvDataList) ->
    gen_server:cast(?MODULE, {update_srv_data, SrvDataList}).


%% @spec display_all() -> ok
%% @doc 列出所有svr_center_mgr管理的数据
display_all() ->
    gen_server:cast(?MODULE, display_all).

%% 打印所有未连接上的镜像服务器
print_disconnected() ->
    L = get_srv_data(),
    ?INFO("there are currently no nodes on the connection:"),
    do_print_disconnected(L, []).
do_print_disconnected([], Result) -> Result;
do_print_disconnected([#server_srv_data{srv_id = SrvId, name = Name} | T], Result) ->
    case lib_center:get_srv(SrvId) of
        #server_srv{mpid = Mpid} when is_pid(Mpid) ->
            do_print_disconnected(T, Result);
        _ ->
            ?INFO("~w, ~s", [SrvId, Name]),
            do_print_disconnected(T, Result)
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
    lib_center:init(),
    init_srv(),
    {ok, #state{}}.

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
%% 列出所有数据
handle_cast(display_all, State) ->
    lists:foreach(fun(ServerSrv = #server_srv_data{}) ->
        ?INFO("Mirror Server Information:(~w)", [?record_kv(ServerSrv, server_srv_data)])
                  end, get_srv_data()),
    {noreply, State};
%% 更新mirror_data
handle_cast({update_srv_data, SrvDataList}, State) ->
    ?INFO("update_srv_data: ~w", [SrvDataList]),
    do_update_srv_data(SrvDataList),
    save_srv_data(SrvDataList),
    {noreply, State};
%% 广播
handle_cast({cast, all, M, F, A}, State) ->
    lib_center:cast(M, F, A),
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
%% @doc 初始化server_srv
init_srv() ->
    L = get_sql_srv_data(),
    lists:foreach(fun(SrvData = #server_srv_data{}) -> do_start_srv(SrvData) end, L).

%% @doc 启动server_srv
do_start_srv(SrvData = #server_srv_data{srv_id = SrvId}) ->
    Ss = lib_center:convert_srv(SrvData),
    case svr_center:start_link(Ss) of
        {ok, _Pid} ->
            ignore;
        _Err -> ?ERROR_MSG("initialize mirror server[SrvId=~s] error:~w", [SrvId, _Err])
    end.

%% 更新分组
do_update_srv_data(NewSrvDataList) ->
    CsList = lib_center:get_srvs(),
    stop_srv(CsList, NewSrvDataList),
    start_srv(NewSrvDataList).

stop_srv([], _) -> ok;
stop_srv([#server_srv{pid = Pid, srv_id = SrvId, node = Node, cookie = Cookie} | T], NewSrvDataList) ->
    IsStop = case lists:keyfind(SrvId, #server_srv_data.srv_id, NewSrvDataList) of
                 ServerSrvData = #server_srv_data{} ->
                     #server_srv{node = Node2, cookie = Cookie2} = lib_center:convert_srv(ServerSrvData),
                     case Node =:= Node2 andalso Cookie =:= Cookie2 of
                         true ->
                             svr_center:update(Pid, ServerSrvData),
                             false;
                         false ->
                             true
                     end;
                 _ ->
                     true
             end,
    case IsStop =:= true of
        true ->
            svr_center:stop(Pid),
            lib_center:delete(SrvId);
        _ -> ignore
    end,
    stop_srv(T, NewSrvDataList).

start_srv([]) -> ok;
start_srv([Ssd = #server_srv_data{srv_id = SrvId} | T]) ->
    case lib_center:get_srv(SrvId) of
        #server_srv{} ->
            ignore;
        _ ->
            do_start_srv(Ssd)
    end,
    start_srv(T);
start_srv([H | T]) ->
    ?ERR("server_srv_data err: ~w", [H]),
    start_srv(T).

%% @doc 从数据库获取所有游戏节点
-define(SQL_NODE_GET, <<"select `id`, `ip`, `port`, `name`, `cookie`, `state`, `time` from `node_info` order by `id`">>).
get_sql_srv_data() ->
    Sql = io_lib:format(?SQL_NODE_GET, []),
    case db:get_all(Sql) of
        {ok, NodeList} when NodeList =/= []->
            F = fun([Id, IpBin, Port, NameBin, CookieBin, _State, _Time], Acc) ->
                Name = util_type:to_atom(NameBin),
                Cookie = util_type:to_atom(CookieBin),
                Ip = util_type:to_list(IpBin),
                Ssd = #server_srv_data{srv_id = Id, name = Name, node = Name, port = Port, cookie = Cookie, host = Ip},
                [Ssd | Acc]
                end,
            lists:foldl(F, [], NodeList);
        _ -> []
    end.



