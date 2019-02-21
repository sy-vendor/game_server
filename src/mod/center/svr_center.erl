%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2018 14:26
%%%-------------------------------------------------------------------
-module(svr_center).
-author("suyang").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([stop/1,
    update/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(RECONNECt_TIME, 60000).   %% 重新连接的时间（毫秒）

-include("center.hrl").
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
% @spec stop(Pid) ->
%% Pid = pid()
%% 关闭center_mirror
stop(Pid) ->
    Pid ! stop.

%% @spec update(Pid, ServerSrvData) ->
%% Pid = pid()
%% ServerSrvData = #server_srv_data{}
%% 更新数据
update(Pid, ServerSrvData) ->
    Pid ! {update, ServerSrvData}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%% @spec start_link(Mirror) -> {ok, Pid} | {error, Reason}
%% Mirror = #mirror{}
%% @doc 创建镜像服务器进程
start_link(ServerSrv = #server_srv{}) ->
    gen_server:start_link(?MODULE, [ServerSrv], []);
start_link(ServerSrvData = #server_srv_data{}) ->
    ServerSrv = lib_center:convert_srv(ServerSrvData),
    gen_server:start_link(?MODULE, [ServerSrv], []).

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
init([Ss = #server_srv{node = Node, cookie = Cookie}]) ->
    erlang:set_cookie(Node, Cookie),
    TimerRef = erlang:send_after(10000, self(), {check_online, 0}),
    NewSs = Ss#server_srv{pid = self(), timer_ref = TimerRef},
    lib_center:save(NewSs),
    {ok, NewSs}.

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
%% 定时检查远端服务器是否上线
handle_info({check_online, N}, State = #server_srv{srv_id = SrvId, node = Node, mpid = undefined}) ->
    case N > 0 of
        true -> ?INFO("~w [~w] unable to connect，try ~w num，reconnect....", [SrvId, Node, N]);
        false -> ignore
    end,
    rpc:cast(Node, svr_server, ready, [svr_server:get_node_type(), node(), self()]),
    TimerRef = erlang:send_after(?RECONNECt_TIME, self(), {check_online, N+1}),
    NewState = State#server_srv{timer_ref = TimerRef},
    lib_center:save(NewState),
    {noreply, NewState};
%% 远端服务器已连接
handle_info({ready, Pid, _SrvIds}, State = #server_srv{srv_id = SrvId, node = Node, timer_ref = TimerRef}) ->
    ?INFO("has been with the server [~w] establish connection", [Node]),
    Mref = erlang:monitor(process, Pid),
    erlang:cancel_timer(TimerRef),
    NewState = State#server_srv{mpid = Pid, mref = Mref, timer_ref = undefined},
    lib_center:save(NewState),
    sync_to_srv(svr_server:get_node_type(), SrvId),
    {noreply, NewState};
handle_info({center_exists, _N}, State) ->
    {noreply, State};
%% 远端服务器已断开连接
handle_info({'DOWN', Mref, _Type, _Object, _Reason} = R, State = #server_srv{node = Node, mref = Mref}) ->
    ?INFO("server [~w] connection disconnected: ~w", [Node, R]),
    TimerRef = erlang:send_after(?RECONNECt_TIME, self(), {check_online, 0}),
    NewState = State#server_srv{mpid = undefined, mref = undefined, timer_ref = TimerRef},
    lib_center:save(NewState),
    demonitor(Mref),
    {noreply, NewState};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({update, ServerSrvData}, State = #server_srv{srv_id = SrvId}) ->
    ?INFO("[~w] mirror process receives updates: ~w", [SrvId, ServerSrvData]),
    #server_srv{name = Name, host = Host} = lib_center:convert_srv(ServerSrvData),
    {noreply, State#server_srv{name = Name, host = Host}};
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
terminate(Reason, #server_srv{srv_id = SrvId, node = Node}) ->
    case Reason of
        normal -> ?INFO("connection to [~w, ~w] mirror process has exited normally", [SrvId, Node]);
        _ -> ?ERROR_MSG("connection to [~w, ~w]mirror process has exited abnormally:~w", [SrvId, Node, Reason])
    end,
    lib_center:delete(self()),
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
%% 逻辑节点连上中央节点时，从中央节点同步数据到逻辑节点处理
sync_to_srv(_, _) ->
    ok.