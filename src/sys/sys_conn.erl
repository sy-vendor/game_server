%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 连接器
%%% 负责收发数据，用来控制角色进程或其它进程
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(sys_conn).
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

-export([create/3, send/2, send2/2, send_filter/2, pack/2, pack_send/3, pack_send_directly/3, p/0, cast/1, call/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(CHECK_HEARTBEAT, 10000).

-include("common.hrl").
-include("role.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @spec create(ClientType, Socket, Ip, Port) -> {ok, Pid} | ignore | {error, Error}
%% ClientType = monitor | game | tester
%% Socket = port()
%% Ip = binary()
%% Port = int()
%% @doc 创建一个连接器
create(Socket, Ip, Port) ->
    gen_server:start(?MODULE, [Socket, Ip, Port], []).

%% @spec send(ConnPid, Bin) -> ok
%% ConnPid = pid()
%% Data = binary()
%% @doc 通知连接器发送数据
%% <div>当此函数在角色进程内执行时会自动处理发送缓冲区的操作</div>
%% <div>如果不是在角色进程内执行则会直接将数据发送到客户端</div>
send(ConnPid, Bin) ->
    case get(send_buff) of
        undefined ->
            ConnPid ! {send_data, Bin};
        [] ->
            ConnPid ! {send_data, Bin};
        [H] ->
            put(send_buff, [[Bin | H]]);
        [H | T] ->
            put(send_buff, [[Bin | H] | T])
    end.

send2(ConnPid, Bin) ->
    ConnPid ! {send_data, Bin}.

send_filter(ConnPid, Bin) ->
    ConnPid ! {sf, Bin}.

%% @spec pack(Cmd, Data) -> {ok, Bin} | {false, Reason}
%% Cmd = integer()
%% Data = tuple()
%% Bin = binary()
%% Reason = bitstring()
%% @doc 打包协议数据
pack(Cmd, Data) ->
    lib_proto:encode_msg(Cmd, Data).

%% @spec pack_send(ConnPid, Cmd, Data) -> ok
%% ConnPid = pid()
%% Cmd = int()
%% Data = tuple()
%% @doc 打包并发送消息
pack_send(ConnPid, Cmd, Data) ->
    case lib_proto:encode_msg(Cmd, Data) of
        {ok, Bin} ->
            send(ConnPid, Bin);
        Err ->
            ?ERR("打包数据出错[Cmd:~w][Err:~w]", [Cmd, Err])
    end.

%% @spec pack_send_directly(ConnPid, Cmd, Data) -> ok
%% ConnPid = pid()
%% Cmd = int()
%% Data = tuple()
%% @doc 打包并发送消息
pack_send_directly(ConnPid, Cmd, Data) ->
    case lib_proto:encode_msg(Cmd, Data) of
        {ok, Bin} ->
            ConnPid ! {send_data, Bin};
        Err ->
            ?ERR("打包数据出错[Cmd:~w][Err:~w]", [Cmd, Err])
    end.

%% @doc 进程Id
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
init([Socket, _Ip, _Port]) ->
    process_flag(trap_exit, true),
    self() ! read_next,
    State = #role{socket = Socket},
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
%% 处理SOCKET协议
%% @param:: cmd: 协议号
%%          data: 协议数据
handle_call({'SOCKET_CMD', [Cmd, Bin]}, _From, State) ->
    % 协议逻辑
    case catch handle(Cmd, State, Bin) of
        {ok, NewState} when is_record(NewState, role) ->
            {reply, ok, NewState};
        {ok, V, NewState} when is_record(NewState, role) ->
            ProtHandler = lib_proto:get_resolve_handler(Cmd),
            ProtHandler:response(V, NewState),
            {reply, ok, NewState};
        NewState when is_record(NewState, role) ->
            {reply, ok, NewState};
        {ok, NewState} ->
            ?ERROR_MSG("sys_conn receive msg error, cmd:~p badrecord:~p", [Cmd, NewState]),
            {reply, ok, State};
        {ok, V, NewState} ->
            ?ERROR_MSG("sys_conn receive msg error, cmd:~p, V:~p, badrecord:~p", [Cmd, V, NewState]),
            {reply, ok, State};
        {'EXIT', R} ->
            ?ERROR_MSG("sys_conn receive msg error, cmd:~p, reason:~p~n", [Cmd, R]),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;
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
handle_info(read_next, State) ->
    read_next(State);
%% 发送socket数据
handle_info({send_data, Bin}, Role = #role{socket = Socket}) ->
    case catch erlang:port_command(Socket, Bin) of
        true ->
            {noreply, Role};
        _Else ->
            ?ERR("Socket[~s]发送socket数据失败:~w", [Socket, _Else]),
            {noreply, Role}
    end;

%% -------------------------------------------------------------------
%% 处理socket数据读取结果
%% -------------------------------------------------------------------
%% @doc 客户端断开了连接
handle_info({inet_async, Socket, _Ref, {error, closed}}, State) ->
    ?INFO("Socket[~s]控制的目标进程退出:~w", [Socket, closed]),
    {stop, normal, State};
%% @doc 收到数据
handle_info({inet_async, _Socket, _Ref, {ok, <<Len:16, Cmd:16, Bin/binary>>}}, State) when length(Bin) == Len ->
    routing(Cmd, Bin, State);
%% @doc 收到异常数据
handle_info({inet_async, _Socket, _Ref, {ok, Bin}}, State) ->
    ?ERROR_MSG("received abnormal data :~w~n", [Bin]),
    {stop, normal, State};
%% @doc 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, _Reason}}, State) ->
    ?ERROR_MSG("读取socket[~w]数据出错:~w", [_Socket, _Reason]),
    {stop, normal, State};

%% @doc 处理socket数据发送结果
handle_info({inet_reply, _Socket, ok}, State) ->
    {noreply, State};
handle_info({inet_reply, _Socket, {error, closed}}, State) ->
    ?ERROR_MSG("发送消息时, {error, closed}"),
    {stop, normal, State};
handle_info({inet_reply, _Socket, {error, timeout}}, State) ->
    {noreply, State};
handle_info({inet_reply, _Socket, _Else}, State) ->
    ?ERROR_MSG("发送socket[~w]数据时发生了未预料的错误: ~w", [_Socket, _Else]),
    {stop, normal, State};

handle_info({'EXIT', Pid, normal}, State) ->
    ?ERROR_MSG("控制的目标进程[~w]退出:~w", [Pid, normal]),
    {stop, normal, State};

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
%% 通知连接器读取下一条指令
read_next(State = #role{socket = Socket, cmd_total_num = CmdTotalNum}) ->
    prim_inet:async_recv(Socket, 0, 60000),
    {noreply, State#role{cmd_total_num = CmdTotalNum + 1}}.

%% 路由处理
routing(Cmd, Bin, State) ->
    ProtoMod = lib_proto:get_proto_mod(Cmd),
    case lib_proto:get_msg_name(Cmd) of
        undefined ->
            ?ERROR_MSG("Cmd[~w] proto undefined~~~~~~", [Cmd]),
            read_next(State);
        MsgName when State#role.pid == undefined ->
            Data = ProtoMod:decode_msg(Bin, MsgName),
            case call({'SOCKET_CMD', [Cmd, Data]}) of
                ok -> read_next(State);
                _ ->
                    ?ERROR_MSG("sys_call hander proto cmd[~w] fail !!!", [Cmd]),
                    read_next(State)
            end;
        MsgName ->
            Data = ProtoMod:decode_msg(Bin, MsgName),
            case svr_role:call(State#role.pid, {'SOCKET_CMD', [Cmd, Data]}) of
                ok -> read_next(State);
                _ ->
                    ?ERROR_MSG("svr_call hander proto cmd[~w] fail !!!", [Cmd]),
                    read_next(State)
            end
    end.


%% 协议路由
%% @param:: cmd: 协议号
%%          Msg: 消息内容
%%data:消息体
handle(Cmd, RoleState, Msg) ->
    ProtHandler = lib_proto:get_resolve_handler(Cmd),
    ProtHandler:do(RoleState, Msg).