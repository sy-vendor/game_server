%%%-------------------------------------------------------------------
%%% @author Su Yang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 连接器
%%% @end
%%% Created : 09. 十二月 2018 17:26
%%%-------------------------------------------------------------------
-module(sys_conn).
-author("Su Yang").

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

-export([create/3,
    send/2,
    send2/2,
    send_filter/2,
    pack/2,
    pack_send/3,
    pack_send_directly/3,
    routing/3,
    read_next/1,
    stop/0]).

-define(SERVER, ?MODULE).

-record(state, {}).
-include("common.hrl").
-include("conn.hrl").
-include("logic.hrl").

%% 记录所有协议请求
-define(DO_SEND_RECORD(Bin), do_record_send(Bin)).
-define(DO_RECV_RECORD(Cmd, Len), sys_conn_mgr:record(recv, Cmd, Len)).

-define(check_heartbeat_inteval, 10000).

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

%% @spec send(ConnPid, Bin) -> ok
%% ConnPid = pid()
%% Data = binary()
%% @doc 通知连接器发送数据
%% <div>当此函数在角色进程内执行时会自动处理发送缓冲区的操作</div>
%% <div>如果不是在角色进程内执行则会直接将数据发送到客户端</div>
send(ConnPid, Bin) ->
    case get(send_record) of
        undefined ->
            ConnPid ! {send_data, Bin};
        [] ->
            ConnPid ! {send_data, Bin};
        [H] ->
            put(send_record, [[Bin | H]]);
        [H | T] ->
            put(send_record, [[Bin | H] | T])
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
    lib_proto:pack(Cmd, Data).

%% @spec pack_send(ConnPid, Cmd, Data) -> ok
%% ConnPid = pid()
%% Cmd = int()
%% Data = tuple()
%% @doc 打包并发送消息
pack_send(ConnPid, Cmd, Data) ->
    case lib_proto:pack(Cmd, Data) of
        {ok, Bin} ->
            send(ConnPid, Bin);
        Err ->
            ?ERROR_MSG("pack proto err[Cmd:~w][Err:~w]", [Cmd, Err])
    end.

%% @spec pack_send_directly(ConnPid, Cmd, Data) -> ok
%% ConnPid = pid()
%% Cmd = int()
%% Data = tuple()
%% @doc 打包并发送消息
pack_send_directly(ConnPid, Cmd, Data) ->
    case lib_proto:pack(Cmd, Data) of
        {ok, Bin} ->
            ConnPid ! {send_data, Bin};
        Err ->
            ?ERROR_MSG("pack proto err[Cmd:~w][Err:~w]", [Cmd, Err])
    end.

%% @doc 关闭进程
stop() ->
    case get_pid() of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, stop);
        _ ->
            ignore
    end.

%% @doc 进程Pid
get_pid() ->
    util_dist:whereis_name(local, ?MODULE).
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
init([Socket, Ip, Port]) ->
    process_flag(trap_exit, true),
    self() ! read_next,
    erlang:send_after(?check_heartbeat_inteval * 10, self(), check_heartbeat), %% 检查心跳
    State = #conn{socket = Socket, ip = Ip, port = Port, connect_time = util_time:unixtime()},
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
handle_cast(stop, State) ->
    {stop, normal, State};
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
%% 心跳检查
handle_info(check_heartbeat, State = #conn{account = Account, last_hb = LastHb}) ->
    %% 如果最后一次心跳距离当前时刻大于10秒，判定为已掉线
    Now = util_time:unixtime(),
    Diff = Now - LastHb,
    case Diff > ?check_heartbeat_inteval of
        true ->
            ?ERROR_MSG("account [~s] client heartbeat timeout: now=~w, last_heartbeat=~w, diff=~w]", [Account, Now, LastHb, Diff]),
%%            {noreply, State};
            {stop, normal, State};
        false ->
            erlang:send_after(?check_heartbeat_inteval, self(), check_heartbeat),
            {noreply, State}
    end;
%% 发送socket数据
handle_info({send_data, Bin}, State = #conn{account = Account, socket = Socket, send_count = SendCount, error_send = ErrSend}) ->
    case catch erlang:port_command(Socket, Bin) of
        true ->
            ?DO_SEND_RECORD(Bin),
            {noreply, State#conn{send_count = SendCount + 1}};
        false ->
            ?ERROR_MSG("[~s] send data port_command fail", [Account]),
            {noreply, State#conn{error_send = ErrSend + 1}};
        _Else ->
            ?ERROR_MSG("[~s]send socket data fail:~w", [Account, _Else]),
            {noreply, State#conn{error_send = ErrSend + 1}}
    end;
%% 客户端断开了连接
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State = #conn{account = _Account}) ->
    {stop, normal, State};
handle_info({inet_async, _Socket, _Ref, {ok, Bin}}, State = #conn{read_bin = true}) ->
    lib_proto:handle_data(Bin, State);
%% 收到异常数据
handle_info({inet_async, Socket, _Ref, {ok, Bin}}, State = #conn{account = Account, ip = Ip, socket = Socket, bad_req_count = BadReq, seq = Seq, read_bin = ReadBin}) ->
    ?ERROR_MSG("client[Acc:~s IP:~p]send invalid request: ~w, ~w, read_head=~w, bad_req=~w", [Account, Ip, Seq, Bin, ReadBin, BadReq]),
    case BadReq > 10 of
        false ->
            {noreply, State#conn{bad_req_count = BadReq + 1, read_bin = false}};
        true ->
            {stop, normal, State}
    end;
%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, timeout}}, State) ->
    {stop, normal, State};
%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, _Reason}}, State = #conn{account = Account}) ->
    ?ERROR_MSG("[~s]pack socket data err:~w", [Account, _Reason]),
    {stop, normal, State};
%% 处理socket数据发送结果
handle_info({inet_reply, _Socket, ok}, State) ->
    {noreply, State};
handle_info({inet_reply, _Socket, {error, closed}}, State = #conn{account = _Account}) ->
    {stop, normal, State};
handle_info({inet_reply, _Socket, {error, timeout}}, State = #conn{error_send = ErrSend}) ->
    {noreply, State#conn{error_send = ErrSend + 1}};
handle_info({inet_reply, _Socket, _Else}, State = #conn{account = _Account}) ->
    ?ERROR_MSG("[~s] send socket data err: ~w", [_Account, _Else]),
    {stop, normal, State};
%% 处理关联进程异常退出
handle_info({'EXIT', Pid, _Why}, State = #conn{account = _Account, pid_object = ObjectPid}) when Pid =:= ObjectPid ->
    ?ERROR_MSG("account:~w, process pid:~w[Pid] abnormal exit _why:~w", [_Account, Pid, _Why]),
    {stop, normal, State};
handle_info({'EXIT', _Pid, _Why}, State) ->
    {stop, {shutdown, reconnect}, State};
handle_info(stop, State) ->
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
terminate({shutdown, reconnect}, _State) ->
    ok;
terminate(_Reason, #conn{account = _Account, socket = Socket, object = Object, pid_object = ObjectPid}) ->
    catch Object:stop_after(ObjectPid),
    gen_tcp:close(Socket),
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
%% @doc 路由处理
routing(Cmd, Bin, State = #conn{account = Account, recv_count = RecvCount, bad_req_count = BadReq}) ->
    case mapping:module(game_server, Cmd) of
        %% 处理需要验证的模块
        {ok, true, Caller, Proto, Mod} when Account =/= <<>> ->
            call(Caller, Proto, Mod, Cmd, Bin, State#conn{recv_count = RecvCount + 1});
        {ok, false, Caller, Proto, Mod} ->
            call(Caller, Proto, Mod, Cmd, Bin, State#conn{recv_count = RecvCount + 1});
        _Else ->
            ?ERROR_MSG("account [~s] send invalid request[~w], _Else:~w", [Account, Cmd, _Else]),
            read_next(State#conn{bad_req_count = BadReq + 1})
    end.

%% @doc 通知连接器读取下一条指令
read_next(State = #conn{socket = Socket, recv_count = RecvCount}) ->
    prim_inet:async_recv(Socket, 0, 60000),
    {noreply, State#conn{recv_count = RecvCount + 1, read_bin = true}};
read_next(State) ->
    ?ERROR_MSG("this is proto unread Cmd:~p", [State#conn.cmd]),
    %% 上一个数据包还未读取完成，忽略掉
    {noreply, State}.

call(Caller, Proto, Mod, Cmd, Bin, State) ->
    case mapping:get(Cmd) of
        undefined ->
            ?ERROR_MSG("mapping get err[Proto:~w, Mod:~w, Cmd:~w]", [Proto, Mod, Cmd]),
            read_next(State);
        MsgName ->
            call_next(Caller, Proto, Mod, Cmd, Bin, State, MsgName)
    end.

call_next(Caller, Proto, Mod, Cmd, Bin, State, MsgName) ->
    Data = Proto:decode_msg(Bin, MsgName),
    do_call(Caller, Mod, Cmd, Data, State).

%% @doc 由connector执行调用
do_call(connector, Mod, Cmd, Data, State = #conn{account = Account, recv_count = RecvCount}) ->
    case catch Mod:handle(Cmd, Data, State#conn{recv_count = RecvCount + 1}) of
        {ok} ->
            read_next(State);
        {ok, NewState} when is_record(NewState, conn) ->
            read_next(NewState);
        {reply, Reply} ->
            pack_send(self(), Cmd, Reply),
            read_next(State);
        {reply, Reply, NewState} when is_record(NewState, conn) ->
            pack_send(self(), Cmd, Reply),
            read_next(NewState);
        {stop, _Reason} ->
            ?ERROR_MSG("handle proto[~w] return fail,exit sys_conn: ~s", [Cmd, _Reason]),
            {stop, normal, State};
        {error, _Reason} ->
            ?ERROR_MSG("account [~s] connector handle err:~w [Cmd:~w Data:~w]", [Account, _Reason, Cmd, Data]),
            read_next(State);
        _Reason ->
            ?ERROR_MSG("account [~s] connector handle unknow err[Mod:~w Cmd:~w Data:~w Reason:~w]", [Account, Mod, Cmd, Data, _Reason]),
            read_next(State)
    end;

%% 过滤无效请求
do_call(object, _Mod, _Cmd, _Data, State = #conn{pid_object = undefined}) ->
    read_next(State);
do_call(object, _Mod, _Cmd, _Data, State = #conn{object = undefined}) ->
    read_next(State);

%% 由object执行调用
do_call(object, Mod, Cmd, Data, State = #conn{object = Object, pid_object = ObjectPid}) ->
    Object:rpc(ObjectPid, Mod, Cmd, Data),
    {noreply, State};

%% 错误的调用发起者
do_call(Caller, Mod, Cmd, _Data, State) ->
    ?ERROR_MSG("no support caller operation[Caller:~w, Mod:~w, Cmd:~w]", [Caller, Mod, Cmd]),
    read_next(State).

do_record_send(Bin) ->
    case is_list(Bin) of
        true -> [do_record_send(B) || B <- Bin];
        false ->
            <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, _Len:7, _L:32, C:16, _B/binary>> = Bin,
            sys_conn_mgr:record(send, C, byte_size(Bin))
    end.
