%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 测试进程
%%% @end
%%% Created : 11. 十二月 2018 10:20
%%%-------------------------------------------------------------------
-module(tester).
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

-export([start/5,
    stop/1,
    stop_all/0,
    apply/3,
    apply2/3,
    apply_after/3,
    cmd/3,
    pack_send/3,
    all_tester/0,
    pack_send/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("tester.hrl").
-include("conn.hrl").
-include("common.hrl").
-include("logic.hrl").

-define(TCP_OPTS, [binary, {packet, 0}, {nodelay, false}, {delay_send, true}, {exit_on_close, false}]).

%%%===================================================================
%%% API
%%%===================================================================
%% @spec start(Account, ProtoMod, Host, Port) -> ok
%% Account = string() |iolist()
%% Host = string()
%% Port = integer()
%% @doc 创建测试进程
start(Account, Name, ProtoMod, Host, Port) when is_list(Account) ->
    start(list_to_binary(Account), list_to_binary(Name), ProtoMod, Host, Port);
start(Account, Name, ProtoMod, Host, Port) ->
    gen_server:start(?MODULE, [Account, Name, ProtoMod, Host, Port], []).

%% @spec stop(Pid) -> ok
%% Pid = pid() | integer()
%% @doc 退出
stop(Rid) when is_integer(Rid) ->
    case ets:lookup(tester_online, Rid) of
        [T] ->
            stop(T#tester.pid);
        _ ->
            ignore
    end;
stop(Pid) ->
    Pid ! logout.

stop_all() ->
    [stop(Pid) || #tester{pid = Pid} <- ets:tab2list(tester_online)].

apply(async, Pid, {M, F, A}) ->
    Pid ! {apply_async, M, F, A}.

apply2(async, Pid, {F, A}) ->
    Pid ! {apply_async, F, A}.

apply_after(Ms, Pid, {M, F, A}) ->
    erlang:send_after(Ms, Pid, {apply_async, M, F, A}).

cmd(Pid, Cmd, Data) ->
    Pid ! {cmd, Cmd, Data}.

%% @spec pack_send(Cmd, Data)-> ok
%% Cmd = integer()
%% Data = tuple()
%% @doc 打包并发送消息到服务器
pack_send(Pid, Cmd, Data) ->
    Pid ! {pack_send, Cmd, Data}.

%% 返回所有的模拟客户端
all_tester() ->
    ets:tab2list(tester_online).

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
init([Account, Name, Mod, Host, Port]) ->
    case gen_tcp:connect(Host, Port, [
        binary,
        {packet, 0},
        {active, false},
        {reuseaddr, true},
        {nodelay, false},
        {delay_send, true}
    ]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, ?CLIENT_GAME),
            {Mod1, Mod2} = test_mod(Mod),
            State = #tester{acc_name = Account, socket = Socket, pid = self(), connect_time = util_time:get_now(), test_mod = Mod1, test_mod2 = Mod2, name = Name},
            ets:insert(tester_online, State),
            put(socket, Socket),
            put(seq, 0),
            put(is_robot, true),
            put(acc_name, Account), %% 临时保存
            self() ! heartbeat, %% 开始心跳
            erlang:send_after(10000, self(), test_proto),
            {ok, State};
        _R ->
            {stop, normal}
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
%% 循环工作
handle_info(work, State = #tester{test_mod = undefined}) ->
    {noreply, State};
handle_info(work, State) ->
    do_work(State);
%% 异步请求
handle_info({apply_async, M, F, A}, State) ->
    handle_async_apply(M, F, A, State);
handle_info({apply_async, F, A}, State = #tester{test_mod = M}) ->
    handle_async_apply(M, F, A, State);
%% 发送数据
handle_info({pack_send, Cmd, Data}, State) ->
    pack_send(Cmd, Data),
    {noreply, State};
handle_info({cmd, Cmd, Data}, State) ->
    Mod = get_tester_mod(Cmd, State),
    handle(Mod, Cmd, Data, State);
%% 客户端断开了连接
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State) ->
    ?DEBUG("socket close: ~w", [_Socket]),
    {stop, normal, State};
%% 收到正常数据
handle_info({inet_async, _Socket, _Ref, {ok, Data}}, State = #tester{name = _Name, read_bin = true}) ->
    handle_data(Data, State);
%%    {_L, Cmd, Bin} = lib_proto:read_bin(Packet),
%%    handle_cmd(Cmd, Bin, State);
%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, _Reason}}, State = #tester{acc_name = Account}) ->
    ?ERR("account[~s]read socket data err:~w", [Account, _Reason]),
    {stop, normal, State};
%% 退出
handle_info(logout, State) ->
    {stop, normal, State};
%% 心跳包
handle_info(heartbeat, State) ->
    erlang:send_after(2000, self(), heartbeat),
    {noreply, State};
handle_info(test_proto, State = #tester{pid = Pid}) ->
    rt_login:info(Pid),
    rt_login:info(Pid),
    rt_login:info(Pid),
    rt_login:info(Pid),
    {noreply, State};
handle_info(_Info, State) ->
    ?DEBUG("received unknown message: ~w", [_Info]),
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
terminate(_Reason, State) ->
    ?INFO("~s pid logout", [State#tester.acc_name]),
    catch (ets:delete(tester_online, State#tester.pid)),
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
handle_cmd(Cmd, Bin, State) ->
    case mapping:module(game_server, Cmd) of
        {ok, _, _, Proto, _Mod} ->
            MsgName = get_name(Cmd),
            Data = Proto:decode_msg(Bin, MsgName),
            Mod = get_tester_mod(Cmd, State),
            handle(Mod, Cmd, Data, State);
        _Other ->
            ?ERR("unknow command: ~w", [Cmd]),
            read_next(State)
    end.

get_name(10002) -> 'HeartRes';
get_name(10004) -> 'LoginRes';
get_name(11002) -> 'RoleInfoRes';
get_name(11003) -> 'RoleAssetChangeNotify';
get_name(11004) -> 'RoleAssetChangeRes';
get_name(11008) -> 'OfflineRewardRes';
get_name(11102) -> 'ProduceNewRes';
get_name(11104) -> 'MergeExchangeRes';
get_name(11106) -> 'RecoveryObjectRes';
get_name(11107) -> 'ObjectProduceAssetRes';
get_name(11502) -> 'OrderInfoRes';
get_name(11602) -> 'WaiterInfoRes';
get_name(11605) -> 'WaiterIntimateRes';
get_name(11702) -> 'SpeedUpStateRes';
get_name(11202) -> 'SignInfoRes';
get_name(11111) -> 'BuyObjectRes';
get_name(_) -> undifiend.

%% 获取tester模块
get_tester_mod(_, #tester{test_mod = _TestMod}) ->
    rt_login.

handle(undefined, _Cmd, _Data, State = #tester{acc_name = _Account}) ->
    read_next(State);
handle(Mod, Cmd, Data, State = #tester{acc_name = Account, name = Name}) ->
    case catch Mod:handle(Cmd, Data, State) of
        {ok} ->
            read_next(State#tester{read_bin = false});
        {ok, NewState} when is_record(NewState, tester) ->
            ets:insert(tester_online, NewState),
            read_next(NewState#tester{read_bin = false});
        {stop} ->
            {stop, normal, State#tester{read_bin = false}};
        {error, _Reason} ->
            ?ERR("handle command err[Account:~s Name:~s Mod:~w Cmd:~w Why:~w]: ~w", [Account, Name, Mod, Cmd, _Reason, Data]),
            read_next(State#tester{read_bin = false});
        _Reason ->
            ?ERR("handle command unknow err[Account:~s  Name:~s Mod:~w Cmd:~w]: ~w", [Account, Name, Mod, Cmd, _Reason]),
            read_next(State#tester{read_bin = false})
    end.

%% 通知连接器读取下一条指令
read_next(State = #tester{socket = Socket, read_bin = false}) ->
    prim_inet:async_recv(Socket, 0, -1),
    {noreply, State#tester{read_bin = true}};
read_next(State) ->
    %% 上一个数据包还未读取完成，忽略掉
    {noreply, State}.

pack_send(Cmd, Data) ->
    case get(is_robot) of
        true ->
            Socket = get(socket),
            case lib_proto:pack(Cmd, Data) of
                {ok, Bin} ->
                    gen_tcp:send(Socket, Bin);
                {error, Reason} ->
                    ?ERR("pack data err[Reason:~w]", [Reason])
            end;
        _ ->
            ?ERR("not robot process", [])
    end.
do_work(State = #tester{work_index = Index, test_mod = Tmod}) ->
    case Tmod:get_work(Index) of
        false ->
            {noreply, State};
        {ok, M, F, A, Interval} ->
            Nstate = case catch (erlang:apply(M, F, [State | A])) of
                         {ok} -> State;
                         {ok, NewState} -> ets:insert(tester_online, NewState), NewState;
                         Else -> ?ERR("handle robat data err[~w]", [Else]), State
                     end,
            case Interval == 0 of
                true -> do_work(Nstate);
                false ->
                    erlang:send_after(Interval, self(), work),
                    {noreply, Nstate}
            end
    end.

test_mod(Mod) ->
    {Mod, undefined}.

handle_async_apply(undefined, _, _, State) -> {noreply, State};
handle_async_apply(M, F, A, State) ->
    case catch erlang:apply(M, F, [State | A]) of
        {ok} ->
            {noreply, State};
        {ok, NewState} when is_record(NewState, tester) ->
            ets:insert(tester_online, NewState),
            {noreply, NewState};
        Else ->
            ?ERR("tester handle_async_apply fail(~w)", [Else]),
            {noreply, State}
    end.

%% @doc 数据处理
handle_data(BinData, State) ->
    case lib_proto:decode_data(BinData) of
        {incomplete} -> {noreply, State};
        {_, UnmaskedData, <<>>} ->
            {L, Cmd, Data} = lib_proto:read_bin(UnmaskedData),
            ?DEBUG("============Cmd:~p, Data:~w", [Cmd, Data]),
            case L =< 0 of
                false -> handle_cmd(Cmd, Data, State);
                true -> handle_cmd(Cmd, <<>>, State)
            end;
        {_, UnmaskedData, Extra} ->
            ?DEBUG("============UnmaskedData:~w", [UnmaskedData]),
            {L, Cmd, Data} = lib_proto:read_bin(UnmaskedData),
            ?DEBUG("============Cmd:~p, Data:~w", [Cmd, Data]),
            {noreply, StateN} =
                case L =< 0 of
                    false -> handle_cmd(Cmd, Data, State);
                    true -> handle_cmd(Cmd, <<>>, State)
                end,
            handle_data(Extra, StateN);
        {unknown, OpCode, _, Extra} ->
            ?ERROR_MSG("Unknown packet OpCode:~p Extra:~w", [OpCode, Extra]),
            {noreply, State}
    end.