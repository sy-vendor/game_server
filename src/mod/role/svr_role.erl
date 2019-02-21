%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 人物角色进程
%%% @end
%%% Created : 10. 十二月 2018 18:16
%%%-------------------------------------------------------------------
-module(svr_role).
-author("suyang").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([i/1,
    p/1,
    call/2,
    cast/2,
    start/3,
    stop/1,
    stop_after/1,
    apply_call/4,
    apply_call2/4,
    apply_cast/4,
    apply_cast2/4,
    init_role_data/2,
    rpc/4,
    reconnect/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("role.hrl").
-include("common.hrl").
-include("conn.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 停止玩家进程
stop(Role) ->
    cast(Role, stop).

%% @doc 停止玩家进程(定时时间后)
stop_after(Role) ->
    cast(Role, stop_after).

%% @doc 玩家断线重连处理
reconnect(Role, Args) ->
    cast(Role, {reconnect, Args}).

%% @spec rpc(RolePid, Mod, Cmd, Data) -> ok
%% RolePid = pid()
%% Mod = atom()
%% Cmd = int()
%% Data = tuple()
%% @doc rpc调用接口(socket事件处理)
rpc(RolePid, Mod, Cmd, Data) ->
    RolePid ! {rpc, Mod, Cmd, Data}.

%% @doc CALL发送玩家消息接口
apply_call(Role, M, F, A) when is_record(Role, role) ->
    #role{pid = RolePid} = Role,
    call(RolePid, {apply_call, [M, F, A]});
apply_call(RoleId, M, F, A) when is_integer(RoleId) ->
    call(RoleId, {apply_call, [M, F, A]});
apply_call(RolePid, M, F, A) when is_pid(RolePid) ->
    call(RolePid, {apply_call, [M, F, A]});
apply_call(_, _M, _F, _A) -> ignore.

%% @doc CALL发送玩家消息接口(后带Role)
apply_call2(Role, M, F, A) when is_record(Role, role) ->
    #role{pid = RolePid} = Role,
    call(RolePid, {apply_call2, [M, F, A]});
apply_call2(RoleId, M, F, A) when is_integer(RoleId) ->
    call(RoleId, {apply_call2, [M, F, A]});
apply_call2(RolePid, M, F, A) when is_pid(RolePid) ->
    call(RolePid, {apply_call2, [M, F, A]});
apply_call2(_, _M, _F, _A) -> ignore.

%% @doc CAST发送玩家消息接口
apply_cast(Role, M, F, A) when is_record(Role, role) ->
    #role{pid = RolePid} = Role,
    cast(RolePid, {apply_cast, [M, F, A]});
apply_cast(RoleId, M, F, A) when is_integer(RoleId) ->
    cast(RoleId, {apply_cast, [M, F, A]});
apply_cast(RolePid, M, F, A) when is_pid(RolePid) ->
    cast(RolePid, {apply_cast, [M, F, A]});
apply_cast(_, _M, _F, _A) -> ignore.

%% @doc CAST发送玩家消息接口(Role)
apply_cast2(Role, M, F, A) when is_record(Role, role) ->
    #role{pid = RolePid} = Role,
    cast(RolePid, {apply_cast2, [M, F, A]});
apply_cast2(RoleId, M, F, A) when is_integer(RoleId) ->
    cast(RoleId, {apply_cast2, [M, F, A]});
apply_cast2(RolePid, M, F, A) when is_pid(RolePid) ->
    cast(RolePid, {apply_cast2, [M, F, A]});
apply_cast2(_, _M, _F, _A) -> ignore.

%% @doc 玩家初始化数据
init_role_data(Role, Args) ->
    cast(Role, {init_role_data, Args}).

%% @doc 进程信息
i(RoleId) ->
    call(RoleId, info).

%% @doc 进程ID
p(RoleId) ->
    get_role_pid(RoleId).

%% @doc call函数
call(Pid, Msg) when is_pid(Pid) ->
    case is_pid(Pid) of
        true -> gen_server:call(Pid, Msg);
        false -> ignore
    end;
call(RoleId, Msg) ->
    case p(RoleId) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _R ->
            {error, server_not_start}
    end.

%% @doc cast函数
cast(Pid, Msg) when is_pid(Pid) ->
    case is_pid(Pid) of
        true -> gen_server:cast(Pid, Msg);
        false -> ignore
    end;
cast(RoleId, Msg) ->
    case p(RoleId) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        _ -> ignore
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%% @doc 启动玩家进程
start(RoleId, Account, ConnPid) ->
    RegName = util_dist:role_process_name(RoleId),
    gen_server:start({global, RegName}, ?MODULE, [RoleId, Account, ConnPid], []).

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
init([RoleId, Account, ConnPid]) ->
    process_flag(priority, high),
    self() ! gc,
    put(conn_pid, ConnPid),
    lib_configure:init_data(),
    State = #role{role_id = RoleId, last_login_time = util_time:unixtime(), account = Account},
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
%% CAST到进程执行函数
handle_call({apply_call, [Module, Fun, Args]}, _From, State) ->
    Res = case catch apply(Module, Fun, Args) of
              {'EXIT', Info} ->
                  ?ERROR_MSG("svr_role_call apply_call error ~p~n", [[Module, Fun, Args, Info]]),
                  {error, Info};
              Ret ->
                  Ret
          end,
    {reply, Res, State};
%% CAST到进程执行函数（带STATE参数）
handle_call({apply_call2, [Module, Fun, Args]}, _From, State) ->
    {Res, NewState} = case catch apply(Module, Fun, Args ++ [State]) of
                          {'EXIT', _Info} ->
                              {error, State};
                          NewStateT when is_record(NewStateT, role) ->
                              {ok, NewStateT};
                          {ok, NewStateT} when is_record(NewStateT, role) ->
                              {ok, NewStateT};
                          {Ret, NewStateT} when is_record(NewStateT, role) ->
                              {Ret, NewStateT};
                          {ok, Ret, NewStateT} when is_record(NewStateT, role) ->
                              {Ret, NewStateT};
                          ok ->
                              {ok, State};
                          {false, ErrCode} ->
                              {{false, ErrCode}, State};
                          _R ->
                              {ignore, State}
                      end,
    {reply, Res, NewState};
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
%% CAST到进程执行函数
handle_cast({apply_cast, [Module, Fun, Args]}, State) ->
    case catch apply(Module, Fun, Args) of
        {'EXIT', Info} ->
            ?ERROR_MSG("svr_role_cast apply_cast error ~p~n", [[Module, Fun, Args, Info]]);
        _ -> ok
    end,
    {noreply, State};
%% CAST到进程执行函数（带STATE参数）
handle_cast({apply_cast2, [Module, Fun, Args]}, State) ->
    StateN = case catch apply(Module, Fun, Args ++ [State]) of
                 {'EXIT', Info} ->
                     ?ERROR_MSG("svr_role_cast apply_cast2 error ~p~n", [[Module, Fun, Args, Info]]),
                     State;
                 NewStateT when is_record(NewStateT, role) ->
                     NewStateT;
                 {_, NewStateT} when is_record(NewStateT, role) ->
                     NewStateT;
                 _ ->
                     State
             end,
    {noreply, StateN};
%% 初始化模块数据
handle_cast({init_role_data, [RoleId, Ip, Socket, RoleSid]}, State) ->
    StateN0 = State#role{role_id = RoleId, pid = self(), sid = RoleSid, ip = Ip, socket = Socket},
    {ok, StateN} = lib_role_api:init_role_data(StateN0),
    {noreply, StateN};
%% 发送二进制消息
handle_cast({send, BinData}, State) ->
    #role{sid = RoleSid} = State,
    lib_send:send_to_sid(RoleSid, BinData),
    {noreply, State};
%% 玩家每日数据重置处理
handle_cast({daily_reset}, State) ->
    % 实时日清处理
    StateN = lib_role_api:daily_reset(State),
    {noreply, StateN};
%% 定时结束进程
handle_cast(stop_after, State) ->
    #role{pid = RolePid} = State,
    case catch lib_role_api:logout_after(State) of
        {'EXIT', R} ->
            ?ERROR_MSG("logout after stop error:~p~n", [R]),
            svr_role:stop(RolePid),
            {noreply, State};
        StateN ->
            erlang:send_after(?INTERVAL_STOP_SPAN, self(), stop),
            {noreply, StateN}
    end;
%% 停止游戏进程
handle_cast(stop, State) ->
    case catch lib_role_api:logout(State) of
        {'EXIT', R} ->
            ?ERROR_MSG("logout cast stop error:~p~n", [R]);
        _ ->
            ignore
    end,
    {stop, normal, State};
%% 断线重连
handle_cast({reconnect, [Ip, Socket, RoleSidN, ConnPid]}, State) ->
    % 重置客户端连接数据
    StateN0 = State#role{sid = RoleSidN, ip = Ip, socket = Socket},
    NowTime = util_time:unixtime(),
    StateN = lib_event:role_login(false, NowTime, StateN0),
    put(conn_pid, ConnPid),
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
%% 处理协议
handle_info({rpc, Mod, Cmd, Data}, State) ->
    handle_rpc({rpc, Mod, Cmd, Data}, State);
%% 玩家秒循环数据
handle_info({second_timer}, State) ->
    #role{
        role_id = _RoleId, sid = _RoleSid,
        tick_time = #tick_time{three_minute = ThreeMinute} = TickTime
    } = State,
%%    NowTimeMs = util_time:unixtime(ms),
%%    NowTime = NowTimeMs div 1000,
    % 每秒必做逻辑
    StateN1 = lib_merge_api:check_role_asset(State),
%%    lib_speed_api:loop_speed_up(RoleId),
    % 检查订单系统
    lib_order_api:loop_order(StateN1),
    % 三分钟必做逻辑
    {ThreeMinuteN, StateN33} = case ThreeMinute =:= 180 of
                              false ->
                                  {ThreeMinute + 1, StateN1};
                              true ->
                                  lib_order_api:production_order(StateN1),
                                  StateN3 = StateN1,
                                  {1, StateN3}
                          end,
    % 更新定时计算
    TickTimeN = TickTime#tick_time{three_minute = ThreeMinuteN},
    StateN = StateN33#role{tick_time = TickTimeN},
    % 重置定时器
    lib_role_api:start_second_timer(),
    {noreply, StateN};
%% 强制GC
handle_info(gc, State) ->
    garbage_collect(),
    erlang:send_after(180000, self(), gc),
    {noreply, State};
%% 停止游戏进程
handle_info(stop, State) ->
    case catch lib_role_api:logout(State) of
        {'EXIT', R} ->
            ?ERROR_MSG("logout cast stop error:~p~n", [R]);
        _ ->
            ignore
    end,
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
%% 获取玩家进程PID
get_role_pid(RoleId) ->
    util_dist:get_role_process_pid(RoleId).

%% 处理协议rpc
handle_rpc({rpc, Mod, Cmd, Data}, State = #role{role_name = Name}) ->
    case catch Mod:handle(Cmd, Data, State) of
        {ok} -> read_next(State);
        {ok, NewState} when is_record(NewState, role) -> read_next(NewState);
        _Reason ->
            ?ERROR_MSG("handle rpc err,Cmd:~w, Name:~w, _Reason:~w", [Cmd, Name, _Reason]),
            read_next(State)
    end.

%% 通知连接器读取下一条指令
read_next(State) ->
    get(conn_pid) ! read_next,
    {noreply, State}.
