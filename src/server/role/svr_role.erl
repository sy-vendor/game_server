%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 玩家处理进程
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_role).
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

-export([start/1, stop/1, stop_after/1, apply_call/4, apply_call2/4, apply_cast/4, apply_cast2/4, reconnect/2, init_role_data/2,
    get_data/2, i/1, p/1, call/2, cast/2, send_after/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("role.hrl").
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 启动玩家进程
start(RoleId) ->
    RegName = dist:role_process_name(RoleId),
    gen_server:start({global, RegName}, ?MODULE, [RoleId], []).

%% @doc 停止玩家进程
stop(Role) ->
    cast(Role, stop).

%% @doc 停止玩家进程(定时时间后)
stop_after(Role) ->
    cast(Role, stop_after).

%% @doc CALL发送玩家消息接口
apply_call(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{pid = RolePid} = RoleState,
    call(RolePid, {apply_call, [M, F, A]});
apply_call(RoleId, M, F, A) when is_integer(RoleId) ->
    call(RoleId, {apply_call, [M, F, A]});
apply_call(RolePid, M, F, A) when is_pid(RolePid) ->
    call(RolePid, {apply_call, [M, F, A]});
apply_call(_, _M, _F, _A) -> ignore.

%% @doc CALL发送玩家消息接口(后带RoleState)
apply_call2(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{pid = RolePid} = RoleState,
    call(RolePid, {apply_call2, [M, F, A]});
apply_call2(RoleId, M, F, A) when is_integer(RoleId) ->
    call(RoleId, {apply_call2, [M, F, A]});
apply_call2(RolePid, M, F, A) when is_pid(RolePid) ->
    call(RolePid, {apply_call2, [M, F, A]});
apply_call2(_, _M, _F, _A) -> ignore.

%% @doc CAST发送玩家消息接口
apply_cast(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{pid = RolePid} = RoleState,
    cast(RolePid, {apply_cast, [M, F, A]});
apply_cast(RoleId, M, F, A) when is_integer(RoleId) ->
    cast(RoleId, {apply_cast, [M, F, A]});
apply_cast(RolePid, M, F, A) when is_pid(RolePid) ->
    cast(RolePid, {apply_cast, [M, F, A]});
apply_cast(_, _M, _F, _A) -> ignore.

%% @doc CAST发送玩家消息接口(后带RoleState)
apply_cast2(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{pid = RolePid} = RoleState,
    cast(RolePid, {apply_cast2, [M, F, A]});
apply_cast2(RoleId, M, F, A) when is_integer(RoleId) ->
    cast(RoleId, {apply_cast2, [M, F, A]});
apply_cast2(RolePid, M, F, A) when is_pid(RolePid) ->
    cast(RolePid, {apply_cast2, [M, F, A]});
apply_cast2(_, _M, _F, _A) -> ignore.

%% @doc 玩家断线重连处理
reconnect(Role, Args) ->
    cast(Role, {reconnect, Args}).

%% @doc 玩家初始化数据
init_role_data(Role, Args) ->
    cast(Role, {init_role_data, Args}).

%% @doc 获取玩家数据
get_data(Role, TypeList) ->
    call(Role, {get_data, [TypeList]}).

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
            {error, 250}
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

%% @doc send_after函数
send_after(RoleNode, Role, DelayTime, Msg) when RoleNode =/= none, RoleNode =/= 0, RoleNode =/= undefined ->
    lib_kfcenter:apply_cast(RoleNode, svr_role, send_after, [Role, DelayTime, Msg]);
send_after(_RoleNode, Role, DelayTime, Msg) ->
    send_after(Role, DelayTime, Msg).
send_after(Role, DelayTime, Msg) ->
    cast(Role, {send_after, [DelayTime, Msg]}).
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
init([RoleId]) ->
    process_flag(priority, high),
    {ok, #role_state{role_id = RoleId}}.

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
%% CALL到进程执行函数
handle_call({apply_call, [Module, Fun, Args]}, _From, State) ->
    Res = case catch apply(Module, Fun, Args) of
              {'EXIT', Info} ->
                  ?ERROR_MSG("svr_role_call apply_call error ~p~n", [[Module, Fun, Args, Info]]),
                  {error, Info};
              Ret ->
                  Ret
          end,
    {reply, Res, State};
%% CALL到进程执行函数（带STATE参数）
handle_call({apply_call2, [Module, Fun, Args]}, _From, State) ->
    {Res, NewState} = case catch apply(Module, Fun, Args ++ [State]) of
                          {'EXIT', _Info} ->
                              {error, State};
                          NewStateT when is_record(NewStateT, role_state) ->
                              {ok, NewStateT};
                          {ok, NewStateT} when is_record(NewStateT, role_state) ->
                              {ok, NewStateT};
                          {Ret, NewStateT} when is_record(NewStateT, role_state) ->
                              {Ret, NewStateT};
                          {ok, Ret, NewStateT} when is_record(NewStateT, role_state) ->
                              {Ret, NewStateT};
                          ok ->
                              {ok, State};
                          {false, ErrCode} ->
                              {{false, ErrCode}, State};
                          _R ->
                              {ignore, State}
                      end,
    {reply, Res, NewState};
%% 打印进程某字段数据
handle_call({print_state, [Args]}, _From, State) ->
    Reply = case Args of
                all -> io:format("RoleState:~p~n", [State]), State;
                N when N > 0, is_integer(N) ->
                    io:format("RoleState-element:~p :~p~n", [N, element(N, State)]),
                    element(N, State);
                _ -> []
            end,
    {reply, Reply, State};
%% 处理SOCKET协议
%% @param:: cmd: 协议号
%%          data: 协议数据
handle_call({'SOCKET_CMD', [Cmd, Bin]}, _From, State) ->
    % 协议逻辑
    case catch routing(Cmd, State, Bin) of
        {ok, NewState} when is_record(NewState, role_state) ->
            {reply, ok, NewState};
        {ok, V, NewState} when is_record(NewState, role_state) ->
            ProtHandler = lib_proto:get_resolve_handler(Cmd),
            ProtHandler:response(V, NewState),
            {reply, ok, NewState};
        NewState when is_record(NewState, role_state) ->
            {reply, ok, NewState};
        {ok, NewState} ->
            ?ERROR_MSG("role receive msg error, cmd:~p badrecord:~p", [Cmd, NewState]),
            {reply, ok, State};
        {ok, V, NewState} ->
            ?ERROR_MSG("role receive msg error, cmd:~p, V:~p, badrecord:~p", [Cmd, V, NewState]),
            {reply, ok, State};
        {'EXIT', R} ->
            ?ERROR_MSG("role receive msg error, cmd:~p, reason:~p~n~p", [Cmd, R, State#role_state.role_id]),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;
%% 获取玩家属性
handle_call({get_data, [TypeLis]}, _from, State) ->
    {RetList, NewState} = get_data_sub(TypeLis, [], State),
    {reply, RetList, NewState};

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
        _ ->
            ok
    end,
    {noreply, State};
%% CAST到进程执行函数（带STATE参数）
handle_cast({apply_cast2, [Module, Fun, Args]}, State) ->
    StateN = case catch apply(Module, Fun, Args ++ [State]) of
                 {'EXIT', Info} ->
                     ?ERROR_MSG("svr_role_cast apply_cast2 error ~p~n", [[Module, Fun, Args, Info]]),
                     State;
                 NewStateT when is_record(NewStateT, role_state) ->
                     NewStateT;
                 {_, NewStateT} when is_record(NewStateT, role_state) ->
                     NewStateT;
                 _ ->
                     State
             end,
    {noreply, StateN};
%% 延迟发送消息
handle_cast({send_after, [DelayTime, Msg]}, State) ->
    erlang:send_after(DelayTime, self(), Msg),
    {noreply, State};
%% 断线重连
handle_cast({reconnect, [Ip, Socket, RoleSidN, [Device]]}, State) ->
    % 重置客户端连接数据
    StateN = State#role_state{
        sid = RoleSidN, ip = Ip, socket = Socket, device = Device
    },
    {noreply, StateN};
%% 初始化模块数据
handle_cast({init_role_data, [Ip, Socket, RoleSid, ClientExtra]}, State) ->
    StateN0 = State#role_state{
        pid = self(), sid = RoleSid, ip = Ip, socket = Socket
    },
    {ok, StateN} = lib_role:init_role_data(ClientExtra, StateN0),
    {noreply, StateN};
%% 玩家定时保存数据
handle_cast({save_timer}, State) ->
    StateN = lib_role:interval_save(State),
    lib_role:start_save_timer(),
    {noreply, StateN};
%% 玩家数据日清处理
handle_cast({daily_reset_zero}, State) ->
    % 延迟日清处理
    Time = util:rand(6, 60) * 1000,
    erlang:send_after(Time, self(), daily_reset),
    % 实时日清处理
    StateN = lib_role:daily_reset_zero(State),
    {noreply, StateN};
%% 玩家数据日清处理(随机延迟)
handle_cast(daily_reset, State) ->
    StateN = lib_role:daily_reset(State),
    {noreply, StateN};
%% 发送二进制消息
handle_cast({send, BinData}, State) ->
    #role_state{sid = RoleSid} = State,
    lib_send:send_to_sid(RoleSid, BinData),
    {noreply, State};
%% 定时结束进程
handle_cast(stop_after, State) ->
    #role_state{pid = RolePid} = State,
    case catch lib_role_login:logout_after(State) of
        {'EXIT', R} ->
            ?ERROR_MSG("logout after stop error:~p~n", [R]),
            svr_role:stop(RolePid),
            {noreply, State};
        StateN ->
            {noreply, StateN}
    end;
%% 停止游戏进程
handle_cast(stop, State) ->
    case catch lib_role_login:logout(State) of
        {'EXIT', R} ->
            ?ERROR_MSG("logout cast stop error:~p~n", [R]);
        _ ->
            ignore
    end,
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
handle_info(Info, State) ->
    cast(State#role_state.role_id, Info),
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
terminate(normal, _State) ->
    ok;
terminate(_Reason, State) ->
    ?ERROR_MSG("role logout terminate reason: ~p", [_Reason]),
    case catch lib_role_login:logout(State) of
        {'EXIT', R} ->
            ?ERROR_MSG("role logout terminate stop:~p~n", [R]);
        _ ->
            ignore
    end,
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
    dist:get_role_process_pid(RoleId).

%% 协议路由
%% @param:: cmd: 协议号
%%          Msg: 消息内容
%%data:消息体
routing(Cmd, RoleState, Msg) ->
    ProtHandler = lib_proto:get_resolve_handler(Cmd),
    ProtHandler:do(RoleState, Msg).

%% 获取玩家进程数据
%% @param:: KeyList: 获取列表 eg. [Key, ...] Key为原子类型
%%          RetList: 结果返回列表 eg. [{Key, Value},...]
%%          RoleState: 当前玩家进程状态
%% @return:: NewRetList: 新增的返回列表
%%           NewRoleState: 更新后玩家状态
get_data_sub([], RetList, RoleState) -> {RetList, RoleState};
get_data_sub([Key | T], RetList, RoleState) ->
    {NewRetList, NewRoleState} = do_get_data_sub(Key, RetList, RoleState),
    get_data_sub(T, NewRetList, NewRoleState).

%% 默认匹配
do_get_data_sub(_Key, RetList, RoleState) ->
    {RetList, RoleState}.