%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 角色相关处理
%%% @end
%%% Created : 10. 十二月 2018 17:42
%%%-------------------------------------------------------------------
-module(lib_role_api).
-author("suyang").

%% API
-export([
    create_role/5,
    create_role/6,
    login/2,
    init_role_data/1,
    logout/1,
    logout_after/1,
    stop_all/0,
    start_second_timer/0,
    daily_reset_all/0,
    daily_reset/1]).

-include("role.hrl").
-include("common.hrl").
-include("ets.hrl").

%% @spec create_role(Id, Account, Name, Sex, RegIp) -> ok | {false, Reason}
%% Id = integer()
%% Account = bitstring()
%% Name = bitstring()
%% Sex = integer()
%% RegIp = bitstring()
%% Reason = bitstring()
%% 创建角色
create_role(Account, Name, HearUrl, Sex, RegIp) ->
    RoleId = inc_id_mgr:get_id(?ROLE_ID_INC),
    create_role(RoleId, Account, Name, HearUrl, Sex, RegIp).
create_role(RoleId, Account, Name, HearUrl, Sex, RegIp) ->
    case lib_role_db:init_role_info(RoleId, util_type:to_binary(Name), util_type:to_binary(Account), util_type:to_list(RegIp), util_type:to_binary(HearUrl), Sex) of
        {ok, _R} -> ok;
        _Err ->
            ?ERROR_MSG("role init is err:~w", [_Err]),
            {false, err_role_create}
    end.

%% @doc 玩家登陆
%% 1、玩家开始登陆检查
login(start, [Account, Ip, Socket, Name, HearUrl, Sex, InviteTokenId, ConnPid]) ->
    AccountN = util_type:to_binary(util_type:to_list(Account)),
    case lib_role_db:get_role_id_by_account(AccountN) of
        {ok, [RoleId]} -> login(do, [RoleId, Ip, Socket, Account, InviteTokenId, ConnPid, HearUrl, repeat]);
        _ ->
            create_role(AccountN, Name, HearUrl, Sex, Ip),
            case lib_role_db:get_role_id_by_account(AccountN) of
                {ok, [RoleIdNew]} ->
                    login(do, [RoleIdNew, Ip, Socket, Account, InviteTokenId, ConnPid, HearUrl, init]);
                _ -> {error, mysql_creat_err}
            end
    end;
login(do, [RoleId, Ip, Socket, Account, InviteTokenId, ConnPid, HearUrl, Type]) ->
    RolePidR = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePidR) of
        true -> % 玩家进程存在
            case catch do_login_reconnect(RoleId, Ip, Socket, ConnPid) of
                {'EXIT', Error} -> {error, Error};
                true -> {ok, RolePidR}
            end;
        false -> % 玩家进程不存在
            case catch svr_role:start(RoleId, Account, ConnPid) of
                {ok, RolePid} ->
                    % 登陆启动
                    case catch do_login(RoleId, RolePid, Ip, Socket, InviteTokenId, HearUrl, Account, Type) of
                        {'EXIT', Error} -> {error, Error};
                        true -> {ok, RolePid}
                    end;
                {'EXIT', Error} -> {error, Error}
            end
    end;
%% 3、容错匹配
login(_R, _S) -> {error, svr_role_fail}.

%% 玩家登陆处理
do_login(RoleId, RolePid, Ip, Socket, InviteTokenId, HearUrl, Account, Type) ->
    % 创建消息收发进程
    RoleSid = spawn_link(fun() -> send_msg(Socket) end),
    util_dist:register(global, util_dist:role_send_process_name(RoleId), RoleSid),
    % 更新在线人数状态
    add_role_online(RoleId, RolePid, RoleSid),
    svr_role:init_role_data(RolePid, [RoleId, Ip, Socket, RoleSid]),
    % 判断是否携带InviteId
    ?IF(Type =:= init, lib_invite_api:b_invite_handle(InviteTokenId, HearUrl, Account, RoleId), ignore),
    true.

%% 同步更新ETS中的玩家数据
add_role_online(RoleId, RolePid, RoleSid) ->
    RoleOnline = #ets_online{
        role_id = RoleId, pid = RolePid, sid = RoleSid
    },
    ets:insert(?ETS_ONLINE, RoleOnline).

%% 发送消息
send_msg(Socket) ->
    receive
        stop ->
            gen_tcp:close(Socket),
            ok;
        {send, close} ->
            gen_tcp:close(Socket),
            ok;
        {inet_reply, _E, ok} ->
            send_msg(Socket);
        {send, Bin} ->
            case erlang:port_command(Socket, Bin, [force]) of
                true ->
                    send_msg(Socket);
                Ex ->
                    ?ERROR_MSG("port_command one ex:~w ~n", [Ex]),
                    send_msg(Socket)
            end;
        {inet_reply, _, {error, closed}} ->
            gen_tcp:close(Socket),
            ok;
        {inet_reply, _, _ERR} ->
            ?ERROR_MSG("port_command two _ERR:~w ~n", [_ERR]),
            send_msg(Socket);
        _R ->
            ?ERROR_MSG("port_command three _R:~w ~n", [_R]),
            send_msg(Socket)
    end.

%% @doc 玩家登陆数据初始化
init_role_data(Role) ->
    % 初始化数据库基本数据
    RoleN = init_db_data(Role),
    #role{last_logout_time = LastLogoutTime} = RoleN,
    NowTime = util_time:unixtime(),
    NewRole = init_module_data(NowTime, LastLogoutTime, RoleN),
    % 启动秒循环定时器
    start_second_timer(),
    % 登录相关触发
    RoleNew = lib_event:role_login(true, NowTime, NewRole),
    {ok, RoleNew}.

%% @doc 初始化数据库基本数据
init_db_data(Role) ->
    {ok, [RoleName, Url, Sex, RegTime, LastLogoutTime, Coin, Diamond, Reputation]} = lib_role_db:get_role_login_data(Role#role.role_id),
    Role#role{role_name = RoleName,
        url = binary_to_list(Url),
        sex = Sex,
        reg_time = RegTime,
        last_logout_time = LastLogoutTime,
        coin = Coin,
        diamond = Diamond,
        reputation = Reputation,
        last_login_time = util_time:unixtime()}.

%% 玩家断线重连
do_login_reconnect(RoleId, Ip, Socket, ConnPid) ->
    % 关闭当前消息收发进程
    RoleSidR = util_dist:get_role_send_process_pid(RoleId),
    case util_dist:is_process_alive(RoleSidR) of
        true ->
            lib_send:send_to_sid(RoleSidR, close),
            stop_send_msg(RoleId, RoleSidR);
        false ->
            ignore
    end,
    % 创建消息收发进程
    RoleSidN = spawn_link(fun() -> send_msg(Socket) end),
    util_dist:register(global, util_dist:role_send_process_name(RoleId), RoleSidN),
    % 更新玩家进程内存数据
    svr_role:reconnect(RoleId, [Ip, Socket, RoleSidN, ConnPid]),
    true.

%% 启动秒循环定时器
start_second_timer() ->
    erlang:send_after(?INTERVAL_SEC_SPAN, self(), {second_timer}).

%% @doc 玩家登出操作
logout(RoleId) when is_integer(RoleId) ->
    case util_dist:get_role_process_pid(RoleId) of
        RolePid when is_pid(RolePid) -> logout(RolePid);
        _ -> ignore
    end;
logout(RolePid) when is_pid(RolePid) ->
    case util_dist:is_process_alive(RolePid) of
        true -> svr_role:stop_after(RolePid);
        false -> ignore
    end;
logout(Role) when is_record(Role, role) ->
    do_logout(Role);
logout(Any) ->
    ?ERROR_MSG("role logout not match Any=~p~n", [Any]).

%% @doc 玩家定时登出操作
logout_after(RoleId) when is_integer(RoleId) ->
    case util_dist:get_role_process_pid(RoleId) of
        RolePid when is_pid(RolePid) ->
            logout_after(RolePid);
        _ ->
            ignore
    end;
logout_after(RolePid) when is_pid(RolePid) ->
    case util_dist:is_process_alive(RolePid) of
        true ->
            svr_role:stop_after(RolePid);
        false ->
            ignore
    end;
logout_after(Role) when is_record(Role, role) ->
    do_logout_forward(Role);
logout_after(Any) ->
    ?ERROR_MSG("role logout_after not match Any=~p~n", [Any]).

%% 玩家定时登出处理
do_logout_forward(Role) ->
    NowTime = util_time:unixtime(),
    #role{role_id = RoleId, sid = RoleSid} = Role,
    % 关闭消息收发进程
    stop_send_msg(RoleId, RoleSid),
    Role#role{last_logout_time = NowTime}.

do_logout(Role) ->
    #role{role_id = RoleId, sid = RoleSid} = Role,
    % 存储玩家信息
    save_db_data(Role),
    save_role_data(Role, util_time:unixtime()),
    % 保存永久键值对数据
    lib_data_api:save(RoleId),
    % 删除玩家在线标记
    del_role_online(RoleId),
    % 关闭消息收发进程
    stop_send_msg(RoleId, RoleSid),
    % 删除玩家进程全局注册
    unregist_role_pid(RoleId),
    ok.

%% 关闭消息发送进程
stop_send_msg(RoleId, RoleSid) when is_pid(RoleSid) ->
    util_dist:unregister(global, util_dist:role_send_process_name(RoleId)),
    exit(RoleSid, kill);
stop_send_msg(_, _) -> ignore.

%% 同步更新ETS中的玩家数据
del_role_online(RoleId) ->
    util:del_ets(?ETS_ONLINE, RoleId).

%% 删除玩家进程PID
unregist_role_pid(RoleId) ->
    RegName = util_dist:role_process_name(RoleId),
    util_dist:unregister(global, RegName).

save_db_data(Role) ->
    #role{role_id = RoleId, role_name = RoleName, role_lv = RoleLv, reg_time = ResTime, last_login_time = LastLoginTime, sex = Sex, state = State, account = Account, ip = Ip, coin = Coin, diamond = Diamond, reputation = Reputation} = Role,
    lib_role_db:save_role_info(RoleId, util_type:to_binary(RoleName), util_type:to_binary(Account), util_type:to_list(Ip), RoleLv, ResTime, LastLoginTime, util_time:unixtime(), Sex, State, Coin, Diamond, Reputation).


%% @doc 将所有玩家踢下线
stop_all() ->
    ?INFO("Prepare to stop server..."),
    do_stop_all(ets:tab2list(?ETS_ONLINE)),
    ok.

%% 让所有角色自动退出
do_stop_all([]) -> ok;
do_stop_all([R | L]) ->
    % 关闭SOCKET
    #ets_online{pid = RolePid, sid = RoleSid} = R,
    lib_send:send_to_sid(RoleSid, close),
    % 执行退出逻辑
    spawn(fun() -> logout(RolePid) end),
    timer:sleep(2000),
    do_stop_all(L).

%% 初始化注册的模块
init_module_data(NowTime, LastLogOutTime, Role) ->
    F = fun(Module, RoleAcc) ->
        case catch Module:init(NowTime, LastLogOutTime, RoleAcc) of
            {_, RoleAccN} when is_record(RoleAccN, role) ->
                RoleAccN;
            RoleAccN when is_record(RoleAccN, role) ->
                RoleAccN;
            {'EXIT', Info} ->
                ?ERROR_MSG("---Role Init Module:~p, Error:~p---~n", [Module, Info]),
                RoleAcc;
            _ ->
                RoleAcc
        end
        end,
    lists:foldl(F, Role, ?INIT_MODULES).

%% 保存各模块数据
save_role_data(Role, NowTime) ->
    F = fun(Module, RoleAcc) ->
        case catch Module:save(NowTime, RoleAcc) of
            {_, RoleAccN} when is_record(RoleAccN, role) ->
                RoleAccN;
            RoleAccN when is_record(RoleAccN, role) ->
                RoleAccN;
            {'EXIT', Info} ->
                ?ERROR_MSG("---Role Save Module:~p, Error:~p---~n", [Module, Info]),
                RoleAcc;
            _ ->
                RoleAcc
        end
        end,
    lists:foldl(F, Role, ?SAVE_MODULES).

%% @doc 所有玩家执行0点重置
daily_reset_all() ->
    RoleIdList = ets:match(?ETS_ONLINE, #ets_online{role_id = '$1', _ = '_'}),
    [svr_role:cast(RoleId, {daily_reset}) || RoleId <- RoleIdList].

daily_reset(Role) ->
    NowTime = util_time:unixtime(),
    Today = util_time:unixdate(NowTime),
    ?TRY_CATCH(lib_daily_api:daily_reset(Today, Role)),
    ?TRY_CATCH(lib_sign_api:daily_reset()),
    Role.
