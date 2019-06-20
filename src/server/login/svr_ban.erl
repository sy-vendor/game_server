%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 玩家封禁管理
%%% @end
%%% Created : 23. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_ban).
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

-export([login_check/3, ban_all/0, create_check_ip/1, create_check_device/1, check_bai/1, ban_ip/1, unbai_ip/1, bai_ip/1, unban_ip/1, ban_device/1, unban_device/1,
    ban_role/1, unban_role/1, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-define(BAN_IP, 1).                 %% 封禁IP
-define(BAI_IP, 2).                 %% 白名单
-define(BAN_DEVICE, 3).             %% 封禁设备

%% 进程数据
-record(state, {
    role_ip_dict = dict:new(),      %% 角色的IP字典(Key:Ip,Value:Id列表)
    role_device_dict = dict:new(),  %% 角色的IP字典(Key:Device,Value:Id列表)
    ban_ip_list = sets:new(),       %% 被禁止的IP列表
    ban_device_list = sets:new(),   %% 被禁止的设备列表
    bai_list = sets:new(),          %% IP 白名单
    ban_all = false                 %% 是否禁止所有人登陆 false:否 true:是
}).

-include("common.hrl").
-include("err_code.hrl").
-include("role.hrl").
-include("acc_data.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 角色登录时检查IP状态
login_check(Id, Ip, Device) ->
    call({login_check, [Id, Ip, Device]}).

%% @doc 添加禁止所有人登陆
ban_all() ->
    call({ban_all}).

%% @doc 角色注册时检查IP状态
create_check_ip(Ip) ->
    call({create_check_ip, [Ip]}).

%% @doc 角色注册时检查设备状态
create_check_device(Device) ->
    call({create_check_device, [Device]}).

%% @doc 检查ip是否在白名单中
%% @param 角色IP地址
%% @return true | false
check_bai(IpStr) ->
    Ip = list_to_binary(util:ip2bin(IpStr)),
    call({check_bai, [Ip]}).

%% @doc 添加白名单IP
bai_ip(IpStr) ->
    Ip = list_to_binary(util:ip2bin(IpStr)),
    call({bai_ip, [Ip]}).

%% @doc 解除白名单IP
unbai_ip(IpStr) ->
    Ip = list_to_binary(util:ip2bin(IpStr)),
    call({unbai_ip, [Ip]}).

%% @doc 添加封禁IP
ban_ip(IpStr) ->
    Ip = list_to_binary(util:ip2bin(IpStr)),
    call({ban_ip, [Ip]}).

%% @doc 解除封禁IP
unban_ip(IpStr) ->
    Ip = list_to_binary(util:ip2bin(IpStr)),
    call({unban_ip, [Ip]}).

%% @doc 添加封禁设备
ban_device(DeviceStr) ->
    Device = list_to_binary(DeviceStr),
    call({ban_device, [Device]}).

%% @doc 解除封禁设备
unban_device(DeviceStr) ->
    Device = list_to_binary(DeviceStr),
    call({unban_device, [Device]}).

%% @doc 封禁账号
ban_role(RoleList) ->
    call({ban_role, [RoleList]}).

%% @doc 解封账号
unban_role(RoleList) ->
    call({unban_role, [RoleList]}).

%% @doc 获取进程ID
p() ->
    dist:whereis_name(global, ?MODULE).

%% @doc call函数
call(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg, 5000);
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
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

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
    State = init_state(),
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
handle_call({login_check, [Id, Ip, Device]}, _From, State) ->
    {IpData1, IpData2, _, _} = Ip,
    {Reply, NewState} =
        case IpData1 =:= 175 andalso IpData2 =:= 102 of
            true -> {forbidip, State};
            false -> private_check(Id, Ip, Device, State)
        end,
    {reply, Reply, NewState};

handle_call({ban_all}, _From, State) ->
    {reply, ok, State#state{ban_all = true}};

handle_call({create_check_ip, [Ip]}, _From, State) ->
    {IpData1, IpData2, _, _} = Ip,
    {Reply, NewState} = case IpData1 =:= 175 andalso IpData2 =:= 102 of
                            true -> {forbidip, State};
                            false ->
                                Ip2 = list_to_binary(util:ip2bin(Ip)),
                                case sets:is_element(Ip2, State#state.ban_ip_list) of
                                    true -> % ip在黑名单中
                                        {forbidip, State};
                                    false ->
                                        {passed, State}
                                end
                        end,
    {reply, Reply, NewState};

handle_call({create_check_device, [Device]}, _From, State) ->
    DeviceB = type:object_to_binary(Device),
    {Reply, NewState} = case sets:is_element(DeviceB, State#state.ban_device_list) of
                            true -> % 设备在黑名单中
                                {forbidip, State};
                            false ->
                                {passed, State}
                        end,
    {reply, Reply, NewState};

handle_call({check_bai, [Ip]}, _From, State) ->
    Reply = sets:is_element(Ip, State#state.bai_list),
    {reply, Reply, State};

handle_call({ban_ip, [Ip]}, _From, State) ->
    {Reply, NewState} = private_ban_ip(Ip, State),
    {reply, Reply, NewState};

handle_call({unban_ip, [Ip]}, _From, State) ->
    {Reply, NewState} = private_unban_ip(Ip, State),
    {reply, Reply, NewState};

handle_call({bai_ip, [Ip]}, _From, State) ->
    {Reply, NewState} = private_bai_ip(Ip, State),
    {reply, Reply, NewState};

handle_call({unbai_ip, [Ip]}, _From, State) ->
    {Reply, NewState} = private_unbai_ip(Ip, State),
    {reply, Reply, NewState};

handle_call({ban_device, [Device]}, _From, State) ->
    {Reply, NewState} = private_ban_device(Device, State),
    {reply, Reply, NewState};

handle_call({unban_device, [Device]}, _From, State) ->
    {Reply, NewState} = private_unban_device(Device, State),
    {reply, Reply, NewState};

handle_call({ban_role, [RoleList]}, _From, State) ->
    Reply = private_ban_role(RoleList),
    {reply, Reply, State};

handle_call({unban_role, [RoleList]}, _From, State) ->
    Reply = private_unban_role(RoleList),
    {reply, Reply, State};

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
%% 初始化状态数据
init_state() ->
    BlackList = sets:from_list(db_ban_get(?BAN_IP)),
    WhiteList = sets:from_list(db_ban_get(?BAI_IP)),
    BlackDevList = sets:from_list(db_ban_get(?BAN_DEVICE)),
    #state{
        role_ip_dict = dict:new(),
        role_device_dict = dict:new(),
        ban_ip_list = BlackList,
        ban_device_list = BlackDevList,
        bai_list = WhiteList
    }.

%% 登录检查角色状态
private_check(Id, Ip, Device, State) ->
    Time = time:unixtime(),
    LoginSate = case get({login_time, Id}) of
                    undefined -> true;
                    Lasetime -> Time - Lasetime > 1
                end,
    case LoginSate of
        true -> % 登陆成功记录下时间
            put({login_time, Id}, Time),
            #state{
                ban_all = IsBanAll,
                bai_list = BaiList,
                role_ip_dict = RoleIpDict,
                role_device_dict = RoleDeviceDict,
                ban_ip_list = BaiIpList,
                ban_device_list = BaiDevList
            } = State,
            case IsBanAll =:= true of
                true ->
                    {forbid_all, State};
                false ->
                    Ip2 = list_to_binary(util:ip2bin(Ip)),
                    Device2 = list_to_binary(Device),
                    case sets:is_element(Ip2, BaiList) of
                        true -> % ip在白名单中
                            NewRoleIpDict = dict:store(Id, Ip2, RoleIpDict),
                            NewState = State#state{role_ip_dict = NewRoleIpDict},
                            {passed, NewState};
                        false ->
                            case sets:is_element(Ip2, BaiIpList) of
                                true -> % ip在黑名单中
                                    {forbid_ip, State};
                                false ->
                                    case sets:is_element(Device2, BaiDevList) of
                                        true -> % device在黑名单中
                                            {forbid_device, State};
                                        false ->
                                            NewRoleIpDict = dict:store(Id, Ip2, RoleIpDict),
                                            NewRoleDevDict = dict:store(Id, Device2, RoleDeviceDict),
                                            NewState = State#state{role_ip_dict = NewRoleIpDict, role_device_dict = NewRoleDevDict},
                                            {passed, NewState}
                                    end
                            end
                    end
            end;
        false -> % 登陆过于频繁
            {login_more, State}
    end.

%% 添加白名单IP(不会立刻处理,只在下次登录或建立帐号的时候生效)
private_bai_ip(Ip, State) when is_binary(Ip) ->
    private_bai_ip([Ip], State);
private_bai_ip(IpList, State) when is_list(IpList) ->
    #state{bai_list = BaiList} = State,
    Fun = fun(Ip, Acc) ->
        db_ban_put(?BAI_IP, Ip),
        sets:add_element(Ip, Acc)
          end,
    NewIpList = lists:foldl(Fun, BaiList, IpList),
    {ok, State#state{bai_list = NewIpList}}.

%% 删除白名单IP(不会立刻处理,只在下次登录或建立帐号的时候生效)
private_unbai_ip(Ip, State) when is_binary(Ip) ->
    private_unbai_ip([Ip], State);
private_unbai_ip(IpList, State) when is_list(IpList) ->
    #state{bai_list = BaiList} = State,
    Fun = fun(Ip, Acc) ->
        db_ban_del(?BAI_IP, Ip),
        sets:del_element(Ip, Acc)
          end,
    NewBaiList = lists:foldl(Fun, BaiList, IpList),
    {ok, State#state{bai_list = NewBaiList}}.

%% 添加禁止的IP
private_ban_ip(Ip, State) when is_binary(Ip) ->
    private_ban_ip([Ip], State);
private_ban_ip(IpList, State) when is_list(IpList) ->
    #state{ban_ip_list = BanIpList} = State,
    Fun = fun(Ip, Acc) ->
        erlang:spawn(fun() -> private_kick_out_role(Ip, State) end),
        sets:add_element(Ip, Acc)
          end,
    NewBanIpList = lists:foldl(Fun, BanIpList, IpList),
    {ok, State#state{ban_ip_list = NewBanIpList}}.

%% 解禁IP
private_unban_ip(Ip, State) when is_binary(Ip) ->
    private_unban_ip([Ip], State);
private_unban_ip(IpList, State) when is_list(IpList) ->
    #state{ban_ip_list = BanIpList} = State,
    Fun = fun(Ip, Acc) ->
        db_ban_login_ip(Ip, 0),
        db_ban_del(?BAN_IP, Ip),
        sets:del_element(Ip, Acc)
          end,
    NewBanIpList = lists:foldl(Fun, BanIpList, IpList),
    {ok, State#state{ban_ip_list = NewBanIpList}}.

%% 添加禁止的device
private_ban_device(Device, State) when is_binary(Device) ->
    private_ban_device([Device], State);
private_ban_device(DevList, State) when is_list(DevList) ->
    #state{ban_device_list = BanDevList} = State,
    Fun = fun(Device, Acc) ->
        erlang:spawn(fun() -> private_kick_out_role_by_device(Device, State) end),
        sets:add_element(Device, Acc)
          end,
    NewBanDevList = lists:foldl(Fun, BanDevList, DevList),
    {ok, State#state{ban_device_list = NewBanDevList}}.

%% 解禁device
private_unban_device(Device, State) when is_binary(Device) ->
    private_unban_device([Device], State);
private_unban_device(DevList, State) when is_list(DevList) ->
    #state{ban_device_list = BanDevList} = State,
    Fun = fun(Device, Acc) ->
        db_ban_login_device(Device, 0),
        db_ban_del(?BAN_DEVICE, Device),
        sets:del_element(Device, Acc)
          end,
    NewBanDevList = lists:foldl(Fun, BanDevList, DevList),
    {ok, State#state{ban_device_list = NewBanDevList}}.

%% 封禁账号
private_ban_role(RoleList) ->
    Fun = fun(RoleId) ->
        lib_role_login:limit_login(RoleId),
        db_ban_login_id(RoleId, ?ROLE_STATE_BANACC),
        timer:sleep(200)
          end,
    spawn(fun() -> lists:foreach(Fun, RoleList) end),
    ok.

%% 解封账号
private_unban_role(RoleList) ->
    Fun = fun(RoleId) ->
        db_ban_login_id(RoleId, 0)
          end,
    lists:foreach(Fun, RoleList),
    ok.

%% 插入数据并把角色踢下线(根据IP)
private_kick_out_role(Ip, #state{role_ip_dict = RoleIpDict}) ->
    db_ban_login_ip(Ip, ?ROLE_STATE_BANIP),
    db_ban_put(?BAN_IP, Ip),
    dict:map(fun(Key, Value) ->
        case Value =:= Ip of
            true ->
                lib_send:send_to_role(Key, close),
                timer:sleep(200);
            false ->
                skip
        end
             end, RoleIpDict).

%% 插入数据并把角色踢下线(根据Device)
private_kick_out_role_by_device(Device, #state{role_device_dict = RoleDevDict}) ->
    db_ban_login_device(Device, ?ROLE_STATE_BANDEV),
    db_ban_put(?BAN_DEVICE, Device),
    dict:map(fun(Key, Value) ->
        case Value =:= Device of
            true ->
                lib_send:send_to_role(Key, close),
                timer:sleep(200);
            false ->
                skip
        end
             end, RoleDevDict).

%% 获取一个类型的禁止数据
-define(SQL_BAN_INFO_GET, <<"select `value` from `ban_info` where `type`=~p">>).
db_ban_get(Type) ->
    SQL = io_lib:format(?SQL_BAN_INFO_GET, [Type]),
    case ?DB:get_all(?POOL_GAME, SQL) of
        [] -> [];
        D -> [Dv || [Dv] <- D]
    end.

%% 更改指定IP的角色的数据库
-define(SQL_ROLE_LOGIN_STATE_BY_IP_UPDATE, <<"update `role_login` set `state`=~p where `last_login_ip`='~s'">>).
db_ban_login_ip(Ip, Type) ->
    IpStr = binary_to_list(Ip),
    SQL = io_lib:format(?SQL_ROLE_LOGIN_STATE_BY_IP_UPDATE, [Type, IpStr]),
    ?DB:execute(?POOL_GAME, SQL).

%% 更改指定Device的角色的数据库
-define(SQL_ROLE_LOGIN_STATE_BY_DEV_UPDATE, <<"update `role_login` set `state`=~p where `device`='~s'">>).
db_ban_login_device(Dev, Type) ->
    DevStr = binary_to_list(Dev),
    SQL = io_lib:format(?SQL_ROLE_LOGIN_STATE_BY_DEV_UPDATE, [Type, DevStr]),
    ?DB:execute(?POOL_GAME, SQL).

%% 更改指定账号的角色的数据库
-define(SQL_ROLE_LOGIN_STATE_BY_ID_UPDATE, <<"update `role_login` set `state`=~p where `role_id`=~p">>).
db_ban_login_id(RoleId, Type) ->
    SQL = io_lib:format(?SQL_ROLE_LOGIN_STATE_BY_ID_UPDATE, [Type, RoleId]),
    ?DB:execute(?POOL_GAME, SQL).

%% 插入一条新的规则
-define(SQL_BAN_INFO_INSERT, <<"insert into `ban_info` set `id`=~p, `type`=~p, `value`='~s'">>).
db_ban_put(Type, Content) ->
    ContentStr = binary_to_list(Content),
    Id = svr_acc_data:get_id(?ACC_BAN_ID),
    SQL = io_lib:format(?SQL_BAN_INFO_INSERT, [Id, Type, ContentStr]),
    ?DB:execute(?POOL_GAME, SQL),
    ok.

%% 删除一条规则(指定类型 1 封号 2 白名单 3 封设备)
-define(SQL_BAN_INFO_DELETE, <<"delete from `ban_info` where `type`=~p and `value`='~s'">>).
db_ban_del(Type, Ip) ->
    IpStr = binary_to_list(Ip),
    SQL = io_lib:format(?SQL_BAN_INFO_DELETE, [Type, IpStr]),
    ?DB:execute(?POOL_GAME, SQL),
    ok.
