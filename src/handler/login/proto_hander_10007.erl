%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 选择登陆角色登陆
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(proto_hander_10007).
-author("sy").

%% API
-export([do/2, response/2]).

-include("role.hrl").
-include("common.hrl").
-include("err_code.hrl").

-record(cs_select_role, {
    role_id = 0,
    time_stamp = 0,
    ticket  = "",
    device = 0
}).

%% =============================================================================
%% API
%% =============================================================================
%% @doc 处理协议操作
do(Role, Data) when is_record(Role, role) ->
    #role{
        accname = AccName,
        login_flag = LoginFlag,
        socket = Socket
    } = Role,
    case LoginFlag =:= 1 of
        false -> {ok, Role};
        true ->
            #cs_select_role{
                role_id = RoleId,
                time_stamp = TimeStamp,
                ticket = Ticket,
                device = Device
            } = Data,
            IP = util:get_ip(Socket),
            case catch check_role_login(RoleId, TimeStamp, Ticket, IP, Device) of
                {error, ErrCode} ->
                    ?ERROR_TOC(Role, ErrCode),
                    {ok, Role};
                {ok, _Trustee} ->
                    ClientExtra = [Device],
                    case catch lib_role_login:login(start, [RoleId, AccName, IP, Socket, ClientExtra]) of
                        {error, ErrCode} -> % 登陆失败
                            ?ERROR_TOC(Role, ErrCode),
                            {ok, Role};
                        {ok, RolePid} -> % 登陆成功
                            ?ERROR_TOC(Role, ?ERR_LOGIN_SUCCESSFUL),
                            {ok, start, Role#role{pid = RolePid}};
                        Error ->
                            ?ERROR_MSG("---Role Lgoin Failed:~p----~n",[Error]),
                            {ok, Role}
                    end
            end
    end;
do(_Role, Data) ->
    ?ERROR_MSG("CMD 10007 is not match:~p", [Data]),
    {error, "CMD 10007 is not match"}.

%% @doc 回调处理操作
response(_V, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================.
%% 检查登陆限制
check_role_login(RoleId, TimeStamp, Ticket, IP, Device) ->

    % 检查TICKET
    TicketCheck = util:check_char_encrypt(RoleId, TimeStamp, Ticket),
    ?iif(TicketCheck =:= false,
        erlang:throw({error, ?ERR_LOGIN_TICKET_UNPASSED}), ignore),

    % 检查角色封禁情况
    case svr_ban:login_check(RoleId, IP, Device) of
        passed -> % 通过登陆
            ignore;
        login_more -> % 登录太频繁
            erlang:throw({error, ?ERR_LOGIN_TIMES_LIMIT});
        forbid_all -> % 所有账号登陆都被禁止
            erlang:throw({error, ?ERR_LOGIN_SERVER_DOWN});
        forbid_ip ->  % 封禁IP
            erlang:throw({error, ?ERR_LOGIN_IP_BAN});
        forbid_device -> % 封禁设备
            erlang:throw({error, ?ERR_LOGIN_DEVICE_BAN});
        Error -> %% 未定义
            ?ERROR_MSG("Login State INNORMAL:~p", [Error]),
            erlang:throw({error, ?ERR_LOGIN_STATE_INNORMAL})
    end,

    {ok, 0}.