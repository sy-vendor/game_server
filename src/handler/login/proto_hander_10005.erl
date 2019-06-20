%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 创建角色
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(proto_hander_10005).
-author("sy").

%% API
-export([do/2, response/2]).

-include("role.hrl").
-include("common.hrl").
-include("err_code.hrl").

-record(cs_create_role, {
    nickname = "",
    server_num = 0,
    career = 0,
    ditch_id = 0,
    device = 0
}).

-record(sc_create_role, {
    role_id = 0,
    nickname = "",
    career = 0,
    sex = 0,
    reg_time = 0,
    state = 0
}).

%% =============================================================================
%% API
%% =============================================================================
%% @doc 处理协议操作
do(Role, Data) when is_record(Role, role) ->
    #role{
        acc_id = AccId,
        accname = AccName,
        login_flag = LoginFlag,
        socket = Socket
    } = Role,
    #cs_create_role{
        nickname = NicknameN,
        server_num = ServerNum,
        career = Career,
        ditch_id = DitchId,
        device = Device
    } = Data,
    case is_list(NicknameN) andalso LoginFlag =:= 1 of
        false ->
            {ok, BinData} = lib_proto:encode_msg(100006, #sc_create_role{}),
            lib_send:send_to_socket(Socket, BinData);
        true ->
            IP = util:get_ip(Socket),
            Nickname = util:escape_varchar(NicknameN),
            case catch check_create_role(AccName, ServerNum, Nickname, Career, IP, Device) of
                {error, ErrCode} ->
                    ?ERROR_TOC(Role, ErrCode),
                    {ok, BinData} = lib_proto:encode_msg(10006, #sc_create_role{}),
                    lib_send:send_to_socket(Socket, BinData);
                {ok, Sex} ->
                    case lib_role:create_role(AccId, AccName, Career, IP, Nickname, Sex, DitchId, Device, ServerNum) of
                        {error, ErrCode} -> % 角色创建失败
                            ?ERROR_TOC(Role, ErrCode),
                            {ok, BinData} = lib_proto:encode_msg(10006, #sc_create_role{}),
                            lib_send:send_to_socket(Socket, BinData);
                        {RoleId, RegTime} -> % 创建角色成功
                            {ok, BinData} = lib_proto:encode_msg(10006, #sc_create_role{role_id = RoleId, nickname = Nickname, career = Career, sex = Sex, reg_time = RegTime, state = 1}),
                            lib_send:send_to_socket(Socket, BinData)
                    end
            end
    end,
    {ok, Role};
do(_Role, Data) ->
    ?ERROR_MSG("CMD 10005 is not match:~p", [Data]),
    {error, "CMD 10005 is not match"}.

%% @doc 回调处理操作
response(_V, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================.
%% 创角检测
-define(SQL_EXIST_ROLE_GET, <<"select role_id from role_login where accname='~s' and server_id=~p limit 1">>).
check_create_role(AccName, ServerNum, Nickname, Career, IP, Device) ->

    % 检查职业和性别是否对应关系
    case lists:keyfind(Career, 1, ?CAREER_LIST) of
        false ->
            erlang:throw({error, ?ERR_COMMON_CAREER_NO_EXIST}),
            Sex = 0;
        {_, SexT} ->
            Sex = SexT
    end,

    % 检查名字合法性
    lib_role_aid:validate_name(?MODULE, Nickname),

    % 检查IP是否被封禁
    CheckIp = svr_ban:create_check_ip(IP),
    ?iif(CheckIp =/= passed,
        erlang:throw({error, ?ERR_LOGIN_IP_BAN}), ignore),

    % 检查设备是否被封禁
    CheckDevice = svr_ban:create_check_device(Device),
    ?iif(CheckDevice =/= passed,
        erlang:throw({error, ?ERR_LOGIN_DEVICE_BAN}), ignore),

    % 判断是否已经存在角色
    Sql = io_lib:format(?SQL_EXIST_ROLE_GET, [AccName, ServerNum]),
    case ?DB:get_row(?POOL_GAME, Sql) of
        [] -> ignore;
        _ -> erlang:throw({error, ?ERR_LOGIN_ROLE_EXIST})
    end,

    {ok, Sex}.