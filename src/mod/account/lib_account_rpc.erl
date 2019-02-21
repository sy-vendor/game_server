%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 账号相关协议处理
%%% @end
%%% Created : 20. 十二月 2018 15:26
%%%-------------------------------------------------------------------
-module(lib_account_rpc).
-author("suyang").

%% API
-export([handle/3]).


-include("common.hrl").
-include("conn.hrl").
-include("role.hrl").

-define(hb_inteval, 2).
-define(ROLE_ONLINE_MAX, 6000).

-include("logic.hrl").
-include("err_code.hrl").
-include("ets.hrl").

%% @doc 心跳请求
handle(10001, #'HeartReq'{}, State = #conn{last_hb = _LastHb, hb_count = HbCount, hb_start = HbStart, hb_err = HbErr, account = _Account}) ->
    Now = util_time:unixtime(),
    sys_conn:pack_send(self(), 10002, #'HeartRes'{}),
    case HbCount >= 10 of
        true ->
            Diff = max(Now - HbStart, 1),
            AccSpeed = ?hb_inteval * 10 / Diff,
            case AccSpeed >= 1.4 of
                true ->
                    ?DEBUG("heart accelerate：count=~w, start=~w, now = ~w, diff = ~w, acc_speed=~w, err_count=~w", [HbCount, HbStart, Now, Now - HbStart, AccSpeed, HbErr]),
%%                    {ok, State#conn{last_hb = Now, hb_count = 1, hb_start = Now, hb_err = HbErr + 1}};
                    case HbErr >= 3 of
                        true ->
                            {stop, heart_accelerate_err};
                        false ->
                            {ok, State#conn{last_hb = Now, hb_count = 1, hb_start = Now, hb_err = HbErr + 1}}
                    end;
                false ->
                    {ok, State#conn{last_hb = Now, hb_count = 1, hb_start = Now, hb_err = 0}}
            end;
        _ ->
            {ok, State#conn{last_hb = Now, hb_count = HbCount + 1}}
    end;
%% 登录请求
handle(10003, _Data, Conn = #conn{socket = Socket, pid_object = ObjectPid, account = Account}) when ObjectPid =/= undefined ->
    ?ERROR_MSG("socket pid is Duplicate connection"),
    {ok, Bin} = lib_proto:pack(10004, #'LoginRes'{code = ?ERR_COMMON_SYS, tokenId = Account}),
    lib_send:send_to_socket(Socket, Bin),
    {ok, Conn}; %% 阻止同一socket重复登录
handle(10003, #'LoginReq'{tokenId = Account, name = Name, headUrl = HearUrl, gender = Sex, inviteTokenId = InviteTokenId}, Conn = #conn{ip = Ip, socket = Socket, port = _Port}) ->
    RoleIdList = ets:match(?ETS_ONLINE, #ets_online{role_id = '$1', _ = '_'}),
    case length(RoleIdList) >= ?ROLE_ONLINE_MAX of
        true ->
            {ok, Bin} = lib_proto:pack(10004, #'LoginRes'{code = ?ERR_COMMON_SYS, tokenId = Account}),
            lib_send:send_to_socket(Socket, Bin);
        false ->
            IpStr = util:ip2bin(Ip),
            case lib_role_api:login(start, [Account, IpStr, Socket, Name, HearUrl, Sex, InviteTokenId, self()]) of
                {ok, RolePid} when is_pid(RolePid) ->
                    {ok, Bin} = lib_proto:pack(10004, #'LoginRes'{code = 0, tokenId = Account}),
                    lib_send:send_to_socket(Socket, Bin),
                    ConnN = Conn#conn{account = Account, object = svr_role, pid_object = RolePid},
                    {ok, ConnN};
                _R ->
                    ?ERROR_MSG("=================login ERR:~w", [_R]),
                    {ok, Bin1} = lib_proto:pack(10004, #'LoginRes'{code = ?ERR_COMMON_SYS, tokenId = Account}),
                    lib_send:send_to_socket(Socket, Bin1),
                    {ok, Conn}
            end
    end;
%% 微信登陆请求
handle(11801, #'WechatLoginReq'{wxCode = WxCode}, #conn{socket = Socket}) ->
    lib_account_api:wechat_login(WxCode, Socket),
    {ok};
%% 容错处理
handle(_Cmd, _Data, #conn{account = Account}) ->
    ?DEBUG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.