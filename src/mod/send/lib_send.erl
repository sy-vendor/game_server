%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 消息发送处理
%%% @end
%%% Created : 07. 十二月 2018 16:59
%%%-------------------------------------------------------------------
-module(lib_send).
-author("suyang").

%% API
-export([
    send_to_socket/2,
    send_to_sid/2,
    send_to_role/2,
    send_to_local_all/1
]).

-include("common.hrl").
-include("role.hrl").
-include("ets.hrl").

%% @doc 发送信息给玩家
%% @param:: Socket: socket
%%          BinData: 二进制数据
send_to_socket(Socket, BinData) ->
    gen_tcp:send(Socket, BinData).

%% @doc 发送信息给玩家
%% @param:: RoleSid: 发送进程ID
%%          BinData: 二进制数据
send_to_sid(Role, BinData) when is_record(Role, role) ->
    Role#role.sid ! {send, BinData};
send_to_sid(RoleSid, BinData) when is_pid(RoleSid) ->
    RoleSid ! {send, BinData};
send_to_sid(_Sid, _Bin) -> skip.

%% @doc 发送信息给玩家
send_to_role(#role{sid = RoleSid}, BinData) ->
    send_to_sid(RoleSid, BinData);
send_to_role(RoleId, BinData) when is_integer(RoleId) ->
    svr_role:cast(RoleId, {send, BinData});
send_to_role(RolePid, BinData) when is_pid(RolePid) ->
    svr_role:cast(RolePid, {send, BinData});
send_to_role(_, _BinData) -> skip.

%% @doc 发送消息给当前节点的玩家
send_to_local_all(BinData) ->
    RolePidList = ets:match(?ETS_ONLINE, #ets_online{pid = '$1', _ = '_'}),
    F = fun([RolePid]) ->
        svr_role:cast(RolePid, {send, BinData})
        end,
    [F(Pid) || Pid <- RolePidList].