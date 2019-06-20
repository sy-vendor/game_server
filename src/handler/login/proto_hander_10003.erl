%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 获取角色列表
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(proto_hander_10003).
-author("sy").

%% API
-export([do/2, response/2]).

-include("role.hrl").
-include("common.hrl").

-record(sc_role_list, {
    list = []
}).

%% =============================================================================
%% API
%% =============================================================================
%% @doc 处理协议操作
do(#role{login_flag = LoginFlag} = Role, _Data) when is_record(Role, role), LoginFlag =:= 1 ->
    #role{accname = AccName, server_num = ServerNum, socket = Socket} = Role,
    RoleList = lib_role:get_role_list(AccName, ServerNum),
    {ok, BinData} = lib_proto:encode_msg(10004, #sc_role_list{list = [list_to_tuple(R) || R <- RoleList]}),
    lib_send:send_to_socket(Socket, BinData),
    {ok, Role};
do(_Role, Data) ->
    ?ERROR_MSG("CMD 10003 is not match:~p", [Data]),
    {error, "CMD 10003 is not match"}.

%% @doc 回调处理操作
response(_V, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================.
