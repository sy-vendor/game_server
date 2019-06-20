%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 玩家账号检测
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(proto_hander_10001).
-author("sy").

%% API
-export([do/2, response/2]).

-include("role.hrl").
-include("common.hrl").

-record(cs_login_verify, {
    acc_id = 0,
    accname = "",
    server_num = 0,
    time_stamp = 0,
    ticket = 0
}).

-record(sc_login_verify, {
    res = 0,
    time_stamp = 0
}).

%% =============================================================================
%% API
%% =============================================================================
do(Role = #role{socket = Scoket}, Data) ->
    NowTime = time:unixtime(),
    case is_bad_pass(Data) of
        true ->
            #cs_login_verify{
                acc_id = AccId,
                accname = AccName,
                server_num = ServerNum
            } = Data,
            {ok, BinData} = lib_proto:encode_msg(10002, #sc_login_verify{res = 1,  time_stamp = NowTime}),
            lib_send:send_to_socket(Scoket, BinData),
            {ok, Role#role{login_flag = 1, acc_id = AccId, accname = AccName, server_num = ServerNum}};
        false ->
            {ok, BinData} = lib_proto:encode_msg(10002, #sc_login_verify{res = 0,  time_stamp = NowTime}),
            lib_send:send_to_socket(Scoket, BinData),
            {ok, Role}
    end;
do(_Role, Data) ->
    ?ERROR_MSG("CMD 10001 is not match:~p", [Data]),
    {error, "CMD 10001 is not match"}.

%% @doc 回调处理操作
response(_V, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% 通行证验证
is_bad_pass(Data) ->
    #cs_login_verify{acc_id = AccId, accname = AccName, time_stamp = Timestamp, ticket = CTicket} = Data,
    TICKET = config:get_ticket(),
    Hex = util:md5(lists:concat([AccId, AccName, Timestamp, TICKET])),
    Hex =:= CTicket.
