%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 订单协议处理
%%% @end
%%% Created : 03. 一月 2019 15:57
%%%-------------------------------------------------------------------
-module(lib_order_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("role.hrl").
-include("common.hrl").
-include("logic.hrl").

%% 订单信息请求
handle(11501, #'OrderInfoReq'{}, Role) ->
    lib_order_api:send_order_info(Role),
    {ok};
%% 接单请求
handle(11503, #'OrderAcceptReq'{orderId = Id}, Role) ->
    lib_order_api:accept_order(Role, Id);
%% 配送请求
handle(11504, #'OrderDeliveryReq'{orderId = OrderId}, Role) ->
    lib_order_api:delivery_order(Role, OrderId);
%% 奖励请求
handle(11505, #'OrderRewardReq'{orderId = OrderId}, Role) ->
    lib_order_api:reward_order(Role, OrderId);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.
