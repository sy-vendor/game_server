%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 协议处理
%%% @end
%%% Created : 18. 十二月 2018 17:12
%%%-------------------------------------------------------------------
-module(lib_merge_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("role.hrl").
-include("common.hrl").
-include("logic.hrl").


handle(11101, #'ProduceNewReq'{}, Role) ->
    lib_merge_api:rand_new_object(Role);

handle(11103, #'MergeExchangeReq'{posOne = PosOne, posTwo = PosTwo}, Role) ->
    lib_merge_api:merge_object(PosOne, PosTwo, Role);

handle(11105, #'RecoveryObjectReq'{pos = Pos}, Role) ->
    lib_merge_api:recovery_object(Pos, Role);

handle(11108, #'PosInfoReq'{}, Role) ->
    lib_merge_api:send_merge_info(Role);

handle(11110, #'BuyObjectReq'{type = Type}, Role) ->
    lib_shop_api:go_shopping(Type, Role);

handle(11112, #'ShopInfoReq'{}, Role) ->
    lib_shop_api:shop_info(Role);

%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.