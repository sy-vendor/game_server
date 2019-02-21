%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十二月 2018 10:50
%%%-------------------------------------------------------------------
-module(rt_login).
-author("suyang").

%% API
-export([handle/3]).

-export([offline_reward/2, info/1, heart/1, login/1, new/1, change/3, recovery/2]).
-export([accept_order/2, delivery_order/2, reward_order/2]).
-export([speed_up/1]).
-export([waiter_unlock/2, waiter_intimate/2]).
-export([sign/2]).
-export([shop/2]).

-include("tester.hrl").
-include("common.hrl").
-include("logic.hrl").

%% 登录
offline_reward(Pid, Type) ->tester:cmd(Pid, 11007, #'OfflineRewardReq'{type = Type}).
info(Pid) -> tester:cmd(Pid, 11001, #'RoleInfoReq'{}).
heart(Pid) -> tester:cmd(Pid, 10001, #'HeartReq'{}).
login(Pid) -> tester:cmd(Pid, 10003, #'LoginReq'{}).
new(Pid) -> tester:cmd(Pid, 11101, #'ProduceNewReq'{}).
change(Pid, Start, End) -> tester:cmd(Pid, 11103, #'MergeExchangeReq'{posOne = Start, posTwo = End}).
recovery(Pid, Pos) -> tester:cmd(Pid, 11105, #'RecoveryObjectReq'{pos = Pos}).
accept_order(Pid, Id) -> tester:cmd(Pid, 11503, #'OrderAcceptReq'{orderId = Id}).
delivery_order(Pid, Id) -> tester:cmd(Pid, 11504, #'OrderDeliveryReq'{orderId = Id}).
reward_order(Pid, Id) -> tester:cmd(Pid, 11505, #'OrderRewardReq'{orderId = Id}).
speed_up(Pid) -> tester:cmd(Pid, 11703, #'SpeedUpReq'{}).
waiter_unlock(Pid, Type) -> tester:cmd(Pid, 11603, #'WaiterUnlockReq'{type = Type}).
waiter_intimate(Pid, Id) -> tester:cmd(Pid, 11604, #'WaiterIntimateReq'{id = Id}).
sign(Pid, Type) -> tester:cmd(Pid, 11203, #'SignRewardReq'{type = Type}).
shop(Pid, Type) -> tester:cmd(Pid, 11110, #'BuyObjectReq'{type = Type}).


%% @doc 心跳测试
handle(10001, #'HeartReq'{}, _Test) ->
    tester:pack_send(10001, #'HeartReq'{}),
    {ok};

%%handle(10002, #'HeartRes'{time = _Time}, #tester{name = _Name}) ->
%%    {ok};

%% @doc 登录测试
handle(10003, #'LoginReq'{}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("login account[~s], name:~s", [Account, Name]),
    tester:pack_send(10003, #'LoginReq'{tokenId = Account, name = Name, headUrl = "guangzhou.myqcloud.com/userAvarta6.png", gender = 1}),
    {ok};

%%handle(10004, #'LoginRes'{success = Success, tokenId = TokenId}, #tester{name = Name}) ->
%%    Account = get(acc_name),
%%    ?DEBUG("login account[~s], name:~s, Success:~w, TokenId:~s", [Account, Name, Success, TokenId]),
%%    {ok};

%% @doc 离线奖励测试
handle(11007, #'OfflineRewardReq'{type = Type}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s] name:~s, role offline reward type:~p", [Account, Name, Type]),
    tester:pack_send(11007, #'OfflineRewardReq'{type = Type}),
    {ok};

handle(11008, #'OfflineRewardRes'{code = Code}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s] name:~s, role offline reward res code:~p", [Account, Name, Code]),
    {ok};

%% @doc 玩家个人信息请求测试
handle(11001, #'RoleInfoReq'{}, _Test) ->
    tester:pack_send(11001, #'RoleInfoReq'{}),
    {ok};

%% @doc 玩家个人信息返回测试
handle(11002, #'RoleInfoRes'{roleInfo = RoleInfo}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s] name:~s, role info res:~w", [Account, Name, RoleInfo]),
    {ok};

%% @doc 生成测试
handle(11101, #'ProduceNewReq'{}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("new account[~s], name:~s", [Account, Name]),
    tester:pack_send(11101, #'ProduceNewReq'{}),
    {ok};

handle(11102, #'ProduceNewRes'{posInfo = _PosInfo}, #tester{name = _Name, pid = _Pid}) ->
%%    Account = get(acc_name),
%%    ?DEBUG("new account[~s] Name:~s merge new PosInfo:~w, NewType:~w, Worth:~w", [Account, Name, PosInfo]),
    {ok};

%% @doc 交换和合并测试
handle(11103, Data, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("cheange account[~s], name:~s", [Account, Name]),
    tester:pack_send(11103, Data),
    {ok};

handle(11104, #'MergeExchangeRes'{posInfo = PosInfo}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("cheange account[~s] Name:~s merge new PosInfo:~w", [Account, Name, PosInfo]),
    {ok};

%% @doc 回收测试
handle(11105, #'RecoveryObjectReq'{pos = Pos}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s recovery pos:~p", [Account, Name, Pos]),
    tester:pack_send(11105, #'RecoveryObjectReq'{pos = Pos}),
    {ok};

handle(11106, #'RecoveryObjectRes'{posInfo = PosInfo}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("object recovery pos account[~s], name:~s, Pos:~w", [Account, Name, PosInfo]),
    {ok};


%%%% @doc 产出测试
%%handle(11107, #'ObjectProduceAssetRes'{pos = _Pos, asset = _Asset}, #tester{name = _Name}) ->
%%    {ok};
%%
%%%% @doc 相关变化测试
%%handle(11004, #'RoleAssetChangeRes'{}, #tester{name = _Name}) ->
%%    {ok};

%% @doc 订单接受请求测试
handle(11503, #'OrderAcceptReq'{orderId = Id}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s accept order id:~p", [Account, Name, Id]),
    tester:pack_send(11503, #'OrderAcceptReq'{orderId = Id}),
    {ok};

%% @doc 配送订单请求
handle(11504, #'OrderDeliveryReq'{orderId = Id}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s delivert order id:~p", [Account, Name, Id]),
    tester:pack_send(11504, #'OrderDeliveryReq'{orderId = Id}),
    {ok};

%% @doc 领取奖励测试
handle(11505, #'OrderRewardReq'{orderId = Id}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s reward order id:~p", [Account, Name, Id]),
    tester:pack_send(11505, #'OrderRewardReq'{orderId = Id}),
    {ok};

%% @doc 订单信息返回测试
handle(11502, #'OrderInfoRes'{orderInfo = OrderInfo}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s order info orderInfo:~w", [Account, Name, OrderInfo]),
    {ok};

%% @doc 服务员信息返回测试
handle(11602, #'WaiterInfoRes'{waiterInfo = WaiterInfo}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s waiter info waiterInfo:~w", [Account, Name, WaiterInfo]),
    {ok};

%% @doc 服务员解锁请求测试
handle(11603, #'WaiterUnlockReq'{type = Type}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s waiter type:~p", [Account, Name, Type]),
    tester:pack_send(11603, #'WaiterUnlockReq'{type = Type}),
    {ok};

%% @doc 服务员打赏请求测试
handle(11604, #'WaiterIntimateReq'{id = Id}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s waiter intimate id:~p", [Account, Name, Id]),
    tester:pack_send(11604, #'WaiterIntimateReq'{id = Id}),
    {ok};

%% @doc 服务员信息返回测试
handle(11605, #'WaiterIntimateRes'{waiterInfo = WaiterInfo}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s waiter intimate info waiterInfo:~w", [Account, Name, WaiterInfo]),
    {ok};

%% @doc 加速测试
handle(11703, #'SpeedUpReq'{}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s speed up", [Account, Name]),
    tester:pack_send(11703, #'SpeedUpReq'{}),
    {ok};

%%handle(11702, #'SpeedUpStateRes'{end_time = EndTime}, #tester{name = Name}) ->
%%    Account = get(acc_name),
%%    ?DEBUG("account[~s], name:~s speed up end time:~p", [Account, Name, EndTime]),
%%    {ok};

%% @doc 签到测试
handle(11203, #'SignRewardReq'{type = Type}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s sign type[~p]", [Account, Name, Type]),
    tester:pack_send(11203, #'SignRewardReq'{type = Type}),
    {ok};

handle(11202, #'SignInfoRes'{isSign = IsSign, totalSignCnt = TotalSignCnt}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s today is_sign[~p] total_sign_cnt[~p]", [Account, Name, IsSign, TotalSignCnt]),
    {ok};

%% @doc 购买测试
handle(11110, #'BuyObjectReq'{type = Type}, #tester{name = Name}) ->
    Account = get(acc_name),
    ?DEBUG("account[~s], name:~s buy type[~p]", [Account, Name, Type]),
    tester:pack_send(11110, #'BuyObjectReq'{type = Type}),
    {ok};

%% 容错处理
handle(_Cmd, _Data, #tester{name = _Name}) ->
    ?DEBUG("[~s]receive unknow data[~w]: ~w", [_Name, _Cmd, _Data]),
    {ok}.