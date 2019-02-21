%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 宝箱
%%% @end
%%% Created : 21. 十二月 2018 18:00
%%%-------------------------------------------------------------------
-module(lib_box_api).
-author("suyang").

%% API
-export([box_reward/1]).

-include("common.hrl").
-include("role.hrl").
-include("err_code.hrl").
-include("logic.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 宝箱奖励领取
box_reward(Role) ->
    case catch check_box_reward(Role) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11005, #'BoxRewardRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Coin} ->
            RoleN = lib_role_asset:add_assets([{?TYPE_COIN, Coin}], Role),
            lib_data_api:put_value(RoleN#role.role_id, box_reward_time, util_time:unixtime()),
            % 协议推送
            {ok, Bin} = lib_proto:pack(11005, #'BoxRewardRes'{code = ?SUCCESS}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 宝箱奖励领取检查
check_box_reward(Role) ->
    RewardTime = lib_data_api:get_value(Role, box_reward_time, 0),
    Now = util_time:unixtime(),
    ?IF(Now - RewardTime > 15, ignore, erlang:throw({error, ?ERR_COMMON_REWARD_LIMIT})),
    Coin = get_reward_config(Role#role.role_lv),
    {true, Coin}.

%% @doc 配置文件
get_reward_config(1) -> 1000;
get_reward_config(2) -> 2000;
get_reward_config(3) -> 3000;
get_reward_config(4) -> 4000;
get_reward_config(5) -> 5000;
get_reward_config(6) -> 6000;
get_reward_config(7) -> 7000;
get_reward_config(8) -> 8000;
get_reward_config(_) -> 0.