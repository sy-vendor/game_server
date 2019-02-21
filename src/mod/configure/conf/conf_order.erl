%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 订单配置表
%%% @end
%%% Created : 07. 一月 2019 15:31
%%%-------------------------------------------------------------------
-module(conf_order).
-author("suyang").

%% API
-export([
    get_config_list/0,
    get_order_time/1,
    get_order_material/1,
    get_order_reward/1
]).

-include("configure.hrl").
-include("role.hrl").


%% @doc 配置表
get_order_config() ->
    util_dist:get(?ORDER_JSON, []).

%% @doc 获取订单权重列表
get_config_list() ->
    List = get_order_config(),
    lists:map(fun(#orderFrom_config{id = Id, probability = Rate})-> {Id, Rate} end, List).

%% @doc 获取订单时间
get_order_time(Id) ->
    List = get_order_config(),
    case lists:keyfind(Id, #orderFrom_config.id, List) of
        #orderFrom_config{init_time = InitTime, ready_time = ReadyTime, delivery_time = DelivTime} -> [InitTime, ReadyTime, DelivTime];
        _ -> [0, 0, 0]
    end.

%% @doc 获取消耗材料配置
get_order_material(Id) ->
    List = get_order_config(),
    case lists:keyfind(Id, #orderFrom_config.id, List) of
        #orderFrom_config{demand_goods = Cost} -> Cost;
        _ -> undefined
    end.

%% @doc 获取奖励基础时间与口碑
get_order_reward(Id) ->
    List = get_order_config(),
    case lists:keyfind(Id, #orderFrom_config.id, List) of
        #orderFrom_config{reward_gold = RewardGold, reward_popularity = RewardPopularity} -> [RewardGold, RewardPopularity];
        _ -> undefined
    end.
