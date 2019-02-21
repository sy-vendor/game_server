%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 合成配置表
%%% @end
%%% Created : 07. 一月 2019 15:23
%%%-------------------------------------------------------------------
-module(conf_building).
-author("suyang").

%% API
-export([
    get_merge_config/0,
    get_recovery_config/1,
    get_config_production/1,
    get_grow_value1/1,
    get_config_offline/1,
    get_grow_value2/1
]).

-include("configure.hrl").

%% @doc 配置表信息
get_merge_config() ->
    util_dist:get(?BUILD_JSON, []).

%% @doc 获取回收收益初始值
get_recovery_config(Type) ->
    List = get_merge_config(),
    case lists:keyfind(Type, #building_config.id, List) of
        #building_config{recovery = Cost} -> Cost;
        _ -> undefined
    end.

%% @doc 获取产出收益
get_config_production(Type) ->
    List = get_merge_config(),
    case lists:keyfind(Type, #building_config.id, List) of
        #building_config{yield = Prod} -> Prod;
        _ -> 0
    end.

%% @doc 获取离线收益
get_config_offline(Type) ->
    List = get_merge_config(),
    case lists:keyfind(Type, #building_config.id, List) of
        #building_config{yield_offline = Val} -> Val;
        _ -> 0
    end.

%% @doc 获取金币成长倍率
get_grow_value1(Type) ->
    List = get_merge_config(),
    case lists:keyfind(Type, #building_config.id, List) of
        #building_config{growth_value = Grow, start_value = Init} -> {Init, Grow};
        _ -> undefined
    end.

%% @doc 获取钻石成长倍率
get_grow_value2(Type) ->
    List = get_merge_config(),
    case lists:keyfind(Type, #building_config.id, List) of
        #building_config{growth_diamond = Grow, start_diamond = Init} -> {Init, Grow};
        _ -> undefined
    end.
