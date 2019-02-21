%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 公共配置表操作
%%% @end
%%% Created : 04. 一月 2019 17:35
%%%-------------------------------------------------------------------
-module(conf_common).
-author("suyang").

%% API
-export([
    get_common_config/0,
    get_order_lv/0,
    get_max_pos/0,
    get_speed_conf/0,
    get_speed_time/0
]).

-include("configure.hrl").

%% @doc 公共配置表信息
get_common_config() ->
    util_dist:get(?COMMON_JSON, []).

%% @doc 获取订单系统开放等级
get_order_lv() ->
    List = get_common_config(),
    case lists:keyfind(orderOpenLvl, #common_config.key, List) of
        #common_config{value = Val} -> Val;
        _ -> 1
    end.

%% @doc 获取最大格子数
get_max_pos() ->
    List = get_common_config(),
    case lists:keyfind(max_pos, #common_config.key, List) of
        #common_config{value = Val} -> Val;
        _R -> 0
    end.

%% @doc 获取加速信息
get_speed_conf() ->
    List = get_common_config(),
    case lists:keyfind(addSpeedDiamonds, #common_config.key, List) of
        #common_config{value = Val} -> Val;
        _R -> undefined
    end.

%% @doc 获取广告加速时间
get_speed_time() ->
    List = get_common_config(),
    case lists:keyfind(addSpeedAd, #common_config.key, List) of
        #common_config{value = Val} -> Val;
        _ -> 0
    end.