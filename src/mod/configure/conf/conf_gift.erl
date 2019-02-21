%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 礼包配置
%%% @end
%%% Created : 14. 二月 2019 14:42
%%%-------------------------------------------------------------------
-module(conf_gift).
-author("suyang").

%% API
-export([get_gift_config/0,
    get_gift_condition/1,
    get_gift_diamond/1,
    get_gift_type/1]).

-include("configure.hrl").

%% @doc 配置表
get_gift_config() ->
    util_dist:get(?GIFT_JSON, []).

%% @doc 获取奖励数
get_gift_diamond(Id) ->
    List = get_gift_config(),
    case lists:keyfind(Id, #gift_config.id, List) of
        #gift_config{diamond = Diamond} -> Diamond;
        _ -> 0
    end.

%% @doc 获取限制条件
get_gift_condition(Id) ->
    List = get_gift_config(),
    case lists:keyfind(Id, #gift_config.id, List) of
        #gift_config{condition = Val} -> Val;
        _ -> 0
    end.

%% @doc 获取礼包种类
get_gift_type(Id) ->
    List = get_gift_config(),
    case lists:keyfind(Id, #gift_config.id, List) of
        #gift_config{type = Val} -> Val;
        _ -> 0
    end.
