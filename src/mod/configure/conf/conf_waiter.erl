%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 服务员配置表
%%% @end
%%% Created : 07. 一月 2019 15:51
%%%-------------------------------------------------------------------
-module(conf_waiter).
-author("suyang").

%% API
-export([
    get_waiter_config/0,
    get_waiter_unlock/1,
    get_waiter_skill/1
]).

-include("configure.hrl").


%% @doc 配置表相关
get_waiter_config() ->
    util_dist:get(?WAITER_JSON, []).

%% @doc  获取服务员解锁消耗
get_waiter_unlock(Type) ->
    List = get_waiter_config(),
    case lists:keyfind(Type, #waiter_config.id, List) of
        #waiter_config{unlockCondition = Cost} -> Cost;
        _ -> undefined
    end.

%% @doc 获取技能Id
get_waiter_skill(Type) ->
    List = get_waiter_config(),
    case lists:keyfind(Type, #waiter_config.id, List) of
        #waiter_config{skill = Skill} -> Skill;
        _ -> undefined
    end.
