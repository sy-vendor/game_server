%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 技能配置表
%%% @end
%%% Created : 07. 一月 2019 15:51
%%%-------------------------------------------------------------------
-module(conf_skill).
-author("suyang").

%% API
-export([
    get_skill_intimacy/2,
    get_skill_add/2
]).

-include("configure.hrl").

%% @doc 配置表相关
get_skill_config() ->
    util_dist:get(?SKILL_JSON, []).

%% @doc 获取亲密度升级相关
get_skill_intimacy(Type, Lv) ->
    List = get_skill_config(),
    SkillList = lists:filter(fun(#skill_config{type = ConfType}) -> ConfType =:= Type end, List),
    case lists:keyfind(Lv, #skill_config.lv, SkillList) of
        #skill_config{intimacy = Intimacy, intimacy_once = IntimacyOnce, reward_consume = RewardConsume} -> [Intimacy, IntimacyOnce, RewardConsume];
        _ -> undefined
    end.

%% @doc 获取技能加成
get_skill_add(Type, Lv) ->
    List = get_skill_config(),
    SkillList = lists:filter(fun(#skill_config{type = ConfType}) -> ConfType =:= Type end, List),
    case lists:keyfind(Lv, #skill_config.lv, SkillList) of
        #skill_config{skill_addition = SkillAddition} -> SkillAddition;
        _ -> 0
    end.