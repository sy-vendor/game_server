%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 签到奖励配置
%%% @end
%%% Created : 08. 一月 2019 16:26
%%%-------------------------------------------------------------------
-module(conf_sign).
-author("suyang").

%% API
-export([
    get_sign_config/0,
    get_sign_reward/1
]).

-include("configure.hrl").


%% @doc 配置表信息
get_sign_config() ->
    util_dist:get(?SIGN_JSON, []).

%% @doc 获取奖励
get_sign_reward(Id) ->
    List = get_sign_config(),
    case lists:keyfind(Id, #sign_config.id, List) of
        #sign_config{common_sign = CommonSign, luxury_sign = LuxurySign} -> [CommonSign, LuxurySign];
        _ -> undefined
    end.
