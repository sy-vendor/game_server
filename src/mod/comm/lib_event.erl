%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 事件触发处理
%%% @end
%%% Created : 27. 十二月 2018 14:48
%%%-------------------------------------------------------------------
-module(lib_event).
-author("suyang").

%% API
-export([role_login/3]).

-include("role.hrl").
-include("common.hrl").

%% @doc 玩家登陆
role_login(_IsDb, _NowTime, Role) ->
    Role.
