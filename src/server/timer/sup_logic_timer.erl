%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 游戏服定时器监控树进程
%%% @end
%%% Created : 29. 9月 2019 10:49
%%%-------------------------------------------------------------------
-module(sup_logic_timer).
-behaviour(supervisor).

-export([
    start_link/0
]).

-export([
    init/1
]).

%% =============================================================================
%% API 
%% =============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% =============================================================================
%% Supervisor Callbacks
%% =============================================================================

init([]) ->
    SupFlags = {one_for_one, 1000, 3600},

    Children = [
        {svr_logic_timer_daily, {svr_logic_timer_daily, start_link, []},
            permanent, 10000, worker, [svr_logic_timer_daily]},
        {svr_logic_timer_hour, {svr_logic_timer_hour, start_link, []},
            permanent, 10000, worker, [svr_logic_timer_hour]},
        {svr_logic_timer_minute, {svr_logic_timer_minute, start_link, []},
            permanent, 10000, worker, [svr_logic_timer_minute]},
        {svr_common_timer, {svr_common_timer, start_link, [logic]},
            permanent, 10000, worker, [svr_common_timer]}
    ],

    {ok, {SupFlags, Children}}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
