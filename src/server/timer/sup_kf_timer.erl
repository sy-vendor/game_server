%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 跨服定时器监控树进程
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(sup_kf_timer).
-author("sy").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 10000,
  Type = worker,

  AChild = [
    {svr_kf_timer_daily, {svr_kf_timer_daily, start_link, []}, Restart, Shutdown, Type, [svr_kf_timer_daily]},
    {svr_kf_timer_hour, {svr_kf_timer_hour, start_link, []}, Restart, Shutdown, Type, [svr_kf_timer_hour]},
    {svr_kf_timer_minute, {svr_kf_timer_minute, start_link, []}, Restart, Shutdown, Type, [svr_kf_timer_minute]},
    {svr_common_timer, {svr_common_timer, start_link, [kfcenter]}, Restart, Shutdown, Type, [svr_common_timer]}
  ],

  {ok, {SupFlags, AChild}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
