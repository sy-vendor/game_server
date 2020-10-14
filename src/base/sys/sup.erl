%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 节点监控树
%%% @end
%%% Created : 29. 9月 2019 14:56
%%%-------------------------------------------------------------------
-module(sup).
-author("sy").

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link([Ip, Port, Id]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Ip, Port, Id]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init(Args) ->
  MaxRestarts = 50,
  MaxSecondsBetweenRestarts = 1,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  CoreL = get_core(Args),
  AcceptorL = get_acceptor(Args),
  io:format("This is init sup node logic.....~n"),
  {ok, {SupFlags, boot_misc:swap_sup_child(CoreL ++ AcceptorL)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 核心模块
get_core([Ip, Port, Id]) ->
  [
    {svr_time, {svr_time, start_link, []}}
    , {svr_logic, {svr_logic, start_link, []}}
    , {svr_node, {svr_node, start_link, [Ip, Port, Id]}}
  ].

%% 链接器模块
get_acceptor([_Host, Port, _Id]) ->
  [
    {sup_acceptor, {sup_acceptor, start_link, []}, permanent, 10000, supervisor, [sup_acceptor]}
    ,{sys_listener, {sys_listener, start_link, [Port]}}
  ].