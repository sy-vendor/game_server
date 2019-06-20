%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(sup_kflogic).
-author("sy").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_link/1]).

-define(SERVER, ?MODULE).

-include("common.hrl").

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

start_link([StrHost, StrPort, StrId]) ->
  ?INFO("[~w] 正在启动监控树...", [?MODULE]),
  Host = list_to_binary(StrHost),
  Port = list_to_integer(StrPort),
  Id = list_to_integer(StrId),
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Id]).

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
init(Args = [_Host, _Port, _Id]) ->
  %% core
  List1 = get_core(Args),
  %% 逻辑模块
  List2 = get_mod(Args),
  %% 链接器
  List = get_acceptor(Args),
  {ok, {{one_for_one, 50, 1}, boot_misc:swap_sup_child(List1 ++ List2 ++ List)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 核心模块
get_core([Host, Port, Id]) ->
  [
    {svr_time, {svr_time, start_link, []}},
    {sup_kf_timer, {sup_kf_timer, start_link, []}},
    {svr_kf, {svr_kf, start_link, []}},
    {svr_node_kf, {svr_node_kf, start_link, [[Host, Port, Id]]}},
    {svr_kfcenter, {svr_kfcenter, start_link, []}}
  ].

%% 逻辑模块
get_mod([_Host, _Port, _Id]) ->
  [
  ].

%% 链接器模块
get_acceptor([_Host, _Port, _Id]) ->
  [
  ].
