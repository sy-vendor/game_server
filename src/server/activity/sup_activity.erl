%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 活动监控管理进程
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(sup_activity).
-author("sy").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_activity/1, stop_activity/1, delete_child/1]).

-define(SERVER, ?MODULE).

-include("common.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================
start_activity(ChildSpec) ->
  supervisor:start_child(?MODULE, ChildSpec).

%% @doc 关闭活动
%%      注意：如果进程trap_exit，将terminate收到退出shutdown
stop_activity(ChildId) ->
  _ = supervisor:terminate_child(?MODULE, ChildId),
  _ = supervisor:delete_child(?MODULE, ChildId),
  ok.

delete_child(ChildId) ->
  _ = supervisor:delete_child(?MODULE, ChildId).

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

  NodeId = util:get_node_id(),
  SvrType = util:get_server_type(),

  Children = [
    {svr_activity_mgr, {svr_activity_mgr, start_link, [NodeId, SvrType]},
      permanent, 10000, worker, [svr_activity_mgr]}
  ],

  Extras = if
             SvrType =:= ?SERVER_TYPE_LOCAL -> % 10节点(游戏逻辑节点)
               [
                 % ===== 日常类活动 BEGIN =====
                 % ===== 日常类活动 END =====

                 % ===== 运营类活动 BEGIN =====
                 % ===== 运营类活动 END =====
               ];
             NodeId =:= ?NODE_ID_KFCLIENT -> % 0节点
               [];
             NodeId =:= ?NODE_ID_KFCENTER -> % 100节点
               [];
             NodeId =:= ?NODE_ID_KFGROUP -> % 200节点
               [
                 % 跨服分组进程
                 {svr_kfgroup, {svr_kfgroup, start_link, []},
                   permanent, 10000, worker, [svr_kfgroup]}
                 % ===== 运营类活动 BEGIN =====
               ];
             NodeId >= ?NODE_ID_KFLOGIC -> % 300节点
               [
                 % ===== 日常类活动 BEGIN =====
                 % ===== 日常类活动 END =====

                 % TO ADD
                 % ===== 运营类活动 BEGIN =====
                 % ===== 运营类活动 END =====
               ];
             true -> []
           end,

  {ok, {SupFlags, Children ++ Extras}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
