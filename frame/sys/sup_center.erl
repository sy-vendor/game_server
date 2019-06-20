%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 中央监控树
%%% @end
%%% Created : 25. 十二月 2018 18:26
%%%-------------------------------------------------------------------
-module(sup_center).
-author("suyang").

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
start_link([StrHost, StrPort, StrId]) ->
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
    %% 核心模块
    List1 = get_core(Args),
    %% 逻辑模块
    List2 = get_mod(Args),
    %% 链接器
    List3 = get_acceptor(Args),
    {ok, {{one_for_one, 50, 1}, boot_misc:swap_sup_child(List1 ++ List2 ++ List3)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 核心模块
get_core([_Host, _Port, _Id]) ->
    [
        % 系统变量管理器
        {sys_env, {sys_env, start_link, [?MODULE, center]}},
        % 数据库管理器
        {db_mgr, {db_mgr, start_link, []}},
        % 链接进程管理器
        {sys_conn_mgr, {sys_conn_mgr, start_link, []}},
        % 时间管理器
        {sup_time, {sup_time, start_link, []}}
    ].

%% @doc 逻辑模块
get_mod([_Host, _Port, _Id]) ->
    [
        % 中央节点管理
        {svr_center_mgr, {svr_center_mgr, start_link, []}}
    ].

%% @doc 链接器
get_acceptor([_Host, Port, _Id]) ->
    [
        {sup_acceptor, {sup_acceptor, start_link, []}, permanent, 10000, supervisor, [sup_acceptor]},
        % TCP监听
        {sys_listener, {sys_listener, start_link, [Port]}}
    ].