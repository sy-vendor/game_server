%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 测试节点监控树
%%% @end
%%% Created : 11. 十二月 2018 11:12
%%%-------------------------------------------------------------------
-module(sup_tester).
-author("suyang").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

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
start_link([StrHost, StrPort]) ->
    ?INFO("[~w] start up tester supervisor1...", [?MODULE]),
    Host = list_to_binary(StrHost),
    Port = list_to_integer(StrPort),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port]);

start_link([StrHost, StrPort, TestEx]) ->
    ?INFO("[~w] start up tester supervisor2...", [?MODULE]),
    Host = list_to_binary(StrHost),
    Port = list_to_integer(StrPort),
    Texample = list_to_atom(TestEx),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Texample]).

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
init(Args) ->
    List1 = [
        {sys_env, {sys_env, start_link, [?MODULE, srv]}},
        {tester_mgr, {tester_mgr, start_link, Args}}
    ],

    {ok, {{one_for_one, 50, 1}, boot_misc:swap_sup_child(List1)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
