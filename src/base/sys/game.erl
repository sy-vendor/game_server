%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 核心application
%%% @end
%%% Created : 29. 9月 2019 14:23
%%%-------------------------------------------------------------------
-module(game).
-author("sy").

-behaviour(application).

%% Application callbacks
-export([start/2,
  start/0,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  ok.

start() ->
  [IpStr, PortStr, IdStr] = init:get_plain_arguments(),
  Port = list_to_integer(PortStr),
  Id = list_to_integer(IdStr),
  case Id of
    10 ->
      {ok, Pid} = sup:start_link([IpStr, Port, Id]),
      ok = tpl_node_logic:start(),
      {ok, Pid};
    _ -> {error, args_not_match}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
