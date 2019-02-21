%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十二月 2018 11:01
%%%-------------------------------------------------------------------
-module(game).
-author("suyang").

-behaviour(application).

%% Application callbacks
-export([start/2,
    stop/1]).

-export([stop/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
stop() ->
    stop(normal).

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
    % 添加日志文件
    error_logger:logfile({open, lists:concat(["../log/node/error_", node(), ".log"])}),
    % 启动HTTP服务
    inets:start(),
    ssl:start(),
    case init:get_plain_arguments() of
        [H | T] ->
            Mod = list_to_atom("sup_" ++ H),
            {ok, Pid} = Mod:start_link(T),
            {ok, Pid};
        _ ->
            {error, args_error}
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
