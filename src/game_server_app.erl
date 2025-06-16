-module(game_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start lager first
    application:start(lager),
    
    %% Log application start
    lager:info("Starting game server application"),
    
    case game_server_sup:start_link() of
        {ok, Pid} ->
            lager:info("Game server supervisor started successfully"),
            {ok, Pid};
        Error ->
            lager:error("Failed to start game server supervisor: ~p", [Error]),
            Error
    end.

stop(_State) ->
    lager:info("Stopping game server application"),
    ok. 