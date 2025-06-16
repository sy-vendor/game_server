-module(game_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start lager first
    application:start(lager),
    
    %% Log application start
    lager:info("Starting game server application"),
    
    %% Start cowboy
    {ok, _} = cowboy:start_clear(http, [
        {port, game_server_config:get(port, 8080)}
    ], #{
        env => #{dispatch => cowboy_router:compile([
            {'_', game_server_router:routes()}
        ])}
    }),
    
    lager:info("Cowboy server started on port ~p", [game_server_config:get(port, 8080)]),
    
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
    cowboy:stop_listener(http),
    ok. 