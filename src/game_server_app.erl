-module(game_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start lager first
    application:start(lager),
    
    %% Log application start
    lager:info("Starting game server application"),
    
    %% Start cowboy
    {ok, _} = application:ensure_all_started(cowboy),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/games", game_server_http_handler, []},
            {"/ws", game_server_ws_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, 100,
        #{port => application:get_env(game_server, http_port, 8080)},
        #{env => #{dispatch => Dispatch},
          middlewares => [game_server_auth_middleware, cowboy_router, cowboy_handler]}
    ),
    
    {ok, _} = cowboy:start_clear(ws_listener, 100,
        #{port => application:get_env(game_server, ws_port, 8081)},
        #{env => #{dispatch => Dispatch}}
    ),
    
    lager:info("Cowboy server started on port ~p", [application:get_env(game_server, http_port, 8080)]),
    
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
    cowboy:stop_listener(http_listener),
    cowboy:stop_listener(ws_listener),
    ok. 