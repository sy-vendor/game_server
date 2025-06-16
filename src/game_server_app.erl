-module(game_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:start(),
    
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
    
    case game_server_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    cowboy:stop_listener(http_listener),
    cowboy:stop_listener(ws_listener),
    ok.