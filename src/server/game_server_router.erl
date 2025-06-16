-module(game_server_router).

-export([routes/0]).

routes() ->
    [
        {"/", game_server_http_handler, []},
        {"/health", game_server_http_handler, []},
        {"/api/game", game_server_http_handler, []},
        {"/ws", game_server_ws_handler, []}
    ]. 