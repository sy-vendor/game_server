-module(game_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {game_server_auth,
         {game_server_auth, start_link, []},
         permanent, 5000, worker, [game_server_auth]},
        {game_server_room_manager,
         {game_server_room_manager, start_link, []},
         permanent, 5000, worker, [game_server_room_manager]},
        {game_server_game_framework,
         {game_server_game_framework, start_link, []},
         permanent, 5000, worker, [game_server_game_framework]},
        {game_server_framework,
         {game_server_framework, start_link, []},
         permanent, 5000, worker, [game_server_framework]}
    ]}}. 