-module(game_server_framework_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = game_server_framework:start_link(),
    Pid.

create_game_test() ->
    Pid = setup(),
    {ok, GameId} = game_server_framework:create_game("test"),
    ?assert(is_binary(GameId)).

join_and_leave_game_test() ->
    Pid = setup(),
    {ok, GameId} = game_server_framework:create_game("test"),
    PlayerId = "p1",
    {ok, _Game} = game_server_framework:join_game(GameId, PlayerId),
    ok = game_server_framework:leave_game(GameId, PlayerId).

get_game_info_test() ->
    Pid = setup(),
    {ok, GameId} = game_server_framework:create_game("test"),
    {ok, Game} = game_server_framework:get_game_info(GameId),
    ?assertMatch(#game{}, Game).

list_games_test() ->
    Pid = setup(),
    {ok, GameId} = game_server_framework:create_game("test"),
    {ok, Games} = game_server_framework:list_games(),
    ?assert(length(Games) >= 1). 