-module(game_server_behaviour).

-callback init(GameType :: term()) ->
    {ok, GameState :: term()} | {error, Reason :: term()}.

-callback handle_action(Action :: term(), PlayerId :: term(), GameState :: term()) ->
    {ok, NewGameState :: term()} | {error, Reason :: term()}.

-callback get_state(GameState :: term()) ->
    {ok, PublicState :: term()}.

-callback validate_action(Action :: term(), PlayerId :: term(), GameState :: term()) ->
    valid | {error, Reason :: term()}.

-callback check_game_over(GameState :: term()) ->
    {continue, GameState :: term()} | {game_over, Result :: term()}.

-optional_callbacks([
    handle_player_join/2,
    handle_player_leave/2
]). 