-module(game_server_game_behaviour).
-author("suyang").

%% 游戏行为定义
-callback init(Args :: term()) -> {ok, State :: term()} | {error, Reason :: term()}.
-callback handle_join(PlayerId :: binary(), State :: term()) -> {ok, NewState :: term()} | {error, Reason :: term()}.
-callback handle_leave(PlayerId :: binary(), State :: term()) -> {ok, NewState :: term()} | {error, Reason :: term()}.
-callback handle_action(PlayerId :: binary(), Action :: term(), State :: term()) -> {ok, NewState :: term()} | {error, Reason :: term()}.
-callback get_state(State :: term()) -> term().
-callback get_players(State :: term()) -> [binary()].
-callback is_game_over(State :: term()) -> boolean().
-callback get_winner(State :: term()) -> {ok, Winner :: binary()} | {error, no_winner}.

%% 可选回调
-optional_callbacks([
    handle_timeout/2,
    handle_player_disconnect/2,
    handle_player_reconnect/2
]). 