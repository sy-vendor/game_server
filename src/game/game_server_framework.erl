-module(game_server_framework).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    create_game/1,
    join_game/2,
    leave_game/2,
    get_game_info/1,
    list_games/0
]).

-record(game, {
    id,
    type,
    status = waiting,
    players = [],
    max_players = 2,
    created_at,
    updated_at
}).

-record(state, {
    games = #{}
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_game(GameType) ->
    gen_server:call(?MODULE, {create_game, GameType}).

join_game(GameId, PlayerId) ->
    gen_server:call(?MODULE, {join_game, GameId, PlayerId}).

leave_game(GameId, PlayerId) ->
    gen_server:call(?MODULE, {leave_game, GameId, PlayerId}).

get_game_info(GameId) ->
    gen_server:call(?MODULE, {get_game_info, GameId}).

list_games() ->
    gen_server:call(?MODULE, list_games).

%% Callback Functions
init([]) ->
    lager:info("Game framework initialized"),
    {ok, #state{}}.

handle_call({create_game, GameType}, _From, State) ->
    GameId = game_server_util:generate_id(),
    Now = game_server_util:timestamp(),
    Game = #game{
        id = GameId,
        type = GameType,
        created_at = Now,
        updated_at = Now
    },
    NewGames = maps:put(GameId, Game, State#state.games),
    lager:info("Game created: ~p", [GameId]),
    {reply, {ok, GameId}, State#state{games = NewGames}};

handle_call({join_game, GameId, PlayerId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            case can_join_game(Game, PlayerId) of
                true ->
                    UpdatedGame = add_player(Game, PlayerId),
                    NewGames = maps:update(GameId, UpdatedGame, State#state.games),
                    lager:info("Player ~p joined game ~p", [PlayerId, GameId]),
                    {reply, {ok, UpdatedGame}, State#state{games = NewGames}};
                false ->
                    {reply, {error, cannot_join}, State}
            end;
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({leave_game, GameId, PlayerId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            UpdatedGame = remove_player(Game, PlayerId),
            NewGames = maps:update(GameId, UpdatedGame, State#state.games),
            lager:info("Player ~p left game ~p", [PlayerId, GameId]),
            {reply, ok, State#state{games = NewGames}};
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({get_game_info, GameId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            {reply, {ok, Game}, State};
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call(list_games, _From, State) ->
    Games = maps:values(State#state.games),
    {reply, {ok, Games}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Game framework terminating"),
    ok.

%% @doc
%% 热升级时，Erlang会调用此回调以实现状态结构的平滑迁移。
%% 你可以在这里处理record字段变更、数据结构升级等。
%% 例：
%%   code_change(_OldVsn, State = #state{games=Games}, _Extra) ->
%%       %% 升级games结构
%%       {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions
can_join_game(Game, PlayerId) ->
    length(Game#game.players) < Game#game.max_players andalso
    not lists:member(PlayerId, Game#game.players).

add_player(Game, PlayerId) ->
    UpdatedPlayers = [PlayerId | Game#game.players],
    Game#game{
        players = UpdatedPlayers,
        updated_at = game_server_util:timestamp()
    }.

remove_player(Game, PlayerId) ->
    UpdatedPlayers = lists:delete(PlayerId, Game#game.players),
    Game#game{
        players = UpdatedPlayers,
        updated_at = game_server_util:timestamp()
    }. 