-module(game_server_game_framework).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    start_game/3,
    stop_game/1,
    join_game/2,
    leave_game/2,
    handle_action/3,
    get_game_state/1,
    list_games/0
]).

-record(game, {
    id,
    type,
    module,
    pid,
    players = [],
    state,
    created_at,
    started_at,
    ended_at
}).

-record(state, {
    games = #{}
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_game(GameType, Module, Args) ->
    gen_server:call(?MODULE, {start_game, GameType, Module, Args}).

stop_game(GameId) ->
    gen_server:call(?MODULE, {stop_game, GameId}).

join_game(GameId, PlayerId) ->
    gen_server:call(?MODULE, {join_game, GameId, PlayerId}).

leave_game(GameId, PlayerId) ->
    gen_server:call(?MODULE, {leave_game, GameId, PlayerId}).

handle_action(GameId, PlayerId, Action) ->
    gen_server:call(?MODULE, {handle_action, GameId, PlayerId, Action}).

get_game_state(GameId) ->
    gen_server:call(?MODULE, {get_game_state, GameId}).

list_games() ->
    gen_server:call(?MODULE, list_games).

%% Callback Functions
init([]) ->
    lager:info("Game framework initialized"),
    {ok, #state{}}.

handle_call({start_game, GameType, Module, Args}, _From, State) ->
    GameId = game_server_util:generate_id(),
    Now = game_server_util:timestamp(),
    
    case Module:init(Args) of
        {ok, GameState} ->
            {ok, Pid} = gen_server:start_link(?MODULE, [GameId, Module, GameState], []),
            
            Game = #game{
                id = GameId,
                type = GameType,
                module = Module,
                pid = Pid,
                state = GameState,
                created_at = Now
            },
            
            NewGames = maps:put(GameId, Game, State#state.games),
            lager:info("Game started: ~p", [GameId]),
            
            {reply, {ok, GameId}, State#state{games = NewGames}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop_game, GameId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            gen_server:stop(Game#game.pid),
            NewGames = maps:remove(GameId, State#state.games),
            lager:info("Game stopped: ~p", [GameId]),
            {reply, ok, State#state{games = NewGames}};
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({join_game, GameId, PlayerId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            case Game#game.module:handle_join(PlayerId, Game#game.state) of
                {ok, NewState} ->
                    NewGame = Game#game{
                        players = [PlayerId | Game#game.players],
                        state = NewState
                    },
                    NewGames = maps:put(GameId, NewGame, State#state.games),
                    lager:info("Player ~p joined game ~p", [PlayerId, GameId]),
                    {reply, {ok, GameId}, State#state{games = NewGames}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({leave_game, GameId, PlayerId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            case Game#game.module:handle_leave(PlayerId, Game#game.state) of
                {ok, NewState} ->
                    NewPlayers = lists:delete(PlayerId, Game#game.players),
                    case NewPlayers of
                        [] ->
                            % If no players left, stop the game
                            gen_server:stop(Game#game.pid),
                            NewGames = maps:remove(GameId, State#state.games),
                            lager:info("Game ~p stopped (no players left)", [GameId]),
                            {reply, ok, State#state{games = NewGames}};
                        _ ->
                            NewGame = Game#game{
                                players = NewPlayers,
                                state = NewState
                            },
                            NewGames = maps:put(GameId, NewGame, State#state.games),
                            lager:info("Player ~p left game ~p", [PlayerId, GameId]),
                            {reply, ok, State#state{games = NewGames}}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({handle_action, GameId, PlayerId, Action}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            case Game#game.module:handle_action(PlayerId, Action, Game#game.state) of
                {ok, NewState} ->
                    NewGame = Game#game{state = NewState},
                    NewGames = maps:put(GameId, NewGame, State#state.games),
                    {reply, ok, State#state{games = NewGames}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({get_game_state, GameId}, _From, State) ->
    case maps:find(GameId, State#state.games) of
        {ok, Game} ->
            State = Game#game.module:get_state(Game#game.state),
            {reply, {ok, State}, State};
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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 