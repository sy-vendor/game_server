-module(game_server_room_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    create_room/2,
    join_room/2,
    leave_room/2,
    get_room_info/1,
    list_rooms/0
]).

-record(room, {
    id,
    game_type,
    max_players,
    players = [],
    status = waiting,  % waiting | playing | finished
    created_at,
    started_at,
    ended_at
}).

-record(state, {
    rooms = #{}
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(GameType, MaxPlayers) ->
    gen_server:call(?MODULE, {create_room, GameType, MaxPlayers}).

join_room(RoomId, PlayerId) ->
    gen_server:call(?MODULE, {join_room, RoomId, PlayerId}).

leave_room(RoomId, PlayerId) ->
    gen_server:call(?MODULE, {leave_room, RoomId, PlayerId}).

get_room_info(RoomId) ->
    gen_server:call(?MODULE, {get_room_info, RoomId}).

list_rooms() ->
    gen_server:call(?MODULE, list_rooms).

%% Callback Functions
init([]) ->
    lager:info("Room manager initialized"),
    {ok, #state{}}.

handle_call({create_room, GameType, MaxPlayers}, _From, State) ->
    RoomId = game_server_util:generate_id(),
    Now = game_server_util:timestamp(),
    
    Room = #room{
        id = RoomId,
        game_type = GameType,
        max_players = MaxPlayers,
        created_at = Now
    },
    
    NewRooms = maps:put(RoomId, Room, State#state.rooms),
    lager:info("Room created: ~p", [RoomId]),
    
    {reply, {ok, RoomId}, State#state{rooms = NewRooms}};

handle_call({join_room, RoomId, PlayerId}, _From, State) ->
    case maps:find(RoomId, State#state.rooms) of
        {ok, Room} ->
            case Room#room.status of
                waiting ->
                    case length(Room#room.players) < Room#room.max_players of
                        true ->
                            NewPlayers = [PlayerId | Room#room.players],
                            NewRoom = Room#room{players = NewPlayers},
                            NewRooms = maps:put(RoomId, NewRoom, State#state.rooms),
                            lager:info("Player ~p joined room ~p", [PlayerId, RoomId]),
                            {reply, {ok, RoomId}, State#state{rooms = NewRooms}};
                        false ->
                            {reply, {error, room_full}, State}
                    end;
                _ ->
                    {reply, {error, game_already_started}, State}
            end;
        error ->
            {reply, {error, room_not_found}, State}
    end;

handle_call({leave_room, RoomId, PlayerId}, _From, State) ->
    case maps:find(RoomId, State#state.rooms) of
        {ok, Room} ->
            case Room#room.status of
                waiting ->
                    NewPlayers = lists:delete(PlayerId, Room#room.players),
                    case NewPlayers of
                        [] ->
                            % If no players left, remove the room
                            NewRooms = maps:remove(RoomId, State#state.rooms),
                            lager:info("Room ~p removed (no players left)", [RoomId]),
                            {reply, ok, State#state{rooms = NewRooms}};
                        _ ->
                            NewRoom = Room#room{players = NewPlayers},
                            NewRooms = maps:put(RoomId, NewRoom, State#state.rooms),
                            lager:info("Player ~p left room ~p", [PlayerId, RoomId]),
                            {reply, ok, State#state{rooms = NewRooms}}
                    end;
                _ ->
                    {reply, {error, cannot_leave_during_game}, State}
            end;
        error ->
            {reply, {error, room_not_found}, State}
    end;

handle_call({get_room_info, RoomId}, _From, State) ->
    case maps:find(RoomId, State#state.rooms) of
        {ok, Room} ->
            {reply, {ok, Room}, State};
        error ->
            {reply, {error, room_not_found}, State}
    end;

handle_call(list_rooms, _From, State) ->
    Rooms = maps:values(State#state.rooms),
    {reply, {ok, Rooms}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Room manager terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 