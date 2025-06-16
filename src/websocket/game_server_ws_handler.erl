-module(game_server_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
    user_id,
    token,
    room_id,
    game_pid
}).

init(Req, _Opts) ->
    case authenticate_ws_connection(Req) of
        {ok, UserId, Token} ->
            {cowboy_websocket, Req, #state{user_id = UserId, token = Token}};
        {error, Reason} ->
            {ok, cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                error => <<"unauthorized">>,
                reason => atom_to_binary(Reason, utf8)
            }), Req), #state{}}
    end.

websocket_init(State) ->
    lager:info("WebSocket connection established for user ~p", [State#state.user_id]),
    {ok, State}.

websocket_handle({text, Data}, State) ->
    try
        Json = jsx:decode(Data, [return_maps]),
        handle_ws_message(Json, State)
    catch
        _:_ ->
            {reply, {text, jsx:encode(#{
                error => <<"invalid_message">>
            })}, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({game_message, Message}, State) ->
    {reply, {text, jsx:encode(Message)}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    case State#state.game_pid of
        undefined -> ok;
        Pid -> game_server_framework:leave_game(Pid, State#state.user_id)
    end,
    ok.

%% Private Functions
authenticate_ws_connection(Req) ->
    case cowboy_req:parse_qs(Req) of
        [{<<"token">>, Token} | _] ->
            case game_server_auth:validate_token(Token) of
                {ok, #token{user_id = UserId}} ->
                    {ok, UserId, Token};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, missing_token}
    end.

handle_ws_message(#{<<"type">> := <<"join_room">>, <<"room_id">> := RoomId}, State) ->
    case game_server_framework:join_game(RoomId, State#state.user_id) of
        {ok, GamePid} ->
            {ok, State#state{room_id = RoomId, game_pid = GamePid}};
        {error, Reason} ->
            {reply, {text, jsx:encode(#{
                error => <<"join_failed">>,
                reason => atom_to_binary(Reason, utf8)
            })}, State}
    end;

handle_ws_message(#{<<"type">> := <<"game_action">>, <<"action">> := Action}, State) ->
    case State#state.game_pid of
        undefined ->
            {reply, {text, jsx:encode(#{
                error => <<"not_in_game">>
            })}, State};
        GamePid ->
            case game_server_framework:handle_action(GamePid, State#state.user_id, Action) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {reply, {text, jsx:encode(#{
                        error => <<"action_failed">>,
                        reason => atom_to_binary(Reason, utf8)
                    })}, State}
            end
    end;

handle_ws_message(_Message, State) ->
    {reply, {text, jsx:encode(#{
        error => <<"unknown_message_type">>
    })}, State}. 