-module(game_server_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
    player_id = undefined,
    game_id = undefined
}).

init(Req, _Opts) ->
    lager:info("WebSocket connection request received"),
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    lager:info("WebSocket connection established"),
    {ok, State}.

websocket_handle({text, Message}, State) ->
    lager:debug("Received WebSocket message: ~p", [Message]),
    try
        Data = jsx:decode(Message, [return_maps]),
        handle_ws_message(Data, State)
    catch
        _:Error ->
            lager:error("Failed to process WebSocket message: ~p", [Error]),
            {reply, {text, jsx:encode(#{error => <<"invalid_message">>})}, State}
    end;

websocket_handle(_Frame, State) ->
    lager:warning("Received unknown WebSocket frame: ~p", [_Frame]),
    {ok, State}.

websocket_info({game_update, GameId, Update}, State) ->
    lager:debug("Sending game update: ~p", [Update]),
    {reply, {text, jsx:encode(Update)}, State};

websocket_info(_Info, State) ->
    lager:warning("Received unknown WebSocket info: ~p", [_Info]),
    {ok, State}.

terminate(Reason, _Req, _State) ->
    lager:info("WebSocket connection terminated: ~p", [Reason]),
    ok.

handle_ws_message(#{<<"type">> := <<"join">>, <<"game_id">> := GameId}, State) ->
    lager:info("Player joining game: ~p", [GameId]),
    {reply, {text, jsx:encode(#{status => <<"joined">>})}, State#state{game_id = GameId}};

handle_ws_message(#{<<"type">> := <<"action">>, <<"action">> := Action}, State) ->
    lager:info("Player action received: ~p", [Action]),
    {reply, {text, jsx:encode(#{status => <<"action_received">>})}, State};

handle_ws_message(_Message, State) ->
    lager:warning("Unknown message type received: ~p", [_Message]),
    {reply, {text, jsx:encode(#{error => <<"unknown_message_type">>})}, State}. 