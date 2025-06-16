-module(game_server_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    connections = #{},
    games = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:info("Game server worker initialized"),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    lager:debug("Received call: ~p", [_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    lager:debug("Received cast: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:debug("Received info: ~p", [_Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:info("Game server worker terminating: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    lager:info("Game server worker code change"),
    {ok, State}. 