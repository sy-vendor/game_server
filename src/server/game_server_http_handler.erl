-module(game_server_http_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    lager:info("Received HTTP request: ~p", [Req0]),
    
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    {ok, Req, NewState} = handle_request(Method, Path, Req0, State),
    
    {ok, Req, NewState}.

handle_request(<<"GET">>, <<"/health">>, Req, State) ->
    lager:info("Health check request received"),
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{status => <<"ok">>}), Req),
    {ok, Req1, State};

handle_request(<<"POST">>, <<"/api/game">>, Req, State) ->
    lager:info("Game API request received"),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    try
        Data = jsx:decode(Body, [return_maps]),
        lager:debug("Received game data: ~p", [Data]),
        Req2 = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>
        }, jsx:encode(#{status => <<"success">>}), Req1),
        {ok, Req2, State}
    catch
        _:Error ->
            lager:error("Failed to process game request: ~p", [Error]),
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"invalid_request">>}), Req1),
            {ok, Req2, State}
    end;

handle_request(_Method, _Path, Req, State) ->
    lager:warning("Unknown request: ~p ~p", [_Method, _Path]),
    Req1 = cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{error => <<"not_found">>}), Req),
    {ok, Req1, State}. 