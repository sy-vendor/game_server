-module(game_server_auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    case check_rate_limit(Req) of
        ok ->
            case authenticate_request(Req) of
                {ok, UserId} ->
                    {ok, Req, [{user_id, UserId} | Env]};
                {error, Reason} ->
                    handle_auth_error(Req, Reason)
            end;
        {error, Reason} ->
            handle_rate_limit_error(Req, Reason)
    end.

check_rate_limit(Req) ->
    IP = cowboy_req:peer(Req),
    case game_server_auth:check_rate_limit(IP) of
        ok -> ok;
        {error, _} = Error -> Error
    end.

authenticate_request(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        {<<"Bearer ", Token/binary>>, _} ->
            case game_server_auth:validate_token(Token) of
                {ok, #token{user_id = UserId}} ->
                    {ok, UserId};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, missing_token}
    end.

handle_auth_error(Req, Reason) ->
    {ok, Req2} = cowboy_req:reply(401, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        error => <<"unauthorized">>,
        reason => atom_to_binary(Reason, utf8)
    }), Req),
    {stop, Req2}.

handle_rate_limit_error(Req, Reason) ->
    {ok, Req2} = cowboy_req:reply(429, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        error => <<"too_many_requests">>,
        reason => atom_to_binary(Reason, utf8)
    }), Req),
    {stop, Req2}. 