-module(game_server_util).

-export([
    generate_id/0,
    timestamp/0,
    format_error/1,
    validate_input/2
]).

%% Generate a unique ID
generate_id() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_binary(Id).

%% Get current timestamp in milliseconds
timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + Micro div 1000.

%% Format error message
format_error(Reason) ->
    case Reason of
        {error, Msg} -> Msg;
        _ -> io_lib:format("~p", [Reason])
    end.

%% Validate input data
validate_input(Data, Schema) ->
    try
        validate_fields(Data, Schema),
        {ok, Data}
    catch
        throw:Error -> {error, Error}
    end.

validate_fields(_Data, []) -> ok;
validate_fields(Data, [{Field, Type} | Rest]) ->
    case maps:find(Field, Data) of
        {ok, Value} ->
            validate_type(Value, Type),
            validate_fields(Data, Rest);
        error ->
            throw({missing_field, Field})
    end.

validate_type(Value, Type) ->
    case Type of
        string when is_list(Value) -> ok;
        integer when is_integer(Value) -> ok;
        float when is_float(Value) -> ok;
        boolean when is_boolean(Value) -> ok;
        _ -> throw({invalid_type, Type})
    end. 