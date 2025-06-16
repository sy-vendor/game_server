-module(game_server_config).

-export([get/1, get/2, set/2]).

%% Default configuration values
-define(DEFAULT_CONFIG, #{
    port => 8080,
    max_connections => 1000,
    heartbeat_interval => 30000,
    game_timeout => 300000
}).

get(Key) ->
    get(Key, maps:get(Key, ?DEFAULT_CONFIG)).

get(Key, Default) ->
    case application:get_env(game_server, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

set(Key, Value) ->
    application:set_env(game_server, Key, Value). 