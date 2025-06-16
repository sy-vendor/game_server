-module(game_server_util_tests).
-include_lib("eunit/include/eunit.hrl").

%% 测试 generate_id/0
id_test() ->
    Id = game_server_util:generate_id(),
    ?assert(is_binary(Id)),
    ?assert(byte_size(Id) > 0).

%% 测试 timestamp/0
stamp_test() ->
    T = game_server_util:timestamp(),
    ?assert(is_integer(T)),
    ?assert(T > 0).

%% 测试 format_error/1
format_error_test() ->
    ?assertEqual("error_msg", game_server_util:format_error({error, "error_msg"})),
    ?assertMatch([_], game_server_util:format_error(badarg)).

%% 测试 validate_input/2
validate_input_test() ->
    Schema = [{name, string}, {age, integer}],
    Data = #{name => "Tom", age => 18},
    ?assertMatch({ok, _}, game_server_util:validate_input(Data, Schema)),
    ?assertMatch({error, _}, game_server_util:validate_input(#{name => "Tom"}, Schema)),
    ?assertMatch({error, _}, game_server_util:validate_input(#{name => 1, age => 18}, Schema)). 