%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十二月 2018 10:44
%%%-------------------------------------------------------------------
-module(login_test).
-author("suyang").

%% API
-export([test/2]).

-include("common.hrl").
-include("tester.hrl").

test(N, Pre) ->
    util_math:for(1, N, fun(Index) -> do_test(Index, Pre) end).

do_test(Index, Pre) ->
    util:sleep(50),
    spawn(fun() -> do_test_2(Index, Pre, 100) end).

do_test_2(_Index, _Pre, 0) -> ignore;
do_test_2(Index, Pre, Count) ->
    Pid = test:login(lists:concat([Pre, util_type:to_list(Index)]), lists:concat([Pre, "_", util_type:to_list(Index)])),
    util:sleep(100),
    test:logout(Pid),
    util:sleep(100),
    do_test_2(Index, Pre, Count - 1).