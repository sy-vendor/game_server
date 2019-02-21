%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 测试
%%% @end
%%% Created : 11. 十二月 2018 11:48
%%%-------------------------------------------------------------------
-module(test).
-author("suyang").

%% API
-export([login/2,
    login/4,
    logout/1,
    logout_by_acc/1,
    apply_one/4,
    apply_all/3,
    display/1,
    display/0,
    pack_send/3]).

-include("common.hrl").
-include("tester.hrl").

%% @spec login(Account, Name) -> pid()
%% Account = string()
%% 登录帐号
%% robot:login("test_1", test1, "127.0.0.1", 8001).
login(Account, Name) ->
%%    login(Account, Name, "129.28.129.192", 8001).
    login(Account, Name, "127.0.0.1", 8001).
login(Account, Name, Host, Port) ->
    login(Account, Name, undefined, Host, Port).
login(Acc, Name, ProtoMod, H, P) ->
    case tester:start(Acc, Name, ProtoMod, H, P) of
        {ok, Pid} ->
            rt_login:login(Pid),
            Pid;
        Else ->
            Else
    end.

%% @spec logout(Pid) ->
%% Pid = pid()
%% 退出登录
logout(Pid) ->
    tester:stop(Pid).

logout_by_acc(Acc) ->
    case ets:match_object(tester_online, #tester{acc_name = Acc, _ = '_'}) of
        [] -> ok;
        [#tester{pid = Pid} | _] -> logout(Pid)
    end.

apply_all(M, F, A) ->
    List = ets:tab2list(tester_online),
    Fun = fun(T) ->
        tester:apply(async, T#tester.pid, {M, F, A})
          end,
    lists:foreach(Fun, List).

apply_one(Acc, M, F, A) ->
    case ets:match_object(tester_online, #tester{acc_name = Acc, _ = '_'}) of
        [] -> ok;
        [#tester{pid = Pid}] -> tester:apply(async, Pid, {M, F, A})
    end.

display() ->
    display(ets:tab2list(tester_online)).
display(Acc) when is_binary(Acc) ->
    case ets:match_object(tester_online, #tester{acc_name = Acc, _ = '_'}) of
        [] -> ok;
        T -> display(T)
    end;
display([]) -> ok;
display([H | T]) ->
    ?INFO("~w~n", [?record_kv(H, tester)]),
    display(T).

%% @spec pack_send(Pid, Cmd, Data) ->
%% Pid = pid()
%% Cmd = integer()
%% Data = term()
%% 发送协议
pack_send(Pid, Cmd, Data) ->
    tester:pack_send(Pid, Cmd, Data).