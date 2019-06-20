%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% MapList辅助处理
%%% @end
%%% Created : 14. 五月 2019
%%%-------------------------------------------------------------------
-module(map).

%% API
-export([
    keyfind/3,
    keystore/4,
    keyreplace/4,
    keydelete/3,
    keytake/3
]).

%% @doc 查找（获取第一个K为指定值的元素，找不到的话返回false）
keyfind(V, K, L) when is_atom(K), is_list(L) ->
    keyfind2(V, K, L).

keyfind2(_V, _K, []) ->
    false;
keyfind2(V, K, [M | T]) ->
    case maps:find(K, M) of
        {ok, Val} when Val =:= V -> M;
        _ -> keyfind2(V, K, T)
    end.

%% @doc 保存（会覆盖第一个K为指定值的元素，找不到的话会加在列表的后面）
keystore(V, K, L, New) when is_atom(K), is_list(L), is_map(New) ->
    keystore2(V, K, L, New).

keystore2(V, K, [M | T], New) ->
    case maps:find(K, M) of
        {ok, Val} when Val =:= V -> [New | T];
        _ -> [M | keystore2(V, K, T, New)]
    end;
keystore2(_V, _K, [], New) ->
    [New].

%% @doc 替换（替换第一个K为指定值的元素，找不到的话返回原来的列表）
keyreplace(V, K, L, New) when is_atom(K), is_list(L), is_map(New) ->
    keyreplace3(V, K, L, New).

keyreplace3(V, K, [M | T], New) ->
    case maps:find(K, M) of
        {ok, Val} when Val =:= V -> [New | T];
        _ -> [M | keyreplace3(V, K, T, New)]
    end;
keyreplace3(_, _, [], _) -> [].

%% @doc 删除（删除第一个K为指定值的元素）
keydelete(V, K, L) when is_atom(K), is_list(L) ->
    keydelete3(V, K, L).

keydelete3(V, K, [M | T]) ->
    case maps:find(K, M) of
        {ok, Val} when Val =:= V -> T;
        _ -> [M | keydelete3(V, K, T)]
    end;
keydelete3(_, _, []) -> [].

%% @doc 取出（取出第一个K为指定值的元素，返回列表剩余的元素）
-spec keytake(V, K, MapList1) -> {value, M, MapList2} | false when
    V :: term(),
    K :: atom(),
    MapList1 :: [map()],
    MapList2 :: [map()],
    M :: tuple().

keytake(V, K, L) when is_atom(K), is_list(L) ->
    keytake(V, K, L, []).

keytake(V, K, [M | T], L) ->
    case maps:find(K, M) of
        {ok, Val} when Val =:= V ->
            {value, M, lists:reverse(L, T)};
        _ ->
            keytake(V, K, T, [M | L])
    end;
keytake(_K, _N, [], _L) -> false.