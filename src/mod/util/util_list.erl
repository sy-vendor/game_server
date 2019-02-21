%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% list类util
%%% @end
%%% Created : 07. 十二月 2018 12:26
%%%-------------------------------------------------------------------
-module(util_list).
-author("suyang").

-export([merge_kvp/1,
    add_index/2,
    add_index/4,
    dedup/1,
    count_elem/1,
    count_elem/2,
    disorder/1,
    keyadd/3,
    keyfinds/3,
    keyfind/5,
    keysort_desc/2,
    keysort_desc_ts/3,
    keysum/2,
    get_idx_val/5,
    pos/2,
    key_group/2,
    keydelete/3,
    split_list/2
]).


%% 合并相同的键值对
-spec merge_kvp(List :: list()) -> NewList :: list().
merge_kvp(List) -> merge_kvp(List, []).
merge_kvp([], Back) -> Back;
merge_kvp([H = [_|_] | T], Back) ->
    merge_kvp(T, merge_kvp(H, Back));
merge_kvp([{K, V} | R], Back) ->
    merge_kvp(R, keyadd(K, V, Back));
merge_kvp([_ | R], Back) ->
    merge_kvp(R, Back).

%% 给列表元素从1到N加上索引，列表需要先按需要排序好
-spec add_index(L :: list(), Index ::integer()) -> L1 :: list().
add_index(L, Index) ->
    add_index(L, Index, 1, []).
add_index([], _I, _No, Back) ->
    lists:reverse(Back);
add_index([H | T], I, No, Back) ->
    add_index(T, I, No + 1, [erlang:setelement(I, H, No) | Back]).

%% 去重
-spec dedup(L :: list()) -> NewL :: list().
dedup([]) -> [];
dedup(L) -> dedup(L, []).
dedup([], Back) -> lists:reverse(Back);
dedup([H | T], Back) ->
    case lists:member(H, Back) of
        false -> dedup(T, [H | Back]);
        _ -> dedup(T, Back)
    end.

%% 统计列表元素个数
-spec count_elem(List :: list(), KeyPos :: integer()) -> [{Key :: term(), Count :: integer()}].
count_elem(List) -> count_elem(List, 1).
count_elem(List, KeyPos) -> count_elem(List, KeyPos, []).
count_elem([], _, Back) -> Back;
count_elem([H | T], KeyPos, Back) when is_tuple(H) ->
    Key = erlang:element(KeyPos, H),
    count_elem(T, KeyPos, keyadd(Key, 1, Back));
count_elem([H | T], KeyPos, Back) ->
    count_elem(T, KeyPos, keyadd(H, 1, Back)).

%% 打乱元素顺序
disorder(List) ->
    Len = length(List),
    NL = lists:map(fun(X) -> {rand:uniform(Len), X} end, List),
    NLL = lists:sort(NL),
    [V || {_,V} <- NLL].

%% 二元元组根据key增加值
keyadd(Key, Val, List) ->
    case lists:keytake(Key, 1, List) of
        false -> [{Key, Val} | List];
        {value, {_, Val1}, List1} -> [{Key, Val + Val1} | List1]
    end.

%% 查找列表元素指定位置的值，没有返回默认值
keyfind(Key, Pos, List, TarPos, Def) ->
    case lists:keyfind(Key, Pos, List) of
        false -> Def;
        T -> element(TarPos, T)
    end.

%% 查找多个
keyfinds(Key, Pos, List) ->
    keyfinds(Key, Pos, List, []).
keyfinds(_Key, _Pos, [], Back) -> lists:reverse(Back);
keyfinds(Key, Pos, [H | T], Back) when Key == element(Pos, H) ->
    keyfinds(Key, Pos, T, [H | Back]);
keyfinds(Key, Pos, [_ | T], Back) ->
    keyfinds(Key, Pos, T, Back).

%% 计算列表指定位置的和，lists:sum/1 的key版本
keysum(Pos, List) ->
    lists:sum([element(Pos, T) || T <- List]).

%% 反自然序排列
keysort_desc(Index, List) ->
    Fun = fun(T1, T2) ->
        element(Index, T1) > element(Index, T2)
          end,
    lists:sort(Fun, List).

%% 反自然序排列，根据时间二次排序，时间早的优先
keysort_desc_ts(Index, Index2, List) ->
    Fun = fun(T1, T2) ->
        V1 = element(Index, T1), V2 = element(Index, T2),
        case V1 == V2 of
            false -> V1 > V2;
            _ -> element(Index2, T1) < element(Index2, T2)
        end
          end,
    lists:sort(Fun, List).

%% @spec get_idx_val(Key, KeyIndex, ValIndex, ValDefault, List) -> Val.
%% 获取列表元组指定位置的值
get_idx_val(Key, KeyIndex, ValIndex, ValDefault, List) ->
    case lists:keyfind(Key, KeyIndex, List) of
        false -> ValDefault;
        Tuple -> erlang:element(ValIndex, Tuple)
    end.

%% 获取指定元素在列表的位置
%% 0表示不存在
-spec pos(Elem :: term(), List :: list()) -> Pos :: integer() | 0.
pos(Elem, List) ->
    pos(Elem, List, 1).

pos(Elem, [Elem | _T], Pos) -> Pos;
pos(Elem, [_ | T], Pos) -> pos(Elem, T, Pos + 1);
pos(_, [], _) -> 0.


%% @doc 根据key将list分组
%% @spec key_group(KeyPos, List) -> List
%% eg key_group(1, [{1,2}, {1,3}, {2,1}, {3,1}]) -> [{1, [{1,2},{1,3}]}, {2, [{2,1}]}, {3, [{3,1}]}]
key_group(_, []) ->[];
key_group(KeyPos, List) ->
    key_group(KeyPos, List, []).
key_group(_, [], Result) -> Result;
key_group(KeyPos, [E | T], Result) ->
    Key = element(KeyPos, E),
    case lists:keytake(Key, 1, Result) of
        false ->
            key_group(KeyPos, T, [{Key, [E]} | Result]);
        {_, {Key, List}, Rest} ->
            key_group(KeyPos, T, [{Key, [E | List]} | Rest])
    end.


keydelete(K, N, L) when is_integer(N), N > 0 ->
    keydelete3(K, N, L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key ->
    keydelete3(Key, N, T);
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].

%% @doc 分割列表
split_list(N, List) when length(List) =< N ->
    {List, []};
split_list(N, List) ->
    lists:split(N, List).

