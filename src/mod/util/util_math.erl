%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 数学类util
%%% @end
%%% Created : 07. 十二月 2018 12:25
%%%-------------------------------------------------------------------
-module(util_math).
-author("suyang").

-export([
    floor/1,
    ceil/1,
    rand/2,
    rand_list/1,
    rand_list_m/2,
    rand_list_one/2,
    for/3,
    check_range/3,
    keyrand/2
]).

%% @spec floor(X) -> integer()
%% X = number()
%% @doc 取小于X的最大整数
floor(X) ->
    T = erlang:trunc(X),
    case X < T of
        true -> T - 1;
        _ -> T
    end.

%% @spec ceil(X) -> integer()
%% X = number()
%% @doc 取大于X的最小整数
ceil(X) ->
    T = erlang:trunc(X),
    case X > T of
        true -> T + 1;
        _ -> T
    end.

%% @spec rand(Min, Max) -> integer()
%% Min = integer()
%% Max = integer()
%% @doc 产生一个介于Min到Max之间的随机整数，已获取随机种子的进程调用
rand(Min, Min) -> Min;
rand(Min, Max) ->
    M = Min - 1,
    rand:uniform(Max - M) + M.

%% @spec rand_list(L::list()) -> null | term()
%% @doc 从一个list中随机取出一项，已获取随机种子的进程调用
rand_list([]) -> null;
rand_list([I]) -> I;
rand_list(List) ->
    Idx = rand(1, length(List)),
    get_term_from_list(List, Idx).

%% @spec rand_list(L::list(), N :: integer()) -> null | [term()]
%% @doc 从一个list中随机取出多项，已获取随机种子的进程调用
rand_list_m(N, L) ->
    rand_list_m(N, L, []).

rand_list_m(_, [], Back) -> Back;
rand_list_m(N, _, Back) when N =< 0 -> Back;
rand_list_m(N, List, Back) ->
    Idx = rand(1, length(List)),
    E = get_term_from_list(List, Idx),
    rand_list_m(N - 1, lists:delete(E, List), [E | Back]).

%% @spec rand_list(L::list(), Index::integer()) -> false | term()
%% @doc 从一个list中随机取出一项
%% Index为相对权重索引,
rand_list_one(List, Index) ->
    RandMax = lists:sum([element(Index, Val) || Val <- List]),
    RandomDomain = util_math:rand(1, RandMax),
    rand_list_one(RandomDomain, List, Index).
rand_list_one(RandomDomain, [H | T], Index) ->
    Domain = element(Index, H),
    case RandomDomain =< Domain of
        true -> H;
        false -> rand_list_one(RandomDomain - Domain, T, Index)
    end;
rand_list_one(_RandomDomain, [], _) ->
    false.

get_term_from_list([H | _T], 1) -> H;
get_term_from_list([_H | T], Idx) ->
    get_term_from_list(T, Idx - 1).

%% @spec for(Begin::integer(), End::integer(), Fun::function()) -> ok
%% @doc 模拟for循环
for(End, End, Fun) ->
    Fun(End),
    ok;
for(Begin, End, Fun) when Begin < End ->
    Fun(Begin),
    for(Begin + 1, End, Fun).

%% @spec check_range(Val, Min, Max) -> number()
%% Val = number()
%% Min = number()
%% Max = number()
%% @doc 取值范围限制
check_range(Val, Min, Max) ->
    if
        Val > Max -> Max;
        Val < Min -> Min;
        true -> Val
    end.

%% @doc 指定tuple中的权重位置随机选择一个
keyrand([], _Pos) -> null;
keyrand([I], _Pos) -> I;
keyrand(L, Pos) ->
    Sum = lists:foldl(fun(Tuple, Acc) -> erlang:element(Pos, Tuple) + Acc end, 0, L),
    Luck = util_math:rand(1, Sum),
    keyrand(L, Pos, Luck).

keyrand([H | L], Pos, Luck) ->
    Lottery = erlang:element(Pos, H),
    case Luck =< Lottery of
        true -> H;
        false -> keyrand(L, Pos, Luck - Lottery)
    end.

