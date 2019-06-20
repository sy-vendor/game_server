%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 通用工具模块
%%% @end
%%% Created : 14. 五月 2019
%%%-------------------------------------------------------------------

-module(util).
-include("common.hrl").

%% 数据库辅助相关
-export([
    batch_insert/5,
    batch_insert/6,
    batch_insert/7,
    insert_values/4,
    insert_values/5,
    insert_values/6,
    format_values/2
]).

%% 服务器时间相关
-export([
    c_open_time/1,
    get_open_time/0,
    get_open_days/0,
    get_open_date/0,
    get_date_since_open/1,
    is_merge/0,
    c_merge_time/1,
    get_start_time/0,
    get_merge_time/0,
	get_shutdown_tick/0,
    get_merge_count/0,
    get_merge_date/0,
    get_merge_days/0,
    get_merge_days/1,
    get_date_since_merge/1
]).

%% 列表 [lists, proplists]
-export([
    prop_get_value/2,
    prop_get_value/3,
    prop_get_value/4,
    prop_get_keys/1,
    prop_delete/2,
    prop_store/2,
    prop_store/3,
    list_rand/1,
    list_rand_n/2,
    merge_list/2,
	multi_item_list/3,
	acc_attr_list/2,
	acc_attr_list/3,
	max_attr_list/2
]).

-compile(export_all).

%% ----------------------------------------------------------
%% 数据处理相关
%% ----------------------------------------------------------

%% @doc 将record转换为相应的tuple
%% eg. #user_id{platform_id = 0, server_id = 0, role_id = 45} -> {0, 0, 45}
record_to_tuple(Record, RecordDefine) ->
    [RecordDefine | ListData] = erlang:tuple_to_list(Record),
    erlang:list_to_tuple(ListData).

%% @doc 将tuple转换为相应的record
%% eg. {0, 0, 45} -> #user_id{platform_id = 0, server_id = 0, role_id = 45}
tuple_to_record(List, RecordDefine) ->
    erlang:list_to_tuple([RecordDefine | erlang:tuple_to_list(List)]).

%% @doc 将record转换为相应的list
%% eg. #user_id{platform_id = 0, server_id = 0, role_id = 45} -> [0, 0, 45]
record_to_list(Record, RecordDefine) ->
    [RecordDefine | ListData] = erlang:tuple_to_list(Record),
    ListData.

%% @doc 将record转换为相应的list
%% eg. #user_id{platform_id = 0, server_id = 0, role_id = 45} -> [{1, 0}, {2, 0}, {3, 45}]
record_to_list2(Record, RecordDefine) ->
    [RecordDefine | ListData] = erlang:tuple_to_list(Record),
    record_to_list3(ListData, 1, []).
record_to_list3([], _Index, ResList) -> ResList;
record_to_list3([Key|L], Index, ResList) ->
    record_to_list3(L, Index + 1, [{Index, Key}|ResList]).

%% @doc 将list转换为相应的record
%% eg. [0, 0, 45] -> #user_id{platform_id = 0, server_id = 0, role_id = 45}
list_to_record(List, RecordDefine) ->
    erlang:list_to_tuple([RecordDefine | List]).

%% @doc 将二维List转为一维Tuple
%% [[1,2], [1,2]] -> [{1, 2}, {1, 2}]
list_to_tuple(List) when erlang:is_list(List) ->
    lists:map(fun(A) ->
        if
            erlang:is_list(A) -> erlang:list_to_tuple(A);
            erlang:is_tuple(A) -> A;
            true -> {A}
        end
    end, List);
list_to_tuple(List) when erlang:is_tuple(List) ->
    lists:map(fun(A) ->
        if
            erlang:is_list(A) -> erlang:list_to_tuple(A);
            erlang:is_tuple(A) -> A;
            true -> {A}
        end
    end, erlang:tuple_to_list(List));
list_to_tuple(List) -> List.

% @doc 合并两个列表，相同键的值相加
%      短的列表放在前面
% @spec L1 = L2      :: {term(), numeric()};
merge_list([], L2) -> L2;
merge_list([{K, V1} | RestL1], L2) ->
    NewL2 = case lists:keytake(K, 1, L2) of
        {value, {K, V2}, L3} -> [{K, V1 + V2} | L3];
        _ -> [{K, V1} | L2]
    end,
    merge_list(RestL1, NewL2);
merge_list([{K, V1, V2} | RestL1], L2) ->
    NewL2 = case lists:keytake(K, 1, L2) of
        {value, {K, OldV1, OldV2}, L3} -> [{K, V1 + OldV1, V2 + OldV2} | L3];
        _ -> [{K, V1, V2} | L2]
    end,
    merge_list(RestL1, NewL2).

%% 物品倍数
multi_item_list([], ItemList, _Multi) ->
	ItemList;
multi_item_list([{ID, Count}|List], ItemList, Multi) ->
	case lists:keyfind(ID, 1, ItemList) of
		false ->
			multi_item_list(List, [{ID, util:floor(Count * Multi)}|ItemList], Multi);
		{ID, Old} ->
			ItemList1 = lists:keydelete(ID, 1, ItemList),
			multi_item_list(List, [{ID, util:floor(Count * Multi + Old)}|ItemList1], Multi)
	end.

%% @doc 结构值系数加成
%% @param:: Record 结构体
%%          Factor 加成系数
mul_record(Record, Factor) ->
    Size = erlang:tuple_size(Record),
    mul_record(Record, Factor, 2, Size).
mul_record(Record, Factor, Size, Size) when is_number(Factor) ->
    NewValue = trunc(element(Size, Record) * Factor),
    setelement(Size, Record, NewValue);
mul_record(Record, Factor, Index, Size) when is_number(Factor) ->
    NewValue = trunc(element(Index, Record) * Factor),
    AttrNew = setelement(Index, Record, NewValue),
    mul_record(AttrNew, Factor, Index + 1, Size).

%% @doc 两个结构值相加
%% @param:: Record 结构体
add_record(Record1, Record2) ->
    Size = erlang:tuple_size(Record1),
    add_record(Record1, Record2, 2, Size).
add_record(Record1, Record2, Size, Size) ->
    NewValue = element(Size, Record1) + element(Size, Record2),
    setelement(Size, Record1, NewValue);
add_record(Record1, Record2, Index, Size) ->
    NewValue = element(Index, Record1) + element(Index, Record2),
    AttrNew = setelement(Index, Record1, NewValue),
    add_record(AttrNew, Record2, Index + 1, Size).

%% @doc 通用结构键值对更新
update_record([], Record) -> Record;
update_record([Args|L], Record) ->
    RecordN = update_record2(Args, Record),
    update_record(L, RecordN).
update_record2({Key, Val}, Record) ->
    setelement(Key, Record, Val).

%% @doc 获取区间配置
get_range_config(Zones, Value) ->
    get_range_config(Zones, Value, undefined).

get_range_config([], _V, Default) -> Default;
get_range_config([{L, H, C} | T], V, Default) ->
    case V >= L andalso (V =< H orelse H =:= -1) of
        true -> C;
        _ -> get_range_config(T, V, Default)
    end.

%% @doc 过滤掉元组列表中某个元素相同的列表
%% eg. L=[{1,2},{2,2},{3,1}]. list_filter(L, 2) -> [{1,2},{3,1}]
list_filter(List, N) ->
    list_filter_helper(List, N, [], []).

list_filter_helper([H | T], N, ResultList, KeyList) ->
    Key = element(N, H),
    case lists:member(Key, KeyList) of
        true -> list_filter_helper(T, N, ResultList, KeyList);
        false -> list_filter_helper(T, N, [H | ResultList], [Key | KeyList])
    end;
list_filter_helper([], _, ResultList, _) -> ResultList.

%% @doc 随机打乱list元素顺序
list_shuffle([]) ->
    [];
list_shuffle(List) ->
    Len = length(List),
    List1 = [{util:rand(1, Len + 10000), X} || X <- List],
    List2 = lists:sort(List1),
    [E || {_, E} <- List2].

shuffle_list_n([], N) when is_integer(N) -> [];
shuffle_list_n(L, 0) when is_list(L) -> [];
shuffle_list_n(L, N) when is_list(L), is_integer(N), N >= 0 ->
    Len = erlang:length(L),
    lists:sublist([X || {_, X} <- lists:sort([{rand:uniform(Len), E} || E <- L])], N).

%% @doc 根据下标替换list元素值
list_replace(Index, NewElem, List) ->
    list_replace_helper(List, Index, NewElem, 1, []).
list_replace_helper([], _Index, _NewElem, _CurIndex, NewList) ->
    NewList;
list_replace_helper([H | T], Index, NewElem, CurIndex, NewList) ->
    if Index =:= CurIndex ->
        list_replace_helper(T, Index, NewElem, CurIndex + 1, NewList ++ [NewElem]);
        true ->
            list_replace_helper(T, Index, NewElem, CurIndex + 1, NewList ++ [H])
    end.

%% @doc 根据list的元素值获得下标
list_get_index(Elem, List) ->
    list_get_index_helper(List, Elem, 0).
list_get_index_helper([], _Elem, _Index) -> 0;
list_get_index_helper([H | T], Elem, Index) ->
    if 
        H =:= Elem -> Index + 1;
        true -> list_get_index_helper(T, Elem, Index + 1)
    end.

%% @doc 根据list的元素值获得下标（加强版）
%% @param:: (Elem, N, List), List为元组列表，N为元组中第N个元素等于Elem
%% @return:: {0,null} | {Index, H}
list_get_index_ex(Elem, N, List) when is_list(List), is_integer(N) ->
    list_get_index_ex(Elem, N, List, 0);
list_get_index_ex(_, _, _) -> {0, null}.
list_get_index_ex(_Elem, _N, [], _) -> {0, null};
list_get_index_ex(Elem, N, [H | _], Index) when element(N, H) =:= Elem -> {Index + 1, H};
list_get_index_ex(Elem, N, [_ | L], Index) -> list_get_index_ex(Elem, N, L, Index + 1).

%% @doc 多个列表数值相加，结果以第一个列表的长度为准
lists_add([ResultList]) -> ResultList;
lists_add([List1, List2 | T]) ->
    ResultList = lists_add_helper(List1, List2, []),
    lists_add([ResultList | T]).

lists_add_helper([], _List2, ResultList) ->
    lists:reverse(ResultList);
lists_add_helper(List1, [], ResultList) ->
    lists:reverse(ResultList) ++ List1;
lists_add_helper([H1 | T1], [H2 | T2], ResultList) ->
    lists_add_helper(T1, T2, [H1 + H2 | ResultList]).

%% @doc 比较函数
cmp([]) ->
    true;
cmp([{Ka, Kb} | T]) ->
    if
        Ka > Kb -> true;
        Ka < Kb -> false;
        true -> cmp(T)
    end.

%% @doc 扩展版lists:min/1
%% @param:: (List, N), List为元组列表，N为元组中第N个元素
min_ex([H | T], N) -> min_ex(T, H, N).
min_ex([H | T], Min, N) when element(N, H) < element(N, Min) -> min_ex(T, H, N);
min_ex([_ | T], Min, N) -> min_ex(T, Min, N);
min_ex([], Min, _) -> Min.

%% @doc 扩展版lists:max/1
%% @param: (List, N), List为元组列表，N为元组中第N个元素
max_ex([H | T], N) -> max_ex(T, H, N);
max_ex([], _N) -> 0.

max_ex([H | T], Max, N) when element(N, H) > element(N, Max) -> max_ex(T, H, N);
max_ex([_ | T], Max, N) -> max_ex(T, Max, N);
max_ex([], Max, _) -> Max.

%% @doc 扩展版lists:max/1
%% @param:: (List, N), List为元组列表，N为元组中第N个元素, Record为列表为空时调用者预期返回的内容
keymax([H | T], N, Record) -> keymax(T, H, N, Record);
keymax([], _N, Record) -> Record.

keymax([H | T], Max, N, Record) when element(N, H) > element(N, Max) -> keymax(T, H, N, Record);
keymax([_ | T], Max, N, Record) -> keymax(T, Max, N, Record);
keymax([], Max, _, _) -> Max.

%% @doc 列表中的元素是否全部相同
%% @param:: (List, N), List为元组列表，N为元组中第N个元素
is_all_same([H | T], N) -> is_all_same(T, H, N).

is_all_same([H | T], Min, N) when element(N, H) =:= element(N, Min) -> is_all_same(T, H, N);
is_all_same(L, _, _) when L =/= [] -> false;
is_all_same([], _, _) -> true.

%% @doc 列表中某元素的总和
sum_ex(L, N) -> sum_ex(L, 0, N).
sum_ex([H | T], Sum, N) -> sum_ex(T, Sum + element(N, H), N);
sum_ex([], Sum, _) -> Sum.

%% 扩展的map函数
map_ex(_Fun, [], _Arg) ->
    [];
map_ex(Fun, [H | T], Arg) ->
    [Fun(H, Arg) | map_ex(Fun, T, Arg)].

%% @doc 截取列表的第Begin个到第End个
sublist(L, Begin, End) ->
    sublist(L, Begin, End, {1, []}).

sublist([], _Begin, _End, {_NowNth, RetL}) ->
    lists:reverse(RetL);
sublist([_ | _L], _Begin, End, {NowNth, RetL}) when NowNth > End ->
    lists:reverse(RetL);
sublist([Item | L], Begin, End, {NowNth, RetL}) when Begin =< NowNth andalso NowNth =< End ->
    sublist(L, Begin, End, {NowNth + 1, [Item | RetL]});
sublist([_ | L], Begin, End, {NowNth, RetL}) ->
    sublist(L, Begin, End, {NowNth + 1, RetL}).

%% @doc 分割列表
split_list(N, List) when length(List) =< N ->
    {List, []};
split_list(N, List) ->
    lists:split(N, List).

%% @doc 随机分割列表
rand_split_list(N, List) when length(List) =< N ->
    {List, []};
rand_split_list(N, List) ->
    rand_split_list(N, List, []).
rand_split_list(0, List, Acc) ->
    {Acc, List};
rand_split_list(N, List, Acc) ->
    I = list_rand(List),
    NList = List -- [I],
    NAcc = [I | Acc],
    rand_split_list(N - 1, NList, NAcc).

%% @doc 多倍列表
list_multiple(N, List) ->
    lists:flatten(lists:duplicate(N, List)).

acc_attr_list(List, Count) ->
	acc_attr_list(List, [], Count).

acc_attr_list([], List, _Count) -> List;
acc_attr_list([{Type, Value}|T], List, Count) ->
	AddValue = util:floor(Value * Count),
	case lists:keyfind(Type, 1, List) of
		false ->
			acc_attr_list(T, [{Type, AddValue}|List], Count);
		{Type, Old} ->
			List1 = lists:keydelete(Type, 1, List),
			acc_attr_list(T, [{Type, AddValue + Old}|List1], Count)
	end.

max_attr_list(List, MaxList) ->
	max_attr_list(List, MaxList, []).

max_attr_list([], _MaxList, Acc) -> lists:reverse(Acc);
max_attr_list([{Type, Value}|T], MaxList, Acc) ->
	case lists:keyfind(Type, 1, MaxList) of
		false ->
			max_attr_list(T, MaxList, Acc);
		{Type, Max} ->
			max_attr_list(T, MaxList, [{Type, min(Value, Max)}|Acc])
	end.

%% @doc 列表循环操作
list_handle(F, Data, List, Place) ->
    case List of
        [H | T] ->
            case F(H, Data, Place) of
                {ok, Data2} ->
                    list_handle(F, Data2, T, Place);
                Error ->
                    Error
            end;
        [] ->
            {ok, Data}
    end.

%% @doc 列表循环函数
list_handle(F, Data, List) ->
    case List of
        [H | T] ->
            case F(H, Data) of
                {ok, Data2} ->
                    list_handle(F, Data2, T);
                Error ->
                    Error
            end;
        [] ->
            {ok, Data}
    end.

%% ----------------------------------------------------------
%% 随机函数相关
%% ----------------------------------------------------------

%% @doc 随机取出list元素
list_rand([]) -> null;
list_rand(List) ->
    Len = length(List),
    Index = rand(1, Len),
    lists:nth(Index, List).

%% @doc 随机从列表中选n个元素
%% @return:: null | List
list_rand_n([], _PickNum) -> [];
list_rand_n(List, PickNum) ->
    list_rand_n(List, PickNum, []).

list_rand_n([], _PickNum, AccList) -> AccList;
list_rand_n(_List, 0, AccList) -> AccList;
list_rand_n(List, PickNum, AccList) ->
    PickOne = list_rand(List),
    LeftList = List -- [PickOne],
    list_rand_n(LeftList, PickNum - 1, [PickOne | AccList]).

%% @doc 查找匹配机率的值
find_ratio([], _, _) -> [];
find_ratio([{N, R} | _], S, Ra) when Ra > S andalso Ra =< (S + R) -> N;
find_ratio([{_, R} | T], S, Ra) -> find_ratio(T, (S + R), Ra).

%% @doc 依据权重，从元组列表中随机挑选N个元素，返回被抽中的元组列表，不放回抽取
%% @Param:: Tuples: 元组列表([{权重, ...}, ...]等类似格式)
%%          Index: 权重所在的位置(如上例则是1)
%%          PickNum: 随机抽出的次数
rand_by_weight(Tuples, Index, PickNum) when PickNum >= 0 ->
    rand_n_by_weight(Tuples, Index, PickNum, []).

rand_n_by_weight(_Tuples, _Index, 0, Ret) -> Ret;
rand_n_by_weight([], _Index, _PickNum, Ret) -> Ret;
rand_n_by_weight(Tuples, Index, PickNum, Ret) ->
    PickOne = rand_by_weight(Tuples, Index),
    LeftTuples = lists:delete(PickOne, Tuples),
    rand_n_by_weight(LeftTuples, Index, PickNum - 1, [PickOne | Ret]).

%% @doc 依据权重，从元组列表中随机挑选一个元素，返回被抽中的元组，
%%      如果没有对应的元素，则抛出异常
%% @param:: Index 权重所在的位置
%% @return:: Tuple
rand_by_weight([], _Index) ->
    error(badargs);
rand_by_weight(List, Index) ->
    Sum = lists:sum([weight_index(Tuple, Index) || Tuple <- List]),
    P = rand(1, Sum),
    rand_one_by_weight(List, Index, P).

rand_one_by_weight([Tuple], _, _) -> Tuple;
rand_one_by_weight([Tuple | T], Index, P) ->
    case weight_index(Tuple, Index) of
        Weight when P =< Weight -> Tuple;
        Weight -> rand_one_by_weight(T, Index, P - Weight)
    end.

weight_index(Tuple, Index) when is_tuple(Tuple) ->
    element(Index, Tuple);
weight_index(Map, Key) when is_map(Map) ->
    maps:get(Key, Map).

%% 从累加的概率中随机一个
rand_list_one([], _Rand) ->  error(badargs);
rand_list_one([{Value, Rate}|T], Rand) ->
	if Rate >= Rand ->
		Value;
	true ->
		rand_list_one(T, Rand)
	end;
rand_list_one([{Value1, Value2, Rate}|T], Rand) ->
	if Rate >= Rand ->
		{Value1, Value2};
	true ->
		rand_list_one(T, Rand)
	end.

%% ----------------------------------------------------------
%% 操作工具相关
%% ----------------------------------------------------------

%% @doc FOR循环
for(Max, Max, F) -> F(Max);
for(I, Max, F) ->
    F(I),
    for(I + 1, Max, F).

%% @doc 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min < Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State) -> {ok, NewState} = F(I, State), for(I + 1, Max, F, NewState).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([type:object_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [type:object_to_list(H) | NList],
    implode(S, T, [S | L]).

%% @doc 分割字符串为列表
explode(S, B) ->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

gen_n(0, []) ->
    [];
gen_n(N, List) when N > 0, is_list(List), List =/= [] ->
    gen_n(N, List, []);
gen_n(_N, _List) ->
    [].

gen_n(0, _List, Acc) ->
    Acc;
gen_n(N, List, Acc) when is_list(Acc) ->
    Item = list_rand(List),
    gen_n(N - 1, List, [Item | Acc]).

%% @doc proplists:get_value/2
prop_get_value(Key, List) ->
    prop_get_value(Key, List, undefined).
prop_get_value(Key, List, Def) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Val} -> Val;
        _ -> Def
    end.
prop_get_value(Key, Pos, List, Def) ->
    case lists:keyfind(Key, Pos, List) of
        {Key, Val} -> Val;
        _ -> Def
    end.

%% @doc proplists:get_keys/1
prop_get_keys(List) ->
    [K || {K, _V} <- List].

%% @doc proplists:delete/2
prop_delete(K, List) ->
    lists:keydelete(K, 1, List).

%% @doc
prop_store({K, V}, List) ->
    prop_store(K, V, List).

prop_store(K, V, List) ->
    lists:keystore(K, 1, List, {K, V}).

%% @doc
prop_increase(K, List) ->
    prop_increase(K, List, 1).

prop_increase(K, List, Incl) ->
    case prop_get_value(K, List, 0) of
        V when is_integer(V) ->
            prop_store(K, V + Incl, List);
        _ ->
            List
    end.

%% @doc 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    %% 以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            rand:seed(exs1024),
            put("rand_seed", 1);
        _ ->
            skip
    end,
    M = Min - 1,
    rand:uniform(Max - M) + M.

%% @doc 向上取整
ceil(X) ->
    T = trunc(X),
    case X =:= T of
        true -> T;
        false -> T + 1
    end.

%% @doc 向下取整
floor(X) ->
    T = trunc(X),
    case X < T of
        true -> max(T - 1, 0);
        _ -> T
    end.

%% @doc 获取客户端ip
get_ip(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, _Port}} -> Ip;
        {error, _Reason} -> {0, 0, 0, 0}
    end.

%% doc 角度和cos的转换，cos(60') = 0.5
angle_to_float(Angle) ->
    math:cos(math:pi() * Angle / 180).

%% @doc 分页取数据
%% @param:: Data: 所有数据（列表）
%%          Page: 第几页数据（大于总页数则默认最后页）
%%          PageNum: 每一页数量
%% @return:: {总条数, 当前页, 当前页数据}
page_data(List, Page, Count) ->
	Len = length(List),
	Start = max(Page - 1, 0) * Count + 1,
	if Start > Len -> 
		   {Len, Page, []};  
	   true ->
		   {Len, Page, lists:sublist(List, Start, Count)}			   
	end.

%% ----------------------------------------------------------
%% 字符文本操作
%% ----------------------------------------------------------

%% @doc 检查TICKET
check_char_encrypt(Id, Time, TK) ->
    TICKET = config:get_ticket(),
    Hex = util:md5(lists:concat([Time, Id, TICKET])),
    Hex =:= TK.

%% @doc 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(S))]).

%% @doc 转换成HEX格式的hmac
hmac(Key, Msg) ->
    crypto:hmac(sha, list_to_binary(Key), list_to_binary(Msg)).

%% @doc 转换成HEX格式的hmac
hmac_hex(Key, Msg) ->
    <<Mac:160/integer>> = crypto:hmac(sha, list_to_binary(Key), list_to_binary(Msg)),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).

%% @doc 检查客户端发过来的内容,false为不合法,true为合法
%% @param:: String: 客户端发来的字符串
%%          Length: 服务端限制的字符串长度
check_string(String, Length) ->
    case check_length(String, Length) of
        true ->
            case check_keyword(String, ["'", "/", "\"", "_", "<", ">"]) of
                false ->
                    case check_keyword(String) of
                        false -> true;
                        true -> false
                    end;
                true ->
                    false
            end;
        false ->
            false
    end.

%% @doc 检查关键字，存在非法字符返回true，否则false
%% @param:: Text : 需要检查的字符串（或字符串的二进制形式）
%%          Words: 非法字符列表
check_keyword(_, []) ->
    false;
check_keyword(Text, [Word | Words]) ->
    case re:run(Text, Word, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            check_keyword(Text, Words)
    end.

check_keyword(Text) ->
    if
        is_list(Text) ->
            lib_filter:word_is_sensitive_name(Text);
        true ->
            true
    end.

filter_text_gm(Text) when is_bitstring(Text) ->
    Text;
filter_text_gm(Text) when is_list(Text) ->
    list_to_bitstring(Text).

%% @doc 敏感词过滤
%% @param:: Text list() | bitstring()
%% @return:: bitstring() 过滤后的文本
filter_text(Text, Lv) when is_bitstring(Text) ->
    S = bitstring_to_list(Text),
    filter_text(S, Lv);
filter_text(Text, Lv) when is_list(Text) ->
    [Term] = io_lib:format("~ts", [Text]),
    lib_filter:replace_sensitive_talk(Term, Lv).

%% @doc 名字过滤
%% @param:: Text list() | bitstring()
%% @return:: bitstring() 过滤后的文本
filter_name(Text) when is_bitstring(Text) ->
    S = bitstring_to_list(Text),
    filter_name(S);
filter_name(Text) when is_list(Text) ->
    [Term] = io_lib:format("~ts", [Text]),
    lib_filter:replace_sensitive_name(Term).

%% @doc 敏感词检测
%% @param:: Text: 字符串 ==> 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
%% @return:: true 存在关键词
%%           false 不存在关键词
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.

%% @doc 长度合法性检查
check_length(Item, LenLimit) ->
    check_length(Item, 1, LenLimit).

check_length(Item, MinLen, MaxLen) ->
    case unicode:characters_to_list(list_to_binary(Item)) of
        UnicodeList when is_list(UnicodeList) ->
            Len = string_width(UnicodeList),
            Len =< MaxLen andalso Len >= MinLen;
        _ ->
            false
    end.

%% @doc IP元组转字符
ip2bin(IP) when is_list(IP) ->
    case inet:parse_address(IP) of
        {ok, _IP} -> ip2bin(_IP);
        _ -> "256.256.256.256"
    end;
ip2bin({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D)];
ip2bin(_) ->
    "256.256.256.256".

%% @doc 过滤掉字符串中的特殊字符
filter_string(String, CharList) ->
    case is_list(String) of
        true ->
            filter_string_helper(String, CharList, []);
        false when is_binary(String) ->
            ResultString = filter_string_helper(binary_to_list(String), CharList, []),
            list_to_binary(ResultString);
        false ->
            String
    end.

filter_string_helper([], _CharList, ResultString) ->
    ResultString;
filter_string_helper([H | T], CharList, ResultString) ->
    case lists:member(H, CharList) of
        true -> filter_string_helper(T, CharList, ResultString);
        false -> filter_string_helper(T, CharList, ResultString ++ [H])
    end.

%% @doc 跳过编码为4个字节的字符
escape_unicode([H | T]) ->
    if
        H band 16#F8 =:= 16#F0 ->
            if
                length(T) >= 3 ->
                    escape_unicode(lists:nthtail(3, T));
                true -> []
            end;
        true ->
            [H | escape_unicode(T)]
    end;
escape_unicode([]) ->
    [].

%% @doc 转义数据库varchar
escape_varchar(S) ->
    S1 = escape_unicode(S),
    R = re:replace(S1, "[\\\\|-|;|,|.|/|\\(|\\)|\\[|\\]|}|{|%|\\@|*|!|'|\"]", "", [global, {return, binary}]),
    erlang:binary_to_list(R).

%% @doc 转义数据库varchar
escape_name(S) ->
    escape_unicode(string:strip(S)).

%% @doc 用于聊天检测
calc_string_compare(A, B) ->
    AWordDict = word_dict(unicode:characters_to_list(type:object_to_binary(A))),
    BWordDict = word_dict(unicode:characters_to_list(type:object_to_binary(B))),
    Dict = merge_dict(AWordDict, BWordDict),
    F = fun(_K, {V1, V2}, {DenominatorAcc, Sqdoc1Acc, Sqdoc2Acc}) ->
        {DenominatorAcc + V1 * V2
            , Sqdoc1Acc + V1 * V1
            , Sqdoc2Acc + V2 * V2
        }
    end,
    {Denominator, Sqdoc1, Sqdoc2} = dict:fold(F, {0, 0, 0}, Dict),
    case Sqdoc1 =:= 0 orelse Sqdoc2 =:= 0 of
        true -> 0;
        false -> Denominator / math:sqrt(Sqdoc1 * Sqdoc2)
    end.

merge_dict(D1, D2) ->
    F1 = fun(_K, V) -> {V, 0} end,
    D1T = dict:map(F1, D1),
    F2 = fun(K, V, Dict) ->
        case dict:find(K, D1T) of
            error -> dict:store(K, {0, V}, Dict);
            {ok, {V1, 0}} -> dict:store(K, {V1, V}, Dict);
            _ -> Dict
        end
    end,
    D2T = dict:fold(F2, D1T, D2),
    D2T.

%% @doc 取字（连续数字、连续字符当作一个字）
%%      A-Z   65-90
%%      a-z   97-122
%%      0-9   48-57
word_dict(L) -> word__(L, [], dict:new()).

word__([A | L], Word, WordDict) when (A >= 65 andalso A =< 90) orelse
    (A >= 97 andalso A =< 122) orelse
    (A >= 48 andalso A =< 57) ->
    word__(L, [A | Word], WordDict);
word__([I | L], [], WordDict) ->
    word__(L, [], dict:update_counter([I], 1, WordDict));
word__([I | L], Word, WordDict) ->
    WordDict1 = dict:update_counter(Word, 1, WordDict),
    WordDict2 = dict:update_counter([I], 1, WordDict1),
    word__(L, [], WordDict2);
word__([], [], WordList) ->
    WordList;
word__([], Word, WordDict) ->
    dict:update_counter(Word, 1, WordDict).

%% @doc 截取字符串
string_sub(Str, Start) ->
    Unicode = unicode:characters_to_list(Str),
    unicode:characters_to_binary(lists:sublist(Unicode, Start)).
string_sub(Str, Start, Len) ->
    Unicode = unicode:characters_to_list(Str),
    unicode:characters_to_binary(lists:sublist(Unicode, Start, Len)).

%% @doc 安全检查格式化字符串
safe_format(Fmt, Args) when is_list(Args) ->
    F = fun(A) -> ?iif(is_list(A) =:= true, list_to_binary(A), A) end,
    ArgsN = lists:map(F, Args),
    case catch lists:flatten(io_lib:format(Fmt, ArgsN)) of
        Val when is_list(Val) orelse is_binary(Val) ->
            Val;
        _ ->
            Fmt
    end.

%% @doc 除去名字前面的平台号
trip_name(Name) when is_binary(Name) ->
    trip_name(erlang:binary_to_list(Name));
trip_name(Name) when is_list(Name) ->
    string:sub_string(Name, string:chr(Name, $.) + 1).

%% -----------------------------------------------------------------------------
%% dict相关
%% -----------------------------------------------------------------------------

dict_find(Key, Dict) ->
    dict_find(Key, Dict, undefined).

dict_find(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, V} -> V;
        _ -> Default
    end.

dict_find2(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, V} -> {true, V};
        _ -> {false, Default}
    end.
    
%% ----------------------------------------------------------
%% 定时器相关
%% ----------------------------------------------------------

%% 取消定时器
cancel_timer(Timer) ->
    case is_reference(Timer) of
        true ->
            erlang:cancel_timer(Timer);
        false ->
            skip
    end,
    [].

cancel_timer(Ref, Event) when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        fasle ->
            receive Event -> 0
            after 0 -> false
            end;
        RemainingTime -> RemainingTime
    end;
cancel_timer(_Ref, _Event) ->
    false.

%% ----------------------------------------------------------
%% ETS相关
%% ----------------------------------------------------------

%% @doc 获取键值
get_ets(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{Key, Val}] -> Val;
        _ -> undefined
    end.
get_ets(Table, Key, Def) ->
    case ets:lookup(Table, Key) of
        [{Key, Val}] -> Val;
        _ -> Def
    end.

%% @doc 存储键值
put_ets(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).

%% @doc 删除键值
del_ets(Table, Key) ->
    ets:delete(Table, Key).

%% @doc 清空表
cls_ets(Table) ->
    ets:delete_all_objects(Table).

%% ----------------------------------------------------------
%% 服务器时间相关（纯时间操作见time）
%% ----------------------------------------------------------

%% @doc 获取开服时间
get_open_time() ->
    case ets:lookup(?ETS_SERVER_STATE, open_time) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> time:unixdate()
    end.

%% @doc 获取开服天数（开服当天为第1天）
get_open_days() ->
    OpenTime = get_open_time(),
    Now = time:unixtime(),
    case Now >= OpenTime of
        true -> time:get_diff_days(OpenTime, Now) + 1;
        _ -> 1
    end.

%% @doc 获取开服天数（开服当天为第1天）
get_open_days(Now) ->
    OpenTime = get_open_time(),
    get_open_days(Now, OpenTime).

%% @doc 获取开服天数（开服当天为第1天）
get_open_days(Now, OpenTime) ->
    case Now >= OpenTime of
        true -> time:get_diff_days(OpenTime, Now) + 1;
        _ -> 1
    end.
 
%% @doc 获取开服周数
get_open_weeks(Now) ->
    OpenDays = get_open_days(Now),
    OpenDays div 7 + 1.

%% @doc 获取开服周数
get_open_weeks(Now, OpenTime) ->
    OpenDays = get_open_days(Now, OpenTime),
    OpenDays div 7 + 1.

%% @doc 获取单双周
get_op_week_type(Now) ->
    OpWeeks = get_open_weeks(Now),
    ?iif(OpWeeks rem 2 =:= 0, 2, 1).

%% @doc 获取单双周
get_op_week_type(Now, OpenTime) ->
    OpWeeks = get_open_weeks(Now, OpenTime),
    ?iif(OpWeeks rem 2 =:= 0, 2, 1).

%% @doc 获取开服时间
get_open_date() ->
    OpenTime = get_open_time(),
    {Date, _} = time:unixtime_to_localtime(OpenTime),
    Date.

%% @doc 开服第N天（开服当天为第1天）
get_date_since_open(Day) when Day >= 1 ->
    OpenDate = get_open_date(),
    time:add_days(OpenDate, Day - 1).

%% @doc 是否合过服
is_merge() ->
    get_merge_time() > 0.

%% @doc 获取合服时间，等于0表示非合服状态
get_merge_time() ->
    case ets:lookup(?ETS_SERVER_STATE, merge_time) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> 0
    end.

%% @doc 获取合服次数
get_merge_count() ->
    case ets:lookup(?ETS_SERVER_STATE, merge_count) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> 0
    end.

%% @doc 获取上一次关机时间戳
get_shutdown_tick() ->
    case ets:lookup(?ETS_SERVER_STATE, shut_down_tick) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> 0
    end.	

%% @doc 获取合服时间
get_merge_date() ->
    MergeTime = get_merge_time(),
    {Date, _} = time:unixtime_to_localtime(MergeTime),
    Date.

%% @doc 获取合服天数
get_merge_days() ->
    MergeTime = get_merge_time(),
    Now = time:unixtime(),
    case MergeTime > 0 of
        true -> time:get_diff_days(MergeTime, Now) + 1;
        _ -> 0
    end.

%% @doc 获取合服天数
get_merge_days(Now) ->
    MergeTime = get_merge_time(),
    case MergeTime > 0 of
        true -> time:get_diff_days(MergeTime, Now) + 1;
        _ -> 0
    end.

%% @doc 获取合服天数（合服当天为第1天）
get_merge_days(Now, MergeTime) ->
    case MergeTime > 0 of
        true -> time:get_diff_days(MergeTime, Now) + 1;
        _ -> 0
    end.

%% @doc 合服第N天（合服当天为第1天）
get_date_since_merge(Day) when Day >= 1 ->
    MergeDate = get_merge_date(),
    time:add_days(MergeDate, Day - 1).

%% @doc 合过服使用合服天数，否则使用开服天数
get_days() ->
    case get_merge_days() of
        0 -> get_open_days();
        MergeDays -> MergeDays
    end.

%% @doc 更改开服时间()
c_open_time(_TimeStart) ->
    TimeStart = time:unixdate(_TimeStart),
    ets:update_element(?ETS_SERVER_STATE, open_time, {#server_state.value, TimeStart}).

%% @doc 更改开服时间 [GM，测试Only]
c_open_time2(_TimeStart) ->
    TimeStart = time:unixdate(_TimeStart),
    case ?DB:transaction(?POOL_GAME, fun() ->
        SQL1 = io_lib:format("replace into `game_info`(`cf_name`, `cf_value`) values('version', from_unixtime(~w, '%Y-%m-%d-%H-%i-%S'))", [TimeStart]),
        ?DB:execute(?POOL_GAME, SQL1),
        SQL2 = io_lib:format("UPDATE role_login a, (SELECT role_id FROM `role_login` WHERE 1 ORDER BY `role_id` LIMIT 30, 1) b
            SET reg_time = ~w WHERE a.role_id = b.role_id", [TimeStart]),
        ?DB:execute(?POOL_GAME, SQL2),
        SQL3 = io_lib:format("update `kf_game_server` set `open_time` = ~p where `platform` = '~s' and `server_num` = ~p", [TimeStart, config:get_platform(), config:get_server_num()]),
        ?DB:execute(?POOL_GAME, SQL3),
        ok
    end) of
        ok ->
            c_open_time(_TimeStart);
        Err ->
            ?ERROR_MSG("op time failed ~w", [Err]),
            ignore
    end.

%% @doc 更改合服时间
c_merge_time(TimeMerge) ->
    c_merge_time(TimeMerge, 1, false).

c_merge_time(_TimeMerge, Count, Db) ->
    TimeMerge = time:unixdate(_TimeMerge),
    Db andalso begin
        SQL = io_lib:format(<<"replace into `merge_count`(`id`, `time`, `merge_count`) values(~w, ~w, ~w)">>, [1, TimeMerge, Count]),
        ?DB:execute(?POOL_GAME, SQL)
    end,
    ets:update_element(?ETS_SERVER_STATE, merge_time, {#server_state.value, TimeMerge}).

%% @doc 服务器启动时间
get_start_time() ->
    case ets:lookup(?ETS_SERVER_STATE, start_time) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> 0
    end.

%% ----------------------------------------------------------
%% 服务器类型
%% ----------------------------------------------------------

%% @doc 获取服务器实际序号
get_sid() ->
    case ets:lookup(?ETS_SERVER_STATE, sid) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> 0
    end.

%% @doc 获取服务器类型
get_server_type() ->
    case ets:lookup(?ETS_SERVER_STATE, server_type) of
        [#server_state{value = V}] when is_integer(V) -> V;
        _ -> ?SERVER_TYPE_LOCAL
    end.

%% @doc 是否跨服节点 [100,200,300]
is_kf_server() ->
    get_server_type() =:= ?SERVER_TYPE_KF.

%% @doc 是否跨服分组节点 [200]
is_kf_group_server() ->
    get_node_id() =:= ?NODE_ID_KFGROUP.

%% @doc 是否跨服通信节点 [0]
is_kf_client_server() ->
    get_node_id() =:= ?NODE_ID_KFCLIENT.

%% @doc 是否逻辑节点 [10]
is_logic_server() ->
    NodeID = get_node_id(),
    NodeID =:= ?NODE_ID_GAME andalso NodeID < ?NODE_ID_KFCENTER.

%% @doc 当前节点ID
get_node_id() ->
    case get_server_type() of
        ?SERVER_TYPE_KF ->
            svr_node_kf:get_node_id();
        _ ->
            ?NODE_ID_GAME
    end.

%% ----------------------------------------------------------
%% 数据库辅助
%% ----------------------------------------------------------

%% @doc 批量插入 [可控制]
batch_insert(PoolFlag, 0, Fmt1, Fmt2, List) ->
    insert_values(PoolFlag, Fmt1, Fmt2, List);
batch_insert(_PoolFlag, _Size, _Fmt1, _Fmt2, []) ->
    ok;
batch_insert(PoolFlag, Size, Fmt1, Fmt2, List) ->
    {Batch, Left} = split_list(Size, List),
    insert_values(PoolFlag, Fmt1, Fmt2, Batch),
    batch_insert(PoolFlag, Size, Fmt1, Fmt2, Left).

batch_insert(PoolFlag, 0, Fmt1, Fmt2, Fun, List) ->
    insert_values(PoolFlag, Fmt1, Fmt2, Fun, List);
batch_insert(_PoolFlag, _Size, _Fmt1, _Fmt2, _Fun, []) ->
    ok;
batch_insert(PoolFlag, Size, Fmt1, Fmt2, Fun, List)
    when is_integer(Size), Size > 0, ?VALID_STR(Fmt1), ?VALID_STR(Fmt2), is_function(Fun, 1), is_list(List) ->
    {Batch, Left} = split_list(Size, List),
    insert_values(PoolFlag, Fmt1, Fmt2, Fun, Batch),
    batch_insert(PoolFlag, Size, Fmt1, Fmt2, Fun, Left).

batch_insert(PoolFlag, 0, Fmt1, Fmt2, Args, Fun, List) -> %% 一次性插入
    insert_values(PoolFlag, Fmt1, Fmt2, Args, Fun, List);
batch_insert(_PoolFlag, _Size, _Fmt1, _Fmt2, _Args, _Fun, []) ->
    ok;
batch_insert(PoolFlag, Size, Fmt1, Fmt2, Args, Fun, List)
    when is_integer(Size), Size > 0, ?VALID_STR(Fmt1), ?VALID_STR(Fmt2), is_list(Args), is_function(Fun, 1), is_list(List) ->
    {Batch, Left} = split_list(Size, List),
    insert_values(PoolFlag, Fmt1, Fmt2, Args, Fun, Batch),
    batch_insert(PoolFlag, Size, Fmt1, Fmt2, Args, Fun, Left).

%% @doc 批量插入 [一次性]
insert_values(_PoolFlag, _Fmt1, _Fmt2, []) ->
    ok;
insert_values(PoolFlag, Fmt1, Fmt2, List) ->
    insert_values2(PoolFlag, Fmt1, Fmt2, [], List).

insert_values(PoolFlag, Fmt1, Fmt2, Fun, List) when is_function(Fun, 1), is_list(List) ->
    insert_values(PoolFlag, Fmt1, Fmt2, [], Fun, List);
insert_values(PoolFlag, Fmt1, Fmt2, Args, List) when is_list(Args) ->
    insert_values2(PoolFlag, Fmt1, Fmt2, Args, List).

insert_values(PoolFlag, Fmt1, Fmt2, Args, Fun, List) when is_list(Args), is_function(Fun, 1), is_list(List) ->
    RankList = lists:map(Fun, List),
    insert_values2(PoolFlag, Fmt1, Fmt2, Args, RankList).

insert_values2(_PoolFlag, _Fmt1, _Fmt2, _Args, []) ->
    ok;
insert_values2(PoolFlag, Fmt1, Fmt2, Args, List) ->
    Values = format_values(Fmt2, List),
    SQL = io_lib:format(Fmt1, Args ++ [Values]),
    ?DB:execute(PoolFlag, SQL),
    ok.

%% @doc
format_values(Temp, List) ->
    FinList = format_values(List, Temp, []),
    implode(",", FinList, []).

format_values([], _Temp, Acc) ->
    lists:reverse(Acc);
format_values([L | T], Temp, Acc) ->
    F = io_lib:format(Temp, L),
    format_values(T, Temp, [F | Acc]).

%% -----------------------------------------------------------------------------
%% supervisor
%% -----------------------------------------------------------------------------

sup_info_child(Sup, Info) ->
    [Pid ! Info || Pid <- sup_children_pid(Sup)].

sup_children_pid(Sup) ->
    case catch supervisor:which_children(Sup) of
        List when is_list(List) ->
            [Child || {_Id, Child, _Type, _Modules} <- List];
        _ ->
            []
    end.
    
%% ----------------------------------------------------------
%% 系统分析工具
%% ----------------------------------------------------------

process_infos() ->
    filelib:ensure_dir("../logs/"),
    File = "../logs/processes_infos.log",
    {ok, Fd} = file:open(File, [write, raw, binary, append]),
    Fun = fun(Pi) ->
        Info = io_lib:format("=>~p \n\n", [Pi]),
        case filelib:is_file(File) of
            true ->
                file:write(Fd, Info);
            false ->
                file:close(Fd),
                {ok, NewFd} = file:open(File, [write, raw, binary, append]),
                file:write(NewFd, Info)
        end,
        timer:sleep(20)
    end,
    [Fun(erlang:process_info(P)) || P <- erlang:processes()].

process_infos(Pid, Type) ->
    filelib:ensure_dir("../logs/"),
    File = "../logs/processes_infos.log",
    {ok, Fd} = file:open(File, [write, raw, binary, append]),
    Fun = fun(Pi) ->
        NewPi = case Type of
            messages ->
                {messages, SendList} = Pi,
                lists:foldl(fun({TType, Content}, CmdList) ->
                    case TType of
                        send ->
                            <<_A:16, Cmd:32, _Left/binary>> = Content,
                            case lists:keyfind(Cmd, 1, CmdList) of
                                false -> [{Cmd, 1} | CmdList];
                                {_, CNum} -> lists:keyreplace(Cmd, 1, CmdList, {Cmd, CNum + 1})
                            end;
                        _ -> [{TType, Content} | CmdList]
                    end
                end, [], SendList);
            _ ->
                Pi
        end,
        Info = io_lib:format("=>~p \n\n", [NewPi]),
        case filelib:is_file(File) of
            true ->
                file:write(Fd, Info);
            false ->
                file:close(Fd),
                {ok, NewFd} = file:open(File, [write, raw, binary, append]),
                file:write(NewFd, Info)
        end
    end,
    PInfo = erlang:process_info(Pid, Type),
    Fun(PInfo).

%% @spec fbin(Bin, Args) -> binary()
%% Bin = binary()
%% Args = list()
%% @doc 返回格式化的二进制字符串
%% <ul>
%% <li>Bin: 待格式化的二进制字符串</li>
%% <li>Args: 格式化参数</li>
%% </ul>
fbin(Bin, Args) ->
    list_to_binary(io_lib:format(Bin, Args)).

fbin(Bin) ->
    fbin(Bin, []).