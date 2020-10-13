%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 类型转换工具
%%% @end
%%% Created : 29. 9月 2019 21:12
%%%-------------------------------------------------------------------

-module(type).

-export([
    object_to_list/1,
    object_to_binary/1,
    object_to_atom/1,
    list_to_atom/1,
    boolean_to_integer/1,
    integer_to_boolean/1,
    record_to_list/1,
    list_to_record/2,
    term_to_bitstring/1,
    bitstring_to_term/1,
    term_to_string/1,
    term_to_string2/1,
    string_to_term/1,
    string_to_term2/1,
    string_to_integer/1,
    mysql_query_quote/1,
    unicode_string/1,
    unicode_binary/1,
    convert_db_field/2,
    convert_db_field/3,
    bitstring_to_pid/1,
    pid_to_bitstring/1
]).

%% @doc 转换其他类型为list
object_to_list(X) when is_integer(X) -> integer_to_list(X);
object_to_list(X) when is_float(X)   -> float_to_list(X);
object_to_list(X) when is_atom(X)    -> atom_to_list(X);
object_to_list(X) when is_binary(X)  -> binary_to_list(X);
object_to_list(X) when is_tuple(X)   -> tuple_to_list(X);
object_to_list(X) when is_list(X)    -> X.

%% @doc 转换其他类型为binary
object_to_binary(X) when is_binary(X) -> X;
object_to_binary(X) when is_list(X)   -> list_to_binary(X);
object_to_binary(X) when is_atom(X)   -> list_to_binary(atom_to_list(X));
object_to_binary(X) when is_binary(X) -> X.

%% @doc 转换其他类型为atom
object_to_atom(X) when is_binary(X) -> type:list_to_atom(binary_to_list(X));
object_to_atom(X) when is_list(X)   -> type:list_to_atom(X);
object_to_atom(X) when is_atom(X)   -> X.

list_to_atom(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
            {'EXIT', _} -> erlang:list_to_atom(List);
            Atom when is_atom(Atom) -> Atom
    end.

%% @doc 转换boolean类型为integer
boolean_to_integer(Boolean) when is_integer(Boolean) -> Boolean > 0;
boolean_to_integer(Boolean) when is_boolean(Boolean) ->
    case Boolean of
        true  -> 1;
        false -> 0
    end.

%% @doc 转换integer类型为boolean
integer_to_boolean(Integer) when is_integer(Integer) ->
    case Integer of
        0 -> false;
        _ -> true
    end.

%% @doc 转换record类型为list
%% 注意：去掉了第一个表示记录类型的字段。
record_to_list(Record) ->
    [_|ListLeft] = erlang:tuple_to_list(Record),
    ListLeft.

%% @doc 转换list类型为record
%% 注意：list长度必须跟record一样
list_to_record(List, RecordAtom) ->
    erlang:list_to_tuple([RecordAtom|List]).

%% @doc term序列化，term转换为bitstring格式
%% eg: [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% @doc term反序列化，bitstring转换为term
%% eg: <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> [];
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

%% term序列化，term转换为string格式
%% eg: [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% @doc term保留字符串显示
term_to_string2(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% @doc term反序列化，string转换为term
%% eg: "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% @doc 字符串序列化
string_to_term2(BinString) ->
    case bitstring_to_term(BinString) of
        undefined -> [];
        Term -> Term
    end.

%% @doc 字符串转整型
string_to_integer(String) ->
    case string:to_integer(String) of
        {error, _} -> 0;
        {I, _Left} -> I
    end.
    
%% @doc Quote a string or binary value so that it can be included safely in a
%% MySQL query.
mysql_query_quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote__(String, [])])];    %% 39 is $'
mysql_query_quote(Bin) when is_binary(Bin) ->
    mysql_query_quote(binary_to_list(Bin)).

quote__([], Acc) ->
    Acc;
quote__([0 | Rest], Acc) ->
    quote__(Rest, [$0, $\\ | Acc]);
quote__([10 | Rest], Acc) ->
    quote__(Rest, [$n, $\\ | Acc]);
quote__([13 | Rest], Acc) ->
    quote__(Rest, [$r, $\\ | Acc]);
quote__([$\\ | Rest], Acc) ->
    quote__(Rest, [$\\ , $\\ | Acc]);
quote__([39 | Rest], Acc) ->        %% 39 is $'
    quote__(Rest, [39, $\\ | Acc]);    %% 39 is $'
quote__([34 | Rest], Acc) ->        %% 34 is $"
    quote__(Rest, [34, $\\ | Acc]);    %% 34 is $"
quote__([26 | Rest], Acc) ->
    quote__(Rest, [$Z, $\\ | Acc]);
quote__([C | Rest], Acc) ->
    quote__(Rest, [C | Acc]).

%% @doc Unicode字符转换为单字节字符
unicode_string(String) when is_list(String) ->
    unicode_string(unicode:characters_to_binary(String));
unicode_string(Bin) when is_binary(Bin) ->
    erlang:binary_to_list(Bin).

%% @doc Unicode字符转换为二进制
unicode_binary(String) when is_list(String) ->
    unicode:characters_to_binary(String);
unicode_binary(Bin) when is_binary(Bin) ->
    Bin.

%% @doc 转换数据库字段
convert_db_field(Type, String) ->
    convert_db_field(Type, String, undefined).

convert_db_field(Type, String, Default) ->
    case {Type, bitstring_to_term(String)} of
        {list, List} when is_list(List) ->
            List;
        {map, Map} when is_map(Map) ->
            Map;
        {term, Term} when Term =/= undefined ->
            Term;
        _ ->
            Default
    end.

%% @doc 进程标识转换为字符串
pid_to_bitstring(Pid) when is_pid(Pid) ->
    term_to_bitstring(erlang:pid_to_list(Pid));
pid_to_bitstring(_Pid) ->
    term_to_bitstring(0).

%% @doc 二进制转换为进程标识
bitstring_to_pid(Bin) ->
    case catch (erlang:list_to_pid(bitstring_to_term(Bin))) of
        Ret when is_pid(Ret) ->
            Ret;
        _ ->
            0
    end.
