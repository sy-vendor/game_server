%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十二月 2018 10:45
%%%-------------------------------------------------------------------
-module(util_type).
-author("suyang").

%% API
-export([to_list/1,
    to_binary/1,
    term_to_string/1,
    term_to_bitstring/1,
    string_to_term/1,
    bitstring_to_term/1,
    to_atom/1,
    list_to_atom/1,
    unicode_string/1,
    to_int/1,
    convert_db_field/2,
    convert_db_field/3,
    list_to_string/4,
    escape_varchar/1]).


%% @spec to_list(X) -> list()
%% X = any()
%% @doc 将任意类型的数据转成list()类型(主要用于控制台打印).
%% 注意:tuple类型有特殊处理
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> float_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_pid(X) -> pid_to_list(X);
to_list(X) when is_function(X) -> erlang:fun_to_list(X);
to_list(X) when is_port(X) -> erlang:port_to_list(X);
to_list(X) when is_tuple(X) -> tuple_to_list(X);
to_list(X) when is_list(X) -> X.

%% @spec to_binary(Val) -> binary()
%% Val = any()
%% @doc 将Val值转换为binary格式（8位二进制）
to_binary(Msg) when is_binary(Msg) -> Msg;
to_binary(Msg) when is_atom(Msg) -> list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) -> unicode:characters_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) -> list_to_binary(f2s(Msg));
to_binary(Msg) when is_tuple(Msg) -> list_to_binary(tuple_to_list(Msg));
to_binary(_Val) -> <<>>.

%% @spec term_to_string(Term::term()) -> list()
%% @doc term序列化，term转换为string格式
term_to_string(Term) -> io_lib:format("~w", [Term]).

%% @spec term_to_bitstring(Term::term()) -> bitstring()
%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) -> list_to_bitstring(term_to_string(Term)).

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

%% @spec bitstring_to_term(String::list()) -> [] | term()
%% @doc term反序列化，bitstring转换为term
bitstring_to_term(undefined) -> [];
bitstring_to_term(BitString) -> string_to_term(binary_to_list(BitString)).


%% @doc 转换其他类型为atom
to_atom(X) when is_binary(X) -> util_type:list_to_atom(binary_to_list(X));
to_atom(X) when is_list(X)   -> util_type:list_to_atom(X);
to_atom(X) when is_atom(X)   -> X.

list_to_atom(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
        {'EXIT', _} -> erlang:list_to_atom(List);
        Atom when is_atom(Atom) -> Atom
    end.

%% @doc Unicode字符转换为单字节字符
unicode_string(String) when is_list(String) ->
    unicode_string(unicode:characters_to_binary(String));
unicode_string(Bin) when is_binary(Bin) ->
    erlang:binary_to_list(Bin).

%% @doc 转整数
to_int(Val) when is_integer(Val) -> Val;
to_int(Val) when is_binary(Val) -> to_int(binary_to_list(Val));
to_int(Val) when is_list(Val) -> to_int(list_to_integer(Val));
to_int(Val) when is_float(Val) -> to_int(float_to_list(Val));
to_int(Val) -> Val.

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

%% 数组转成字符串
list_to_string([], _H, _M, _T) -> [];
list_to_string([HList | TList], H, M, T) -> list_to_string(TList, H, M, T, H ++ util_type:to_list(HList)).
list_to_string([], _H, _M, T, Str) -> Str ++ T;
list_to_string([HList | TList], H, M, T, Str) -> list_to_string(TList, H, M, T, Str ++ M ++ util_type:to_list(HList)).

%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
    A.

escape_varchar(S) ->
    S1 = escape_unicode(S),
    R = re:replace(S1, "[\\\\|-|;|,|.|/|\\(|\\)|\\[|\\]|}|{|%|\\@|*|!|'|\"]", "", [global, {return, binary}]),
    erlang:binary_to_list(R).

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