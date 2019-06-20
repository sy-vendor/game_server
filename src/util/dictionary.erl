%%-------------------------------------------------------
%% @File     : dictionary.erl
%% @Brief    : 进程字典操作函数
%% @Author   : shebiao <63880782@qq.com>
%% @Date     : 2015-1-6
%%-------------------------------------------------------

-module(dictionary).
-export([
    start/2,
    stop/1,
    clear/1,
    update/2,
    get/1,
    get/2,
    get/3,
    erase/2,
    erase/1,
    find/2,
    get_dic/1
]).

%%#################进程字典存放类型为tuple的数据项####################

start(DictType, KeyPos) ->
    set_dict_key_pos(DictType, KeyPos),
    erlang:put(DictType, []),
    ok.

stop(DictType) ->
    erase_dict_key_pos(DictType),
    erlang:erase(DictType),
    ok.

clear(DictType) ->
    erlang:erase(DictType).

update(DictType, Value) ->
    Val = erlang:get(DictType),
    KeyPos = get_dict_key_pos(DictType),
    L = erlang:tuple_to_list(Value),
    Key = lists:nth(KeyPos, L),
    Val1 = lists:keystore(Key, KeyPos, Val, Value),
    erlang:put(DictType, Val1).
    
get(DictType) ->
    erlang:get(DictType).

get(DictType, Filter) when is_function(Filter) ->
    Val = erlang:get(DictType),
    lists:filter(Filter, Val);
    
get(DictType, Key) ->
    Val = erlang:get(DictType),
    KeyPos = get_dict_key_pos(DictType),
    case lists:keysearch(Key, KeyPos, Val) of
        {value, T} -> T;
        false -> []
    end.

get(DictType, Filter, MapF) ->
    Val = erlang:get(DictType),
    Val1 = lists:filter(Filter, Val),
    lists:map(Val1, MapF).

find(DictType, Key) ->
    Val = erlang:get(DictType),
    KeyPos = get_dict_key_pos(DictType),
    case lists:keysearch(Key, KeyPos, Val) of
        {value, _} -> true;
        false -> false
    end.

erase(DictType, Filter) when is_function(Filter) ->
    Val = erlang:get(DictType),
    Val1 = lists:filter(Filter, Val),
    erlang:put(DictType, Val--Val1);
    
erase(DictType, Key) ->
    Val = erlang:get(DictType),
    KeyPos = get_dict_key_pos(DictType),
    Val1 = lists:keydelete(Key, KeyPos, Val),
    erlang:put(DictType, Val1).

erase(DictType) ->
    erlang:put(DictType, []).

set_dict_key_pos(DictType, KeyPos) ->
    DictTypeKey = list_to_atom(lists:concat([DictType, key])),
    erlang:put(DictTypeKey, KeyPos).

get_dict_key_pos(DictType) ->
    DictTypeKey = list_to_atom(lists:concat([DictType, key])),
    erlang:get(DictTypeKey).

erase_dict_key_pos(DictType) ->
    DictTypeKey = list_to_atom(lists:concat([DictType, key])),
    erlang:erase(DictTypeKey).

get_dic(DicName) ->
	case erlang:get(DicName) of
		undefined ->
			[];
		Dic ->
			Dic
	end.