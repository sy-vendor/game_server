%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 玩家数据
%%% @end
%%% Created : 19. 十二月 2018 17:09
%%%-------------------------------------------------------------------
-module(lib_data).
-author("suyang").

%% API
-export([data_load/1,
    data_save/1,
    data_save/2,
    get_from_db/2,
    put_to_db/3,
    add_to_db/3,
    kv_data/1,
    get_from_dict/2,
    put_to_dict/3,
    add_to_dict/3,
    format_role_data/1]).

-include("common.hrl").
-include("role.hrl").

%% 数据保存的Key
-define(KEY_ROLE_KV_DATA, role_kv_data).
%% 数据库相关
-define(SQL_KV_DATA_GET, <<"select `data` from `role_kv_data` where `role_id`=~p">>).
-define(SQL_KV_DATA_REPLACE, <<"replace into `role_kv_data` (`role_id`, `data`) values (~p, '~s')">>).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 加载数据
data_load(RoleId) ->
    Sql = io_lib:format(?SQL_KV_DATA_GET, [RoleId]),
    case db:get_one(Sql) of
        {ok, DataBin} -> dict:from_list(util_type:bitstring_to_term(DataBin));
        _ -> dict:new()
    end.

%% @doc 保存数据
data_save(RoleId) ->
    data_save(RoleId, kv_data(RoleId)).
data_save(RoleId, Data) ->
    DataBin = util_type:term_to_bitstring(dict:to_list(Data)),
    Sql = io_lib:format(?SQL_KV_DATA_REPLACE, [RoleId, DataBin]),
    db:execute(Sql).

%% @doc 离线获取
get_from_db(RoleId, Key) ->
    case dict:find(Key, data_load(RoleId)) of
        error -> undefined;
        {ok, Value} -> Value
    end.

%% @doc 离线保存
put_to_db(RoleId, Key, Value) ->
    data_save(RoleId, dict:store(Key, Value, data_load(RoleId))).

%% @doc 离线增加
add_to_db(RoleId, Key, Value) ->
    data_save(RoleId, dict:update_counter(Key, Value, data_load(RoleId))).

%% @doc 键值数据
kv_data(RoleId) ->
    case erlang:get(?KEY_ROLE_KV_DATA) of
        undefined ->
            Data = data_load(RoleId),
            erlang:put(?KEY_ROLE_KV_DATA, Data),
            Data;
        Data -> Data
    end.

%% @doc 在线获取
get_from_dict(RoleId, Key) ->
    case dict:find(Key, kv_data(RoleId)) of
        error -> undefined;
        {ok, Value} -> Value
    end.

%% @doc 在线保存
put_to_dict(RoleId, Key, Value) ->
    erlang:put(?KEY_ROLE_KV_DATA, dict:store(Key, Value, kv_data(RoleId))).


%% @doc 在线增加
add_to_dict(RoleId, Key, Value) ->
    erlang:put(?KEY_ROLE_KV_DATA, dict:update_counter(Key, Value, kv_data(RoleId))).

%% @doc 后台打印数据
format_role_data(Data) ->
    case catch dict:to_list(erlang:binary_to_term(Data)) of
        List when is_list(List) ->
            util_type:term_to_string(List);
        _ -> null
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================