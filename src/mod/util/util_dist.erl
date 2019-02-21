%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 进程标识处理函数
%%% @end
%%% Created : 07. 十二月 2018 15:08
%%%-------------------------------------------------------------------
-module(util_dist).
-author("suyang").

%% API
-export([
    whereis_name/2,
    register/3,
    unregister/2,
    is_process_alive/1,
    role_process_name/1,
    get_role_process_pid/1,
    role_send_process_name/1,
    get_role_send_process_pid/1,
    get_child_count/1,
    get/1,
    get/2,
    put/2,
    add/2,
    erase/1
]).

%% @doc 获取进程ID
whereis_name(local, Atom) ->
    erlang:whereis(Atom);
whereis_name(global, Term) ->
    global:whereis_name(Term).

%% @doc 注册进程ID
register(local, Name, Pid) ->
    erlang:register(Name, Pid);
register(global, Name, Pid) ->
    global:re_register_name(Name, Pid).

%% @doc 解绑进程ID
unregister(local, Name) ->
    erlang:unregister(Name);
unregister(global, Name) ->
    global:unregister_name(Name).

%% @doc 判断进程是否存活
is_process_alive(Pid) ->
    try
        if
            is_pid(Pid) ->
                case node(Pid) =:= node() of
                    true ->
                        erlang:is_process_alive(Pid);
                    false ->
                        case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                            {badrpc, _Reason}  -> false;
                            Res -> Res
                        end
                end;
            true -> false
        end
    catch
        _:_ -> false
    end.

%% @doc 获取玩家进程名称
role_process_name(RoleId) ->
    {svr_role, RoleId}.

%% @doc 获取玩家进程ID
get_role_process_pid(RoleId) ->
    RegName = role_process_name(RoleId),
    util_dist:whereis_name(global, RegName).

%% @doc 获取玩家消息进程名称
role_send_process_name(RoleId) ->
    {mls, RoleId}.

%% @doc 获取玩家消息进程ID
get_role_send_process_pid(RoleId) ->
    RegName = role_send_process_name(RoleId),
    util_dist:whereis_name(global, RegName).

%% @doc 获取子进程数量
get_child_count(Atom) ->
    case whereis_name(local, Atom) of
        undefined ->
            0;
        _ ->
            [_,{active, ChildCount},_,_] = supervisor:count_children(Atom),
            ChildCount
    end.

%% @spec get(Key) -> Value | 0
%% @spec get(Key, Def) -> Value | Def
get(Key) -> ?MODULE:get(Key, 0).
get(Key, Def) ->
    case erlang:get(Key) of
        undefined -> Def;
        Value -> Value
    end.

%% @spec put(Key, Val) -> term()
put(Key, Val) ->
    erlang:put(Key, Val).

%% @spec add(Key, Val) -> term()
add(Key, Val) ->
    ?MODULE:put(Key, [Val | ?MODULE:get(Key, [])]).

erase(Key) ->
    erlang:erase(Key).




