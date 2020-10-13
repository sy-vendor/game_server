%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 进程标识处理函数
%%% @end
%%% Created : 08. 10月 2019 09:10
%%%-------------------------------------------------------------------

-module(dist).
-include("common.hrl").

-export([
    whereis_name/2,
    register/3,
    unregister/2,
    is_process_alive/1,
    get_child_count/1,
    pg2_get_members/1
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

%% @doc 获取子进程数量
get_child_count(Atom) ->
    case whereis_name(local, Atom) of
        undefined ->
            0;
        _ ->
            [_,{active, ChildCount},_,_] = supervisor:count_children(Atom),
            ChildCount
    end.

pg2_get_members(Pg2_name) ->
    L = case pg2:get_members(Pg2_name) of
            {error, _} ->
                timer:sleep(100),
                pg2:get_members(Pg2_name);
            Other when is_list(Other) ->
                Other
        end,
    if  not is_list(L) -> [];
        true -> lists:usort(L)
    end.



