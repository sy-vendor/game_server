%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2018 14:09
%%%-------------------------------------------------------------------
-module(lib_center).
-author("suyang").

%% API
-export([init/0,
    get_srv/1,
    get_srvs/0,
    convert_srv/1,
    cast/3,
    delete/1,
    save/1]).

-include("common.hrl").
-include("center.hrl").

%% @doc 初始化
init() ->
    ets:new(server_srv_online, [set, named_table, public, {keypos, #server_srv.srv_id}]).

%% @spec get_srv(SrvId) -> false | #server_srv{}
%% 获取指定的server_srv
get_srv(SrvId) ->
    case ets:lookup(server_srv_online, SrvId) of
        [Mirror = #server_srv{}] -> Mirror;
        _ -> false
    end.

%% @spec get_srvs() -> [#server_srv{} | ..]
%% 获取所有server_srv
get_srvs() ->
    ets:tab2list(server_srv_online).

%% @spec save(Ss) -> ok.
%% Ss = #server_srv{}
%% 保存在线的server_srv
save(Ss) ->
    ets:insert(server_srv_online, Ss).

%% @spec delete(Key) -> ok.
%% Key = SrvId | Pid
%% SrvId = integer()
%% Pid = pid()
%% 删除在线的server_srv
delete(SrvId) when is_integer(SrvId) ->
    ets:delete(server_srv_online, SrvId);
delete(Pid) when is_pid(Pid) ->
    ets:match_delete(server_srv_online, #server_srv{_ = '_', pid = Pid}).

%% @doc server_srv与server_srv_data之间的转换
convert_srv(#server_srv_data{srv_id = SrvId, name = Name, node = Node, host = Host, cookie = Cookie}) ->
    %% 把node转成atom，如：'XXX@YYY.com'
    Node1 = case is_bitstring(Node) of
                true -> list_to_atom(bitstring_to_list(Node));
                false ->
                    case is_list(Node) of
                        true -> list_to_atom(Node);
                        false -> Node
                    end
            end,
    %% 把cookie转成atom，如：'game'
    Cookie1 = case is_bitstring(Cookie) of
                  true -> list_to_atom(bitstring_to_list(Cookie));
                  false ->
                      case is_list(Cookie) of
                          true -> list_to_atom(Cookie);
                          false -> Cookie
                      end
              end,
    #server_srv{srv_id = SrvId, name = Name, node = Node1, host = Host, cookie = Cookie1};
convert_srv(#server_srv{srv_id = SrvId, name = Name, node = Node, host = Host, cookie = Cookie}) ->
    Node1 = atom_to_list(Node),
    Cookie1 = atom_to_list(Cookie),
    #server_srv_data{srv_id = SrvId, name = Name, node = Node1, host = Host, cookie = Cookie1}.

%% @spec cast(M, F, A) -> ok
%% M = F = atom()
%% A = term()
cast(M, F, A) ->
    L = get_srvs(),
    do_cast(L, M, F, A).

%% 广播
do_cast([], _M, _F, _A) -> ok;
do_cast([SrvId| T], M, F, A) when is_integer(SrvId)->
    do_cast([get_srv(SrvId)| T], M, F, A);
do_cast([#server_srv{srv_id = _SrvId, node = Node} | T], M, F, A) ->
    rpc:cast(Node, M, F, A),
    do_cast(T, M, F, A);
do_cast([_|T], M, F, A) ->
    do_cast(T, M, F, A).