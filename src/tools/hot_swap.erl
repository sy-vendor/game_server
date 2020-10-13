%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 本地热更模块
%%% @end
%%% Created : 30. 9月 2019 14:05
%%%-------------------------------------------------------------------

-module(hot_swap).

-export([
    load/1,
    network_load/1,
    network_load/2
]).


%% 当前节点热部署：
%% @param ModList 模块列表(原子列表)
load(ModList)->
    lists:foreach(fun(Module)-> 
        code:purge(Module), 
        code:load_file(Module)
    end,ModList).
                
%% 节点集热更：
%% @param ModList 模块列表(原子列表)         
network_load(ModList)->
    lists:foreach(fun(Module)-> 
        [begin rpc:call(Node, code, purge, [Module]),rpc:call(Node, code, load_file, [Module]) end || Node <- (nodes()++[node()])]
    end,ModList).
        
%% 指定单节点热更：
%% @param NodeList 节点集合(原子列表)
%% @param ModList 模块列表(原子列表)
network_load(NodeList, ModList)->
    lists:foreach(fun(Node)->
        lists:foreach(fun(Module)-> 
            rpc:call(Node, code, purge, [Module]),
            rpc:call(Node, code, load_file, [Module])
        end,ModList)
    end,NodeList).


