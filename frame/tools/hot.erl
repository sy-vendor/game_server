%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 热更模块
%%% @end
%%% Created : 02. 一月 2019 10:58
%%%-------------------------------------------------------------------
-module(hot).
-author("suyang").

%% API
-export([load/1, network_load/1, network_load/2]).


%% @spec ModList 模块列表(原子列表)
%% @doc 当前节点热部署
load(ModList) ->
    lists:foreach(fun(Module) ->
        code:purge(Module),
        code:load_file(Module)
                  end, ModList).

%% @spec ModList 模块列表(原子列表)
%% @doc 节点集热更
network_load(ModList) ->
    lists:foreach(fun(Module) ->
        [begin rpc:call(Node, code, purge, [Module]), rpc:call(Node, code, load_file, [Module]) end || Node <- (nodes() ++ [node()])]
                  end, ModList).

%% @spec NodeList 节点集合(原子列表)
%% @spec ModList 模块列表(原子列表)
%% @doc 指定单节点热更
network_load(NodeList, ModList) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            rpc:call(Node, code, purge, [Module]),
            rpc:call(Node, code, load_file, [Module])
                      end, ModList)
                  end, NodeList).