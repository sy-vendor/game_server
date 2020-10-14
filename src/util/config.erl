%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 配置操作函数
%%% @end
%%% Created : 30. 9月 2019 17:43
%%%-------------------------------------------------------------------

-module(config).
-include("config.hrl").

-export([
    get_log_level/0,
    get_log_path/0,
    get_mysql/0,
    is_debug/0
]).

%% @doc 日志系统级别
get_log_level() ->
    case application:get_env(game, log_level) of
        {ok, LogLevel} -> LogLevel;
        _ -> 3
    end.

%% @doc 日志文件路径
get_log_path() ->
    case application:get_env(game, log_path) of
        {ok, LogPath} -> LogPath;
        _ -> "../log"
    end.

%% @doc 获取MYSQL参数
get_mysql() ->
    Host1 = case application:get_env(game, db_host) of
        {ok, Host} -> Host;
        _ -> "localhost"
    end,
    Port1 = case application:get_env(game, db_port) of
        {ok, Port} -> Port;
        _ -> 3306
    end,
    Roler1 = case application:get_env(game, db_user) of
        {ok, Roler} -> Roler;
        _ -> "root"
    end,
    Pass1 = case application:get_env(game, db_pass) of
        {ok, Pass} -> Pass;
        _ -> "123456"
    end,
    Name1 = case application:get_env(game, db_name) of
        {ok, Name} -> Name;
        _ -> "sy_game_db"
    end,
    Encode1 = case application:get_env(game, db_encode) of
        {ok, Encode} -> Encode;
        _ -> utf8
    end,
    [Host1, Port1, Roler1, Pass1, Name1, Encode1].

%% @doc 是否测试环境
is_debug() ->
    case application:get_env(game, debug) of
        {ok, true} -> true;
        _ -> false
    end.
