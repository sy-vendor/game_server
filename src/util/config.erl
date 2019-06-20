%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 配置操作函数
%%% @end
%%% Created : 14. 五月 2019
%%%-------------------------------------------------------------------

-module(config).
-include("config.hrl").

-export([
    get_log_level/0,
    get_log_path/0,
    get_ticket/0,
    get_mysql/0,
    get_log_mysql/0,
    get_zone/0,
    get_plat_id/0,
    get_platform/0,
    get_platform/1,
    get_server_num/0,
    get_server_num/1,
    get_server_num2/0,
    is_debug/0,
    get_tcp_options/0,
    get_acceptor_num/0
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
        _ -> "game_alarm.log"
    end.

%% @doc 私钥
get_ticket() ->
    case application:get_env(game, ticket) of
        {ok, Ticket} -> Ticket;
        _ -> "ticket"
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
        _ -> "root"
    end,
    Name1 = case application:get_env(game, db_name) of
        {ok, Name} -> Name;
        _ -> "test"
    end,
    Encode1 = case application:get_env(game, db_encode) of
        {ok, Encode} -> Encode;
        _ -> utf8
    end,
    [Host1, Port1, Roler1, Pass1, Name1, Encode1].

%% @doc 获取MYSQL参数
get_log_mysql() ->
    Host1 = case application:get_env(game, log_db_host) of
        {ok, Host} -> Host;
        _ -> "localhost"
    end,
    Port1 = case application:get_env(game, log_db_port) of
        {ok, Port} -> Port;
        _ -> 3306
    end,
    Roler1 = case application:get_env(game, log_db_user) of
        {ok, Roler} -> Roler;
        _ -> "root"
    end,
    Pass1 = case application:get_env(game, log_db_pass) of
        {ok, Pass} -> Pass;
        _ -> "root"
    end,
    Name1 = case application:get_env(game, log_db_name) of
        {ok, Name} -> Name;
        _ -> "log"
    end,
    Encode1 = case application:get_env(game, log_db_encode) of
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

%% @doc 获取区域信息
get_zone() ->
    case application:get_env(game, zone) of
        {ok, Zone} -> Zone;
        _ -> "cn"
    end.

%% @doc 获取平台标识（别名）
get_plat_id() ->
    case application:get_env(game, plat_id) of
        {ok, PlatId} -> PlatId;
        _ -> ""
    end.

%% @doc 获取平台标识
get_platform() ->
    case application:get_env(game, platform) of
        {ok, Platform} -> Platform;
        _ -> ""
    end.

%% @doc 获取平台标识
get_platform(Platform) ->
    case catch list_to_integer(Platform) of
        PlatformT when is_integer(PlatformT) -> PlatformT;
        _ -> 0
    end.

%% @doc 获取当前所在服务器ID -- 整型形式
get_server_num() ->
    case application:get_env(game, server_num) of
        {ok, ServerNum} when is_list(ServerNum) ->
            Len = length(ServerNum),
            case Len > 1 of
                true -> list_to_integer(lists:sublist(ServerNum, 2, Len));
                false -> 0
            end;
        _ ->
            0
    end.

%% @doc 获取当前所在服务器ID -- 简化整型形式
get_server_num(ServerNum) when is_integer(ServerNum) ->
    case ServerNum div 10000 * 10000 of
        0 -> ServerNum;
        Platform -> ServerNum rem Platform
    end;
get_server_num(ServerNum) -> ServerNum.
     
%% @doc 获取当前所在服务器ID -- 字符串形式
get_server_num2() ->
    case application:get_env(game, server_num) of
        {ok, ServerNum} -> ServerNum;
        _ -> ""
    end.

%% @doc 获取连接参数
get_tcp_options() ->
    case application:get_env(game, tcp_options) of
        {ok, TcpOptions} -> TcpOptions;
        _ -> []
    end.

%% @doc 获取acceptor启动数
get_acceptor_num() ->
    case application:get_env(game, tcp_acceptor_num) of
        {ok, AcceptorNum} -> AcceptorNum;
        _ -> 0
    end.

