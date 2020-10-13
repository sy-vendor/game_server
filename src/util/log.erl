%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 错误日志模块
%%% @end
%%% Created : 29. 9月 2019 20:34
%%%-------------------------------------------------------------------

-module(log).
-include("common.hrl").

-export([
    log/5,
    errlog/2
]).

%% @doc 系统运行日志
log(Type, Format, Args, Mod, Line) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    File =
        case get("errlog") of
            % 首次打开
            undefined ->
                % DateStr = time:date_to_iso_string(),
                % FileName = "errlog_" ++ DateStr,
                FileName = io_lib:format("errlog_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = config:get_log_path() ++ "/" ++ type:unicode_string(FileName),
                {ok, File1} = file:open(PathName, [write, append]),
                put("errlog", {File1, {Year, Month, Day}}),
                File1;
            % 已经打开
            {File2, {Year, Month, Day}} ->
                File2;
            % 需要切换
            {File2, _} ->
                file:close(File2),
                % DateStr = time:date_to_iso_string(),
                % FileName = "errlog_" ++ DateStr,
                FileName = io_lib:format("errlog_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = config:get_log_path() ++ "/" ++ type:unicode_string(FileName),
                {ok, File3} = file:open(PathName, [write, append]),
                put("errlog", {File3, {Year, Month, Day}}),
                File3
        end,
    Format1 = type:unicode_string("#" ++ Type ++ " ~p-~p-~p ~p:~p:~p[~w:~w] \r\n" ++ Format ++ "\r\n~n"),
    io:format(File, Format1, [Year, Month, Day, Hour, Min, Sec, Mod, Line] ++ Args).

%% @log 日志打印
errlog(Format, Args) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    File =
        case get("errlog") of
            % 首次打开
            undefined ->
                % DateStr = time:date_to_iso_string(),
                % FileName = "errlog_" ++ DateStr,
                FileName = io_lib:format("errlog_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = config:get_log_path() ++ "/" ++ type:unicode_string(FileName),
                {ok, File1} = file:open(PathName, [write, append]),
                put("errlog", {File1, {Year, Month, Day}}),
                File1;
            % 已经打开
            {File2, {Year, Month, Day}} ->
                File2;
            % 需要切换
            {File2, _} ->
                file:close(File2),
                % DateStr = time:date_to_iso_string(),
                % FileName = "errlog_" ++ DateStr,
                FileName = io_lib:format("errlog_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = config:get_log_path() ++ "/" ++ type:unicode_string(FileName),
                {ok, File3} = file:open(PathName, [write, append]),
                put("errlog", {File3, {Year, Month, Day}}),
                File3
        end,
    Format1 = type:unicode_string("#error" ++ " ~p-~p-~p ~p:~p:~p \r\n" ++ Format ++ "\r\n~n"),
    io:format(File, Format1, [Year, Month, Day, Hour, Min, Sec] ++ Args).