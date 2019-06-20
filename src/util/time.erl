%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 时间处理模块
%%% @end
%%% Created : 14. 五月 2019
%%%-------------------------------------------------------------------
-module(time).
-include("common.hrl").

-export([
    unixtime/0,
    unixtime/1,
    unixtime_ms/0,
    unixtime_us/0,
    unixtime_to_now/1,
    unixtime_to_localtime/1,
    unixtime_to_string/1,
    unixdate/0,
    unixdate/1,
    next_unixdate/0,
    next_unixdate/1,
    get_day_of_week/0,
    get_day_of_week/1,
    get_secs_from_midnight/0,
    get_secs_from_midnight/1,
    is_today/1,
    is_yesterday/1,
    is_in_time/3,
    is_same_day/2,
    is_same_week/2,
    is_same_month/2,
    get_diff_days/1,
    get_diff_days/2,
    get_diff_days2/2,
    get_week_unixtime/0,
    get_week_unixtime/1,
    get_month_unixtime/0,
    get_month_unixtime/1,
    get_lastday_string/1,
    get_lastday_localtime/1,
    get_date_string/1,
    get_next_check_seconds/1,
    time_point_to_unixtime/1,
    time_point_to_string/1,
    get_time_point_floor/0,
    get_gregorian_days/0,
    get_week_day/1,
    add_days/2,
    now_to_iso_string/0,
    unixtime_to_iso_string/1,
    date_to_iso_string/0
]).

%% @doc 获取当前的Unix时间(秒)
unixtime() ->
 	svr_time:now_seconds().

%% @doc 获取某个时间点的Unix时间
%% @param LocalTime = {{Y,M,D},{H,M,S}} 
%%        erlang:localtime()或calendar:local_time()返回的Erlang本地时间。
unixtime(LocalTime) ->
    case catch unixtime2(LocalTime) of
        Timestamp when is_integer(Timestamp) -> Timestamp;
        _ -> 0
    end.

unixtime2(LocalTime) ->
    [UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),
    calendar:datetime_to_gregorian_seconds(UniversalTime) - ?DIFF_SECONDS_0000_1900.

%% @doc 获取当前的Unix时间(毫妙)
unixtime_ms() ->
	svr_time:now_milseconds().

%% @doc 获取当前的Unix时间(微妙)
unixtime_us() ->
	svr_time:now_usseconds().

%% @doc Unix时间转换成对应的Erlang时间
%% 返回 Now = {MegaSecs, Secs, _MicroSecs} Erlang时间
unixtime_to_now(UnixTime) ->
    M = UnixTime div 1000000,
    S = UnixTime rem 1000000,
    {M, S, 0}.

%% @doc Unix时间转换成对应的Localtime时间
%% 返回 LocalTime = {Date, Time} Localtime时间
unixtime_to_localtime(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    calendar:now_to_local_time(Now).

%% @doc Unix时间转为易辨别时间字符串
%% 例如： 1291014369 -> "2010年11月29日15时6分"
unixtime_to_string(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, _Second}} = calendar:now_to_local_time({Timestamp div 1000000, Timestamp rem 1000000, 0}),
    lists:concat([Year, "年", Month, "月", Day, "日", Hour, "时", Minute, "分"]).

%% @doc 获取当天零点的Unix时间
unixdate() ->
    Now = unixtime(),
    unixdate(Now).

%% @doc 获取某个时间对应零点的Unix时间
%% @param UnixTime Unix时间
unixdate(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {_, Time} = calendar:now_to_local_time(Now),
    DiffSecs = calendar:time_to_seconds(Time),
    {MegaSecs, Secs, _MicroSecs} = Now,
    MegaSecs * 1000000 + Secs - DiffSecs.

%% @doc 获取下一天零点时间
next_unixdate() ->
    next_unixdate(unixtime()).
next_unixdate(UnixTime) ->
    unixdate(UnixTime) + ?ONE_DAY_SECONDS.

%% @doc 获取今天是星期几
get_day_of_week() ->
    NowTime = unixtime(),
    get_day_of_week(NowTime).

%% @doc 获取指定的Unix时间是星期几
get_day_of_week(UnixTime) ->
    {Date, _Time} = unixtime_to_localtime(UnixTime),
    calendar:day_of_the_week(Date).

%% @doc 获取星期几的日期
get_week_day(Day) when Day > 0, Day =< 7 ->
    {Date, _Time} = erlang:localtime(),
    add_days(Date, Day - get_day_of_week()).

%% @doc 格式化当前时间
%% YYYY-MM-DD HH:mm:SS
now_to_iso_string() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second]
        )
    ).

%% @doc 格式化时间戳
%% YYYY-MM-DD HH:mm:SS
unixtime_to_iso_string(UnixTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = unixtime_to_localtime(UnixTime),
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second]
        )
    ).

%% @doc 格式化当前日期
%% YYYY-MM-DD
date_to_iso_string() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])).


%% @doc 获取当天0点到现在的秒数
get_secs_from_midnight() ->
    NowTime = unixtime(),
    get_secs_from_midnight(NowTime).

%% @doc 获取指定的Unix时间当天0点到当时的秒数
get_secs_from_midnight(UnixTime) ->
    {{_Year, _Month, _Day}, Time} = unixtime_to_localtime(UnixTime),
    calendar:time_to_seconds(Time).

%% @doc 判断是否是今天
is_today(Unixtime) ->
    is_same_day(unixtime(), Unixtime).


%% @doc 判断是否是昨天
is_yesterday(Unixtime) ->
    is_same_day(unixtime(), Unixtime + ?ONE_DAY_SECONDS).

%% @doc 判断是否在时间段内
is_in_time(Unixtime, Start, End) ->
    Start =< Unixtime andalso Unixtime =< End.


%% @doc 判断是否同一天
is_same_day(UnixTime1, UnixTime2) ->
    {{Year1, Month1, Day1}, _Time1} = unixtime_to_localtime(UnixTime1),
    {{Year2, Month2, Day2}, _Time2} = unixtime_to_localtime(UnixTime2),
    (Year1 =:= Year2) andalso (Month1 =:= Month2) andalso (Day1 =:= Day2).

%% @doc 判断是否同一星期
is_same_week(UnixTime1, UnixTime2) ->
    {MonUnixTime1, SunUnixTime1} = get_week_unixtime(UnixTime1),
    (UnixTime2 >= MonUnixTime1) andalso (UnixTime2 < SunUnixTime1).

%% @doc 判断是否同一月
is_same_month(UnixTime1, UnixTime2) ->
    {{Year1, Month1, _}, _} = unixtime_to_localtime(UnixTime1),
    {{Year2, Month2, _}, _} = unixtime_to_localtime(UnixTime2),
    (Year1 =:= Year2) andalso (Month1 =:= Month2).

%% @doc 计算现在和指定Unix时间相差的天数
get_diff_days(UnixTime) ->
    {Date1, _} = calendar:now_to_local_time(unixtime_to_now(UnixTime)),
    {Date2, _} = calendar:local_time(),
    calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

%% @doc 计算两个Unix时间相差的天数
get_diff_days(UnixTime1, UnixTime2) ->
    {{Year1, Month1, Day1}, _} = unixtime_to_localtime(UnixTime1),
    {{Year2, Month2, Day2}, _} = unixtime_to_localtime(UnixTime2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
    DiffDays = abs(Days2 - Days1),
    DiffDays.

get_diff_days2(UnixTime1, UnixTime2) ->
    {{Year1, Month1, Day1}, _} = unixtime_to_localtime(UnixTime1),
    {{Year2, Month2, Day2}, _} = unixtime_to_localtime(UnixTime2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
    Days2 - Days1.

%% @doc 增加天数
add_days({Y, M, D}, Day) ->
    Days = calendar:date_to_gregorian_days(Y, M, D),
    NewDays = Days + Day,
    calendar:gregorian_days_to_date(NewDays).

%% @doc 当前天数
get_gregorian_days() ->
    {Date, _} = calendar:local_time(),
    calendar:date_to_gregorian_days(Date).

%% @doc 获取一周的Unix时间范围
get_week_unixtime() ->
    NowTime = unixtime(),
    get_week_unixtime(NowTime).

get_week_unixtime(UnixTime) ->
    {Date, Time} = unixtime_to_localtime(UnixTime),
    % 星期几
    Week = calendar:day_of_the_week(Date),
    % 从午夜到现在的秒数
    DiffSecs = calendar:time_to_seconds(Time),
    % 星期一和星期天的Unix时间
    MonUnixTime = UnixTime - DiffSecs - (Week - 1) * ?ONE_DAY_SECONDS,
    SunUnixTime = MonUnixTime + 7 * ?ONE_DAY_SECONDS,
    {MonUnixTime, SunUnixTime}.

%% @doc 获得一个月的Unix时间范围
get_month_unixtime() ->
    NowTime = unixtime(),
    get_month_unixtime(NowTime).

get_month_unixtime(UnixTime) ->
    {{Year, Month, _}, _} = unixtime_to_localtime(UnixTime),
    LastDay = calendar:last_day_of_the_month(Year, Month),
    FirstDayUnixTime = unixtime({{Year, Month, 1}, {0, 0, 0}}),
    LastDayUnixTime = unixtime({{Year, Month, LastDay}, {23, 59, 59}}) + 1,
    {FirstDayUnixTime, LastDayUnixTime}.

%% @doc 获取上一天的易辨别时间字符串
get_lastday_string(Date) ->
    {LastDate, _} = get_lastday_localtime({Date, {0, 0, 0}}),
    get_date_string(LastDate).

%% @doc 获取上一天的LocalTime时间
get_lastday_localtime(LocalTime) ->
    NowTime = unixtime(LocalTime),
    LastDayUnixTime = NowTime - ?ONE_DAY_SECONDS,
    LastDayNow = time:unixtime_to_now(LastDayUnixTime),
    calendar:now_to_local_time(LastDayNow).

get_date_string(Date) ->
    {Year, Month, Day} = Date,
    io_lib:format("~p年~p月~p日", [Year, Month, Day]).

%% @doc 获取下次检查间隔秒数
get_next_check_seconds(GapSeconds) when GapSeconds > 0, GapSeconds =< ?ONE_DAY_SECONDS ->
    Seconds = time:get_secs_from_midnight(),
    NextCheck = GapSeconds - (Seconds rem GapSeconds),
    NextCheck.


%% @doc 时间点转时间戳 (半小时一个点 1-48)
time_point_to_unixtime(TimePoint) when 0 < TimePoint andalso TimePoint =< 48 ->
    unixdate() + (TimePoint - 1) * ?HALF_HOUR;
time_point_to_unixtime(_) -> 0.

%% @doc 时间点转易辨别的时间字符串
time_point_to_string(TimePoint) when 0 < TimePoint andalso TimePoint =< 48 ->
    lists:flatten(io_lib:format("~2.10.0b:~2.10.0b", [(TimePoint - 1) div 2, ((TimePoint - 1) rem 2) * 30]));
time_point_to_string(_) -> "".

%% @doc 获取当前时间点（向下取）
get_time_point_floor() ->
    {{_, _, _}, {Hour, Min, _S}} = unixtime_to_localtime(unixtime()),
    case Min >= 30 of
        true -> Hour * 2 + 2;
        false -> Hour * 2 + 1
    end.