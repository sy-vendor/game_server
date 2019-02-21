%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 时间相关的util
%%% @end
%%% Created : 07. 十二月 2018 12:22
%%%-------------------------------------------------------------------
-module(util_time).
-author("suyang").

-export([
    unixtime/0,
    unixtime/1,
    datetime_to_seconds/1,
    to_unixtime/1,
    seconds_to_datetime/1,
    to_datetime/1,
    day_of_the_week/1,
    day_of_the_week/0,
    is_same_day/1,
    is_same_day/2,
    is_same_week/2,
    is_today/1,
    time_left/1,
    time_left/2,
    valid_datetime/1,
    valid_time/1,
    day_diff/2,
    time_string/0,
    unixdate/1,
    get_secs_from_midnight/1
]).

-export([
    get_now/0,
    get_dt/0,
    get_date/0,
    get_date/1,
    get_time/0
]).

-include("common.hrl").

%% @spec time_string() -> <<"2016-07-29 20:29:01">>
time_string() ->
    {{Y, M, D}, {H, I, S}} = get_dt(),
    util:fbin("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, M, D, H, I, S]).

%% @spec get_now() -> {S1, S2, S3}
%% S1 = S2 = S3 = Shift = integer()
%% {S1, S2, S3} = os:timestamp(), 并加入设置的偏移量
get_now() ->
    case sys_env:get(cur_time) of
        {S1, S2, S3, Shift} ->
            {S1, S2 + Shift, S3};
        _ ->
            os:timestamp()
    end.

%% 获取当前日期
%% @spec get_dt() -> {{year(), month(), day()}, {hour(), minute(), seconds()}}
get_dt() ->
    to_datetime(unixtime()).

%% @spec get_date() -> {year(), month(), day()}
%% 获取当前日期
get_date() ->
    {Data, _} = get_dt(),
    Data.

get_date(yesterday) ->
    {Data, _} = to_datetime(unixtime() - ?DATE_SECONDS),
    Data;
get_date(tomorrow) ->
    {Data, _} = to_datetime(unixtime() + ?DATE_SECONDS),
    Data.

%% @spec get_time() -> {hour(), minute(), seconds()}
%% 获取当前时间点
get_time() ->
    {_, Time} = get_dt(),
    Time.

%% @spec unixtime() -> Timestamp
%% Timestamp = integer()
%% @doc 取得当前的unix时间戳
unixtime() ->
    {M, S, _Ms} = get_now(),
    M * 1000000 + S.

%% @spec unixtime(ms) -> Timestamp
%% Timestamp = integer()
%% @doc 取得当前的unix时间戳，精确到毫秒
unixtime(ms) ->
    {S1, S2, S3} = os:timestamp(),
    trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);


%% 获取当天0时0分0秒的时间戳（这里是相对于当前时区而言，后面的unixtime调用都基于这个函数
unixtime(today) ->
    {M, S, MS} = get_now(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}), %% 性能几乎和之前的一样
    M * 1000000 + S - calendar:time_to_seconds(Time);

%% 获取某时间戳的00:00:00的时间戳当Unixtime为0时，返回值有可能是负值，因为这里有时区偏移值
unixtime({today, Unixtime}) ->
    Base = unixtime(today),  %% 当前周一
    case Unixtime > Base of
        false -> Base - util_math:ceil((Base - Unixtime) / 86400) * 86400;
        true -> (Unixtime - Base) div 86400 * 86400 + Base
    end;

%% 获取明天0点时间戳
unixtime(tomorrow) ->
    unixtime(today) + 86400;

%% @spec unixtime({tomorrow, UnixTime}) -> UnixTime
%% @doc 获取明天的00:00:00的时间戳
unixtime({tomorrow, UnixTime}) ->
    unixtime({today, UnixTime}) + 86400;

%% @spec unixtime({nexttime, X}) -> NextTime;
%% @spec 当前距离每天某个时刻的时间
%% 如当前9:00 距离10:00为3600秒 返回3600
%% 如当前时间 23:00 距离 1:00为7200秒 返回7200
unixtime({nexttime, X}) ->
    Now = unixtime(),
    TodayStartTime = unixtime({today, Now}),
    BaseTime = TodayStartTime + X, %% 取当天距离X的时间为指定时间
    case BaseTime > Now of
        true -> BaseTime - Now; %% 当前时间比指定时间小 直接返回差距
        false -> BaseTime + 86400 - Now %% 当前时间比指定时间大 加上一天时间后求差
    end;

%% @spec unixtime(nextweek) -> NextTime;
%% @doc 当前时间的下一周周一0点的时间戳
unixtime(nextweek) ->
    Tom = unixtime(tomorrow),
    Wd = day_of_the_week(),
    Tom + (7 - Wd) * 86400;

%% @spec unixtime(nextweek, UnixTime) -> NextTime;
%% @doc 目标时间的下一周周一0点的时间戳
unixtime({nextweek, X}) ->
    Tom = unixtime({tomorrow, X}),
    Wd = day_of_the_week(X),
    Tom + (7 - Wd) * 86400.

%% @doc 获取某个时间对应零点的Unix时间
%% @param UnixTime Unix时间
unixdate(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {_, Time} = calendar:now_to_local_time(Now),
    DiffSecs = calendar:time_to_seconds(Time),
    {MegaSecs, Secs, _MicroSecs} = Now,
    MegaSecs * 1000000 + Secs - DiffSecs.

%% @doc Unix时间转换成对应的Erlang时间
%% 返回 Now = {MegaSecs, Secs, _MicroSecs} Erlang时间
unixtime_to_now(UnixTime) ->
    M = UnixTime div 1000000,
    S = UnixTime rem 1000000,
    {M, S, 0}.

%% @spec datetime_to_seconds(DateTime) -> false | SecondsTime
%% DateTime = {{2011,11,15},{16,14,57}} = {{Y, M, D}, {h, m, s}}
%% @doc 将日期转换unix时间戳
datetime_to_seconds(0) -> 0;
datetime_to_seconds(DateTime) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
        [] -> false;
        [_, Udate] ->
            calendar:datetime_to_gregorian_seconds(Udate) - 719528 * 24 * 3600;
        [Udate] ->
            calendar:datetime_to_gregorian_seconds(Udate) - 719528 * 24 * 3600
    end.

%% @spec to_unixtime(DateTime) -> false | Unixtime
%% DateTime = {{2011,11,15},{16,14,57}} = {{Y, M, D}, {h, m, s}}
%% Unixtime = integer()
%% @doc 将日期转换unix时间戳
to_unixtime(DateTime) ->
    datetime_to_seconds(DateTime).

%% @spec seconds_to_datetime(Unixtime) -> {{Y, M, D}, {h, m, s}}
%% Unixtime = unix时间戳
%% @doc 将unix时间戳转换成当地日期
seconds_to_datetime(Unixtime) ->
    Local = erlang:universaltime_to_localtime({{1970, 1, 1}, {0, 0, 0}}),
    LocalStamp = calendar:datetime_to_gregorian_seconds(Local),
    TimeStamp = Unixtime + LocalStamp,
    calendar:gregorian_seconds_to_datetime(TimeStamp).

%% @spec to_datetime(Unixtime) -> {{Y, M, D}, {h, m, s}}
%% Unixtime = unix时间戳
%% @doc 将unix时间戳转换成当地日期
to_datetime(Unixtime) ->
    seconds_to_datetime(Unixtime).

%% @spec day_of_the_week(Unixtime) -> integer()
%% Unixtime = unix时间戳
%% @doc 将unix时间戳转换成星期几
day_of_the_week(Unixtime) ->
    {Date, _} = seconds_to_datetime(Unixtime),
    calendar:day_of_the_week(Date).

%% @spec day_of_the_week() -> integer()
%% @doc 获取当前时间是周几
day_of_the_week() ->
    calendar:day_of_the_week(get_date()).

%% @spec is_same_day(Time1, Time2) -> boolean()
%% Time1 = Time2 = integer()
%% @doc 根据unix时间戳判断是否为同一天
is_same_day(Ts) ->
    is_same_day(Ts, unixtime()).

is_same_day(OldTime, NewTime) when is_integer(OldTime) andalso is_integer(NewTime) ->
    Day1 = unixtime({today, OldTime}),
    Day2 = unixtime({today, NewTime}),
    Day1 =:= Day2;

%% @spec is_same_day(Timestamp1, Timestamp2) -> true | false
%% Timestamp1 = erlang:Timestamp()
%% Timestamp2 = erlang:Timestamp()
%% @doc 判断2个时间戳是否同一天
is_same_day(Timestamp1, Timestamp2) ->
    case {Timestamp1, Timestamp2} of
        {{_, _, _}, {_, _, _}} ->
            {{Y1, M1, D1}, _} = calendar:now_to_local_time(Timestamp1),
            case calendar:now_to_local_time(Timestamp2) of
                {{Y1, M1, D1}, _} -> true;
                _ -> false
            end;
        _ -> false
    end.

%% @spec is_same_week(Date1, Date2) -> bool()
%% @doc 判断是否同一周
is_same_week(Date1, Date2) ->
    calendar:iso_week_number(Date1) =:= calendar:iso_week_number(Date2).

%% @spec is_today(Time) -> boolean()
%% Time = unixtime()
%% @doc 判断是否为今天时间
is_today(Time) ->
    Now = unixtime(),
    is_same_day(Time, Now).

%% @spec time_left(TimeMax::integer(), Begin::erlang:timestamp()) -> integer()
%% @doc 计算剩余时间，单位：毫秒
time_left(TimeMax, Begin) ->
    TL = util_math:floor(TimeMax - timer:now_diff(get_now(), Begin) / 1000),
    case TL > 0 of
        true -> TL;
        false -> 0
    end.
%% @doc 计算剩余时间，单位：秒
time_left({_H, _M, _S} = Time) ->
    Target = calendar:time_to_seconds(Time),
    Now = calendar:time_to_seconds(get_time()),
    case Now < Target of
        true -> Target - Now;
        false -> Target + ?DATE_SECONDS - Now
    end.

%% @spec valid_datetime(datetime()) -> true | false
%% @doc 检测datetime()是否为有效的
valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> false
    end;
valid_datetime(_) -> false.

%% @spec valid_time(time()) -> true | false
%% 检测time是否有效
valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H >= 0, H < 24,
    M >= 0, M < 60,
    S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.

%% @spec day_diff(UnixTime, UnixTime) -> int()
%% @doc 两个unixtime相差的天数,相邻2天返回1
day_diff(FromTime, ToTime) when ToTime > FromTime ->
    FromDate = unixtime({today, FromTime}),
    ToDate = unixtime({today, ToTime}),
    case (ToDate - FromDate) / (3600 * 24) of
        Diff when Diff < 0 -> 0;
        Diff -> round(Diff)
    end;

day_diff(FromTime, ToTime) when ToTime =:= FromTime ->
    0;

day_diff(FromTime, ToTime) ->
    day_diff(ToTime, FromTime).

%% @doc 获取指定的Unix时间当天0点到当时的秒数
get_secs_from_midnight(UnixTime) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_datetime(UnixTime),
    calendar:time_to_seconds(Time).


