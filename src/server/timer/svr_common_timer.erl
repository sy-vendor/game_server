%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 通用定时器
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_common_timer).
-author("sy").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-export([add_timer/1, remove_timer/2, restart_timer/1, p/0, call/1, cast/1]).

-export([start_link/1]).

-define(SERVER, ?MODULE).

-include("common.hrl").
-include("timer.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 动态添加定时器
%%      新添加的定时器#timer_info中的module和func，不能在timer.hrl已经有配置
add_timer(TimerInfo) when is_record(TimerInfo, timer_info) ->
  cast({add_timer, TimerInfo});
add_timer(_TimerInfo) -> ignore.

%% @doc 动态移除定时器
%%      本方法以Module和Func作为唯一索引，找到则移除
remove_timer(Module, Func) ->
  cast({remove_timer, Module, Func}).

%% @doc 重启定时器(重新读取一次配置，马上又刷新一次)
restart_timer(NodeType) ->
  cast({restart_timer, NodeType}).

%% @doc 进程ID
p() ->
  dist:whereis_name(local, ?MODULE).

%% @doc call函数
call(Msg) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_statem:call(Pid, Msg);
    _ ->
      {error, ?ERR_COMMON_SERVICE_NOT_START}
  end.

%% @doc cast函数
cast(Msg) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_statem:cast(Pid, Msg);
    _ ->
      ignore
  end.
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(NodeType) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [NodeType], []).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([NodeType]) ->
  State = init_timer_data(NodeType),
  Action = {timeout, ?INIT_TIMEOUT * 1000, {wait_time_out}},
  {ok, working, State, Action}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
  handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%% 处理定时业务
handle_event(timeout, {wait_time_out}, working, State) ->
  NowTime = time:unixtime(),
  DayTime = time:unixdate(NowTime),
  Week = time:get_day_of_week(NowTime),
  #timer_state{day = DayT, daytime = LastDayTime, list = TimerList} = State,
  NewState = case DayTime > LastDayTime of
               true -> % 12点重置时间列表
                 {{_Year, _Month, Day}, _Time} = time:unixtime_to_localtime(NowTime),
                 NewTimerList = check_timer_list(NowTime, DayTime, Day, Week, TimerList, []),
                 NewTimerList2 = trigger_timer(NowTime, DayTime, Day, Week, NewTimerList, []),
                 State#timer_state{
                   daytime = DayTime,
                   day = Day,
                   week = Week,
                   list = NewTimerList2
                 };
               _ ->
                 NewTimerList = trigger_timer(NowTime, DayTime, DayT, Week, TimerList, []),
                 State#timer_state{list = NewTimerList}
             end,
  Action = {timeout, ?TIMER_REFRESH * 1000, {wait_time_out}},
  {next_state, working, NewState, Action};
%% 重置定时器
handle_event(cast, {restart_timer, NodeType}, _StateName, _State) ->
  NewState = init_timer_data(NodeType),
  Action = {timeout, 0, {wait_time_out}},
  {next_state, working, NewState, Action};
%% 添加定时器
handle_event(cast, {add_timer, TimeInfo}, _StateName, State) ->
  #timer_state{list = TimerList} = State,
  NewState = State#timer_state{list = [TimeInfo|TimerList]},
  Action = {timeout, 0, {wait_time_out}},
  {next_state, working, NewState, Action};
%% 移除定时器
handle_event(cast, {remove_timer, Module, Func}, _StateName, State) ->
  #timer_state{list = TimerList} = State,
  NewTimerList = lists:filter(fun(#timer_info{module = ModuleT, func = FuncT}) ->
    case ModuleT =:= Module andalso FuncT =:= Func of
      true -> false;
      _ -> true
    end
                              end, TimerList),
  NewState = State#timer_state{list = NewTimerList},
  Action = {timeout, 0, {wait_time_out}},
  {next_state, working, NewState, Action};
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 初始化定时器数据
init_timer_data(NodeType) ->
  TimerList = case NodeType of
                kfcenter -> ?TIMER_KFCLIENT_LIST;
                logic -> ?TIMER_LOGIC_LIST;
                _ -> []
              end,
  NowTime = time:unixtime(),
  DayTime = time:unixdate(NowTime),
  Week = time:get_day_of_week(NowTime),
  {{_Year, _Month, Day}, _Time} = time:unixtime_to_localtime(NowTime),
  TimerListN = init_timer_list(NowTime, DayTime, Day, Week, TimerList, []),
  #timer_state{
    daytime = DayTime,
    day = Day,
    week = Week,
    list = TimerListN
  }.

%% 初始化定时器列表
init_timer_list(_NowTime, _DayTime, _DayOfMonth, _Week, [], List) -> List;
init_timer_list(NowTime, DayTime, DayOfMonth, Week, [Timer|L], List) ->
  {TriggerOnStart, Module, Func, Args, DayList, WeekList, {StartHour, StartMinute, StartSecond}, {EndHour, EndMinute, EndSecond}, Gap} = Timer,
  StartTime = DayTime + StartHour * 3600 + StartMinute * 60 + StartSecond,
  EndTime = DayTime + EndHour * 3600 + EndMinute * 60 + EndSecond,
  NextTime = case check_is_triggered(Week, DayOfMonth, WeekList, DayList) of
               true ->
                 case TriggerOnStart of
                   % 服务启动就需要触发一次
                   1 -> NowTime;
                   _ ->
                     case Gap of
                       % 指定时间只触发一次
                       0 ->
                         if
                           NowTime =< StartTime -> StartTime;
                           true -> StartTime + 86400
                         end;
                       % 间隔时间触发一次
                       _ ->
                         if
                           [StartHour, StartMinute, StartSecond, EndHour, EndMinute, EndSecond] =:= [0, 0, 0, 0, 0, 0] -> NowTime;
                           NowTime =< StartTime -> StartTime;
                           NowTime > StartTime andalso NowTime =< EndTime -> NowTime;
                           true -> 0
                         end
                     end
                 end;
               _ -> 0
             end,
  TimerInfo = #timer_info{
    next_time = NextTime,
    module = Module,
    func = Func,
    args = Args,
    day_list = DayList,
    week_list = WeekList,
    start_hour = StartHour,
    start_minute = StartMinute,
    start_second = StartSecond,
    end_hour = EndHour,
    end_minute = EndMinute,
    end_second = EndSecond,
    gap = Gap
  },
  init_timer_list(NowTime, DayTime, DayOfMonth, Week, L, [TimerInfo | List]).

%% 检查今天是否要执行操作
%% 判断星期及日期
%% @param:: Week: 今天是星期几
%%          Day: 今天是几号
%%          WeekList: 定时器配置的周期列表
%%          DayList: 定时器配置的日期列表
%% @return:: true|false
check_is_triggered(Week, Day, WeekList, DayList) ->
  IfIsWeek = lists:member(Week, WeekList),
  IfIsDay = lists:member(Day, DayList),
  CheckWeek = WeekList =:= [] orelse IfIsWeek,
  CheckDay = DayList =:= [] orelse IfIsDay,
  if
    WeekList == [] andalso DayList == [] -> true;
    WeekList == [] -> CheckDay;
    DayList == [] -> CheckWeek;
    WeekList /= [] -> IfIsWeek orelse CheckDay;
    DayList /= [] -> IfIsDay orelse CheckWeek;
    true -> false
  end.

%% 过了每天的0点，重新检查一次定时器
check_timer_list(_NowTime, _DayTime, _Day, _Week, [], NewTimerList) -> NewTimerList;
check_timer_list(NowTime, DayTime, Day, Week, [TimerInfo|L], NewTimerList) ->
  #timer_info{
    start_hour = StartHour,
    start_minute = StartMinute,
    start_second = StartSecond,
    day_list = DailyList,
    week_list = WeekList,
    next_time = NextTime
  } = TimerInfo,
  NewNextTime = case NextTime of
                  0 ->
                    case check_is_triggered(Week, Day, WeekList, DailyList) of
                      true -> DayTime + StartHour * 3600 + StartMinute * 60 + StartSecond;
                      _ -> 0
                    end;
                  _ -> NextTime
                end,
  NewTimerInfo = TimerInfo#timer_info{next_time = NewNextTime},
  check_timer_list(NowTime, DayTime, Day, Week, L, [NewTimerInfo|NewTimerList]).

%% 定时器触发逻辑
trigger_timer(_NowTime, _DayTime, _Day, _Week, [], NewTimerList) -> NewTimerList;
trigger_timer(NowTime, DayTime, Day, Week, [TimerInfo | Tail], NewTimerList) ->
  #timer_info{
    start_hour = StartHour,
    start_minute = StartMinute,
    start_second = StartSecond,
    end_hour = EndHour,
    end_minute = EndMinute,
    end_second = EndSecond,
    day_list = DailyList,
    week_list = WeekList,
    next_time = NextTime,
    gap = Gap,
    module = Module,
    func = Func,
    args = Args
  } = TimerInfo,
  NewTimerInfo = case check_is_triggered(Week, Day, WeekList, DailyList) of
                   true ->
                     case NowTime >= NextTime of
                       true ->
                         % 触发业务处理
                         spawn(fun() -> apply(Module, Func, Args) end),
                         % 计算时间
                         StartTime = DayTime + StartHour * 3600 + StartMinute * 60 + StartSecond,
                         EndTime = DayTime + EndHour * 3600 + EndMinute * 60 + EndSecond,
                         % 计算下一次执行时间
                         NewNextTime = if
                                         Gap =:= 0 -> % 一天执行一次
                                           StartTime + 86400;
                                         StartTime =:= EndTime -> % 开始和结束时间一样，即每隔多少时间会重新执行
                                           NowTime + Gap;
                                         NowTime + Gap > EndTime -> % 如果下次时间超过结束时间，则第二天执行
                                           StartTime + 86400;
                                         true ->
                                           StartTime + 86400
                                       end,
                         % 更新时间
                         TimerInfo#timer_info{next_time = NewNextTime};
                       _ ->
                         TimerInfo
                     end;
                   _ ->
                     TimerInfo
                 end,
  trigger_timer(NowTime, DayTime, Day, Week, Tail, [NewTimerInfo | NewTimerList]).