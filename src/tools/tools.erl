%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 性能监控工具
%%% @end
%%% Created : 30. 9月 2019 16:31
%%%-------------------------------------------------------------------

-module(tools).

-export([
    eprof_all/1,
    eprof/2,
    task_tc/1,
	get_dic/2,
    timertc/3          % 执行时间测试
]).


eprof_all(TimeoutSec) ->
    eprof(processes() -- [whereis(eprof)], TimeoutSec).

eprof(Pids, TimeoutSec) ->
    eprof:start(),
    eprof:start_profiling(Pids),
    timer:sleep(TimeoutSec),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().

%% -----------------------------
%% 执行时间统计
%% -----------------------------
%% @spec 执行时间统计
%% ProNum:      开启进程数
%% ExeNum:      每个进程执行次数
%% Fun:         执行函数
%% @return:     执行时间（us）
%% @end
timertc(ProNum, ExeNum, Fun) ->
    Parent = self(),
    lists:sum([receive {Pid, Res} -> Res end || Pid <- [spawn(fun()-> 
            ExeL__ = lists:seq(1, ExeNum),
            {BA,BB,BC} = os:timestamp(),
            Begin = 1000000000000*BA+1000000*BB+BC,
            [Fun() || _Exe <- ExeL__],
            {EA,EB,EC} = os:timestamp(),
            End = 1000000000000*EA+1000000*EB+EC,
            Us = End-Begin,
            Parent ! {self(), Us},
            ok
    end) || _ProL <- lists:seq(1, ProNum)]]).


%% =================================================================================================
%% 测试函数
%% =================================================================================================
%% 测试type:term_to_bitstring 和 erlang:term_to_binary 的性能。结果erlang:term_to_binary 要快20倍左右
% test_term_to_binary(N,M) ->
%     D = [{shfkshfskhf,asdfhjsfj,asdfkhaslfj,sdfkhjasklfj,sflhjaslfkj,lkhasfkljs,ksdhflhjasf,sjdflj} || _<-lists:seq(1, 500)],
%     D1 = type:term_to_bitstring(D),
%     D2 = erlang:term_to_binary(D),
%     F11 = fun() -> type:term_to_bitstring(D) end,
%     F12 = fun() -> type:bitstring_to_term(D1) end,
%     A1 = timertc(N,M, F11),
%     A2 = timertc(N,M, F12),
%     io:format("a ~p ~p~n", [A1, A2]),
%     F21 = fun() -> erlang:term_to_binary(D) end,
%     F22 = fun() -> erlang:binary_to_term(D2) end,
%     B1 = timertc(N,M, F21),
%     B2 = timertc(N,M, F22),
%     io:format("b ~p ~p~n", [B1, B2]),
%     ok.

% task_tc(Msg, M, F, A) ->
%     ListMsg = tuple_to_list(Msg), 
%     [AtomMsg | _] = ListMsg, 
%     statistics(runtime),
%     statistics(wall_clock),
%     Value = apply(M, F, A),
%     {_, Time1} = statistics(runtime),
%     {_, Time2} = statistics(wall_clock),
%     time_test("Msg:~p, ~p:~p, Time1:~p, Time2:~p", [AtomMsg, M, F, Time1, Time2]),
%     Value.

task_tc(Msg) ->
    ListMsg = tuple_to_list(Msg), 
    [AtomMsg | _] = ListMsg, 
    ProInfo = erlang:process_info(self(), total_heap_size),
    NowMs = time:unixtime_ms(),
    LastMs = 
    case get(last_ms) of 
        undefined -> NowMs;
        LM -> LM 
    end,
    put(last_ms, NowMs),
    DiffMs = NowMs - LastMs,
    LastT =
    case get(last_time) of 
        undefined -> 0;
        LT -> LT 
    end,
    Ms = DiffMs + LastT,
    put(last_time, Ms),
    time_test("Msg:~p, Ms:~p, ProInfo:~p", [AtomMsg, Ms, ProInfo]).

%% 日志记录函数
time_test(Format, Args) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    File = 
        case get("time_test") of
            % 首次打开
            undefined ->
                FileName = io_lib:format("time_test_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = config:get_log_path() ++  "/" ++ type:unicode_string(FileName),
                {ok, File1} = file:open(PathName, [write, append]),
                put("time_test", {File1, {Year, Month, Day}}),
                File1;
            % 已经打开
            {File2, {Year, Month, Day}} ->
                File2;
            % 需要切换
            {File2, _} ->
                file:close(File2),
                FileName = io_lib:format("time_test_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = config:get_log_path() ++  "/" ++ type:unicode_string(FileName),
                {ok, File3} = file:open(PathName, [write, append]),
                put("time_test", {File3, {Year, Month, Day}}),
                File3
        end,
    %Format1 = unicode:characters_to_list("#error" ++ " ~p-~p-~p ~p:~p:~p \r\n" ++ Format ++ "\r\n~n"),
    Format1 = type:unicode_string("#error" ++ " ~p-~p-~p ~p:~p:~p \r\n" ++ Format ++ "\r\n~n"),
    io:format(File, Format1, [Year, Month, Day, Hour, Min, Sec] ++ Args).

get_dic(Dic, Default) ->
	case get(Dic) of
		undefined ->
			Default;
		Info ->
			Info
	end.

















