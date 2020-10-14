%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 9月 2020 11:08
%%%-------------------------------------------------------------------
-module(svr_make_log).
-author("sy").

-behaviour(gen_server).

-include("make_log.hrl").
-include("common.hrl").

%% API
-export([start_link/0, stop/0, write_log/3, p/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(LOOP_WRITE_TIME,    300000).            %% 定时写入日志文件：5min

-record(svr_make_log_state, {loop_time = 0, log_list = []}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, {stop});
    _ ->
      ignore
  end.

write_log(Mod, Content, Time) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, {log, Mod, Content, Time});
    _ ->
      skip
  end.

%% 进程Pid
p() ->
  dist:whereis_name(local, ?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #svr_make_log_state{}} | {ok, State :: #svr_make_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  process_flag(trap_exit, true),
  LogList = init_log_list(),
  erlang:send_after(?LOOP_WRITE_TIME, self(), {loop_write_time}),
  {ok, #svr_make_log_state{loop_time = time:unixtime(), log_list = LogList}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #svr_make_log_state{}) ->
  {reply, Reply :: term(), NewState :: #svr_make_log_state{}} |
  {reply, Reply :: term(), NewState :: #svr_make_log_state{}, timeout() | hibernate} |
  {noreply, NewState :: #svr_make_log_state{}} |
  {noreply, NewState :: #svr_make_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #svr_make_log_state{}} |
  {stop, Reason :: term(), NewState :: #svr_make_log_state{}}).
handle_call(Request, From, State = #svr_make_log_state{}) ->
  try
    do_handle_call(Request, From, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, from ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Request, From, State, Reason, erlang:get_stacktrace()]),
      {reply, {error, 255}, State}
  end.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #svr_make_log_state{}) ->
  {noreply, NewState :: #svr_make_log_state{}} |
  {noreply, NewState :: #svr_make_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #svr_make_log_state{}}).
handle_cast(Request, State = #svr_make_log_state{}) ->
  try
    do_handle_cast(Request, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Request, State, Reason, erlang:get_stacktrace()]),
      {noreply, State}
  end.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #svr_make_log_state{}) ->
  {noreply, NewState :: #svr_make_log_state{}} |
  {noreply, NewState :: #svr_make_log_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #svr_make_log_state{}}).
handle_info(Info, State = #svr_make_log_state{}) ->
  try
    do_handle_info(Info, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, info ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
      {noreply, State}
  end.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #svr_make_log_state{}) -> term()).
terminate(_Reason, _State = #svr_make_log_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #svr_make_log_state{},
    Extra :: term()) ->
  {ok, NewState :: #svr_make_log_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #svr_make_log_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_log_list() ->
  init_log_list_1(?MAKE_LOG_LIST, []).

init_log_list_1([], LogList) ->
  LogList;
init_log_list_1([Mod|T], LogList) ->
  FileName = atom_to_list(Mod) ++ ".txt",
  Info = #svr_log_info{record_mod = Mod,
    file_name = FileName},
  init_log_list_1(T, [Info|LogList]).

%% ======================== do_handle_call ============================
%% 默认匹配
do_handle_call(_R, _FROM, State) ->
  {reply, ok, State}.

%% ======================== do_handle_cast ============================
do_handle_cast({log, Mod, Content, Time}, State) ->
  LogList = State#svr_make_log_state.log_list,
  case lists:keyfind(Mod, #svr_log_info.record_mod, LogList) of
    Info when is_record(Info, svr_log_info) ->
      #svr_log_info{file_content_list = List} = Info,
      NewList = [{Time, Content}|List],
      NewInfo = Info#svr_log_info{file_content_list = NewList},
      NewLogList = lists:keyreplace(Mod, #svr_log_info.record_mod, LogList, NewInfo),
      NewState = State#svr_make_log_state{log_list = NewLogList};
    _ ->
      NewState = State
  end,
  {noreply, NewState};

%% 关闭进程
do_handle_cast({stop}, State) ->
  {{Year, Month, Day}, {_Hour, _Min, _Sec}} = erlang:localtime(),
  NewLogList = loop_write_time(State#svr_make_log_state.log_list, [], Year, Month, Day),
  {stop, normal, State#svr_make_log_state{log_list = NewLogList}};

%% 默认匹配
do_handle_cast(Info, State) ->
  ?ERROR_MSG("svr_make_log do_handle_cast is not match:~w", [Info]),
  {noreply, State}.

%% ======================== do_handle_info ============================
do_handle_info({loop_write_time}, State) ->
  {{Year, Month, Day}, {_Hour, _Min, _Sec}} = erlang:localtime(),
  NewLogList = loop_write_time(State#svr_make_log_state.log_list, [], Year, Month, Day),
  erlang:send_after(?LOOP_WRITE_TIME, self(), {loop_write_time}),
  {noreply, State#svr_make_log_state{log_list = NewLogList}};

do_handle_info(_Info, State) ->
  ?ERROR_MSG("svr_make_log do_handle_info is not match:~w", [_Info]),
  {noreply, State}.

loop_write_time([], LogList, _Year, _Month, _Day) ->
  LogList;
loop_write_time([Info|T], LogList, Year, Month, Day) ->
  #svr_log_info{file_name = FileName,
    file_content_list = FileContentList} = Info,
  if FileContentList == [] ->
    NewInfo = Info;
    true ->
      SortFileContentList = lists:keysort(1, FileContentList),
      ReFileContentList = lists:reverse(SortFileContentList),
      TxtFileContentList = loop_write_time_1(ReFileContentList, []),
      if TxtFileContentList == [] ->
        skip;
        true ->
          Path = "../log/" ++ io_lib:format("~p-~p-~p", [Year, Month, Day]) ++ "/game_log",
          case filelib:ensure_dir(Path) of
            ok ->
              AbsFileName = filename:join(Path, FileName),
              case open_log_file(AbsFileName) of
                {ok, FilePid} ->
                  case is_process_alive(FilePid) of
                    true ->
                      io:put_chars(FilePid, TxtFileContentList);
                    _ ->
                      skip
                  end;
                _ ->
                  skip
              end;
            {error, Reason} ->
              ?ERROR_MSG("Target Directory: ~s Cannot create, Reason: ~p", [Path, Reason]),
              skip
          end
      end,
      NewInfo = Info#svr_log_info{file_content_list = []}
  end,
  loop_write_time(T, [NewInfo|LogList], Year, Month, Day).

loop_write_time_1([], List) ->
  List;
loop_write_time_1([H|T], List) ->
  {_Time, Content} = H,
  NewList = Content ++ List,
  loop_write_time_1(T, NewList).

open_log_file(AbsFileName) ->
  case filelib:ensure_dir(AbsFileName) of
    ok ->
      case filelib:is_file(AbsFileName) of
        true ->
          file:open(AbsFileName, [append, {encoding, utf8}]);
        false ->
          file:open(AbsFileName, [write, {encoding, utf8}])
      end;
    _ ->
      ?ERROR_MSG("create log:~p directory fail!~n", [AbsFileName]),
      false
  end.