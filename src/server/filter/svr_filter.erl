%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 过滤词处理进程
%%% @end
%%% Created : 14. 10月 2020 16:22
%%%-------------------------------------------------------------------
-module(svr_filter).
-author("sy").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/0, update_all_words/0, load_word/1, delete_word/1,
  load_word_group/0, load_word_group/1, delete_word_group/0, delete_word_group/1,
  match_word_group/1, i/0, p/0, call/1, cast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(svr_filter_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 更新所有词库
update_all_words() ->
  cast(update_all_words).

%% @doc 加载单个词
load_word(ID) ->
  cast({load_word, ID}).

%% @doc 删除一个词
delete_word(Word) ->
  cast({delete_word, Word}).

%% @doc 加载所有词组
load_word_group() ->
  cast(load_word_group).

%% @doc 加载单个词组
load_word_group(ID) ->
  cast({load_word_group, ID}).

%% @doc 删除所有过滤词组
delete_word_group() ->
  cast(delete_word_group).

%% @doc 删除过滤词组
delete_word_group(ID) ->
  cast({delete_word_group, ID}).

%% @doc 是否匹配过滤词组
match_word_group(Talk) ->
  call({match_word_group, Talk}).

%% @doc 进程信息
i() ->
  call({info}).

%% @doc 进程ID
p() ->
  dist:whereis_name(local, ?MODULE).

%% @doc call
call(Info) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:call(Pid, Info);
    _ ->
      {error, err_common_service_not_start}
  end.

%% @doc cast
cast(Info) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, Info);
    _ ->
      ignore
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #svr_filter_state{}} | {ok, State :: #svr_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:process_flag(trap_exit, true),
  erlang:send_after(1000, self(), init_for_start),
  {ok, #svr_filter_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #svr_filter_state{}) ->
  {reply, Reply :: term(), NewState :: #svr_filter_state{}} |
  {reply, Reply :: term(), NewState :: #svr_filter_state{}, timeout() | hibernate} |
  {noreply, NewState :: #svr_filter_state{}} |
  {noreply, NewState :: #svr_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #svr_filter_state{}} |
  {stop, Reason :: term(), NewState :: #svr_filter_state{}}).
handle_call(Request, From, State = #svr_filter_state{}) ->
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
-spec(handle_cast(Request :: term(), State :: #svr_filter_state{}) ->
  {noreply, NewState :: #svr_filter_state{}} |
  {noreply, NewState :: #svr_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #svr_filter_state{}}).
handle_cast(Request, State = #svr_filter_state{}) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #svr_filter_state{}) ->
  {noreply, NewState :: #svr_filter_state{}} |
  {noreply, NewState :: #svr_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #svr_filter_state{}}).
handle_info(Info, State = #svr_filter_state{}) ->
  try
    do_handle_cast(Info, State)
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
    State :: #svr_filter_state{}) -> term()).
terminate(_Reason, _State = #svr_filter_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #svr_filter_state{},
    Extra :: term()) ->
  {ok, NewState :: #svr_filter_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #svr_filter_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_handle_call({match_word_group, Talk}, _From, State) ->
  Reply = match_word_group(Talk),
  {reply, Reply, State};
do_handle_call(_Request, _From, State) ->
  {reply, ok, State}.


do_handle_cast(init_for_start, State) ->
  lib_filter:init(),
  {noreply, State};
do_handle_cast({delete_word, Word}, State) ->
  lib_filter:delete_word(Word),
  {noreply, State};
do_handle_cast(update_all_words, State) ->
  lib_filter:update_all(),
  {noreply, State};
do_handle_cast({load_word, ID}, State) ->
  lib_filter:import_words_by_id(ID),
  {noreply, State};
do_handle_cast(delete_word_group, State) ->
  lib_filter:delete_word_group(),
  {noreply, State};
do_handle_cast({delete_word_group, ID}, State) ->
  lib_filter:delete_word_group(ID),
  {noreply, State};
do_handle_cast(load_word_group, State) ->
  lib_filter:load_word_groups(),
  {noreply, State};
do_handle_cast({load_word_group, ID}, State) ->
  lib_filter:load_word_group(ID),
  {noreply, State};
do_handle_cast(_Request, State) ->
  {noreply, State}.