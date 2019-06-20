%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 过滤词处理进程
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_filter).
-author("sy").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([update_all_words/0, load_word/1, delete_word/1, load_word_group/0, load_word_group/1, delete_word_group/0,
  delete_word_group/1, match_word_group/1, i/0, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
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
      {error, ?ERR_COMMON_SERVICE_NOT_START}
  end.

%% @doc cast
cast(Info) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(Pid, Info);
    _ ->
      ignore
  end.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:process_flag(trap_exit, true),
  erlang:send_after(1000, self(), init_for_start),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({match_word_group, Talk}, _From, State) ->
  Reply = match_word_group(Talk),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(init_for_start, State) ->
  lib_filter:init(),
  {noreply, State};
handle_cast({delete_word, Word}, State) ->
  lib_filter:delete_word(Word),
  {noreply, State};
handle_cast(update_all_words, State) ->
  lib_filter:update_all(),
  {noreply, State};
handle_cast({load_word, ID}, State) ->
  lib_filter:import_words_by_id(ID),
  {noreply, State};
handle_cast(delete_word_group, State) ->
  lib_filter:delete_word_group(),
  {noreply, State};
handle_cast({delete_word_group, ID}, State) ->
  lib_filter:delete_word_group(ID),
  {noreply, State};
handle_cast(load_word_group, State) ->
  lib_filter:load_word_groups(),
  {noreply, State};
handle_cast({load_word_group, ID}, State) ->
  lib_filter:load_word_group(ID),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
