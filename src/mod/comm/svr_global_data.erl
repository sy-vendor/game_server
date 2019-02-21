%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 全局数据
%%% @end
%%% Created : 19. 十二月 2018 14:44
%%%-------------------------------------------------------------------
-module(svr_global_data).
-author("suyang").

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

-export([stop/0,
    get_value/3,
    get_all_value/1,
    put_value/3,
    add_count/3,
    append_value/4,
    daily_clear/0,
    i/0,
    p/0,
    cast/1,
    call/1]).

-define(SERVER, ?MODULE).

%% 进程状态数据
-record(state, {
    permanent_data = dict:new(),  % 永久数据
    daily_data = dict:new()       % 每日数据
}).

-include("global.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 关闭进程
stop() ->
    cast({stop}).

%% @获取数据
%% @param: DailyFlag 0永久数据|1每日数据
get_value(DailyFlag, Key, Default) ->
    call({get_value, [DailyFlag, Key, Default]}).

%% @doc 获取所有数据
get_all_value(DailyFlag) ->
    call({get_all_value, [DailyFlag]}).

%% @doc 设置数据
put_value(DailyFlag, Key, Value) ->
    cast({put_value, [DailyFlag, Key, Value]}).

%% @doc 增加值
add_count(DailyFlag, Key, AddNum) ->
    cast({add_count, [DailyFlag, Key, AddNum]}).

%% @doc 列表append数据
append_value(DailyFlag, Key, Value, MaxLen) ->
    cast({append_value, [DailyFlag, Key, Value, MaxLen]}).

%% @doc 日清数据处理
daily_clear() ->
    cast({daily_clear}).

%% @doc 进程信息
i() ->
    call({info}).

%% @doc 进程ID
p() ->
    util_dist:whereis_name(global, ?MODULE).

%% @doc call函数
call(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, server_not_start}
    end.

%% @doc cast函数
cast(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
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
    process_flag(trap_exit, true),
    State = load_data(),
    {ok, State}.

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
%% @doc 获取永久数据
handle_call({get_value, [?PERMANENT_DATA, Key, Default]}, _FROM, State) ->
    #state{permanent_data = PerData} = State,
    ResData = get_value_from_dict(PerData, Key, Default),
    {reply, ResData, State};
%% @doc 获取日常数据
handle_call({get_value, [?DAILY_DATA, Key, Default]}, _FROM, State) ->
    #state{daily_data = DailyData} = State,
    ResData = get_value_from_dict(DailyData, Key, Default),
    {reply, ResData, State};
%% @doc 获取所有永久数据
handle_call({get_all_value, [?PERMANENT_DATA]}, _FROM, State) ->
    #state{permanent_data = PerData} = State,
    ResList = dict:to_list(PerData),
    {reply, ResList, State};
%% @doc 获取所有日常数据
handle_call({get_all_value, [?DAILY_DATA]}, _FROM, State) ->
    #state{daily_data = DailyData} = State,
    ResList = dict:to_list(DailyData),
    {reply, ResList, State};
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
%% 设置永久数据
handle_cast({put_value, [?PERMANENT_DATA, Key, Value]}, State) ->
    #state{permanent_data = PerData} = State,
    NewPerData = put_value_to_dict(PerData, Key, Value, ?PERMANENT_DATA),
    NewState = State#state{permanent_data = NewPerData},
    {noreply, NewState};
%% 设置日常数据
handle_cast({put_value, [?DAILY_DATA, Key, Value]}, State) ->
    #state{daily_data = DailyData} = State,
    NewDailyData = put_value_to_dict(DailyData, Key, Value, ?DAILY_DATA),
    NewState = State#state{daily_data = NewDailyData},
    {noreply, NewState};
%% 永久数据增加值
%% @param:: AddNum 增加值|integer
handle_cast({add_count, [?PERMANENT_DATA, Key, AddNum]}, State) when is_integer(AddNum) ->
    #state{permanent_data = PerData} = State,
    NewPerData = add_count_to_dict(PerData, Key, AddNum, ?PERMANENT_DATA),
    NewState = State#state{permanent_data = NewPerData},
    {noreply, NewState};
%% 日常数据增加值
%% @param:: AddNum 增加值|integer
handle_cast({add_count, [?DAILY_DATA, Key, AddNum]}, State) when is_integer(AddNum) ->
    #state{daily_data = DailyData} = State,
    NewDailyData = add_count_to_dict(DailyData, Key, AddNum, ?DAILY_DATA),
    NewState = State#state{daily_data = NewDailyData},
    {noreply, NewState};
%% 永久数据列表增加值
handle_cast({append_value, [?PERMANENT_DATA, Key, Value, MaxLen]}, State) ->
    #state{permanent_data = PerData} = State,
    NewPerData = append_value_to_dict(PerData, Key, Value, MaxLen, ?PERMANENT_DATA),
    NewState = State#state{permanent_data = NewPerData},
    {noreply, NewState};
%% 日常数据列表增加值
handle_cast({append_value, [?DAILY_DATA, Key, Value, MaxLen]}, State) ->
    #state{daily_data = DailyData} = State,
    NewDailyData = append_value_to_dict(DailyData, Key, Value, MaxLen, ?DAILY_DATA),
    NewState = State#state{daily_data = NewDailyData},
    {noreply, NewState};
%% 日清数据处理
handle_cast({daily_clear}, State) ->
    catch daily_clear_db(),
    NewState = State#state{daily_data = dict:new()},
    {noreply, NewState};
%% 关闭进程
handle_cast({stop}, State) ->
    {stop, normal, State};
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
%% @doc 加载数据
-define(SQL_GLOBAL_DATA_GET, <<"select `key`, `value`, `daily_flag` from `global_data`">>).
load_data() ->
    Sql = io_lib:format(?SQL_GLOBAL_DATA_GET, []),
    case db:get_all(Sql) of
        {ok, Data} ->
            % 分开永久数据和每日数据
            {PerDict, DailyDict} = split_data(Data),
            #state{permanent_data = PerDict, daily_data = DailyDict};
        _ ->
            #state{permanent_data = dict:new(), daily_data = dict:new()}
    end.

%% @doc 分开永久数据和每日数据
split_data(Data) ->
    split_data2(Data, dict:new(), dict:new()).
split_data2([], PerDict, DailyDict) -> {PerDict, DailyDict};
split_data2([[KeyBin, ValueBin, ?PERMANENT_DATA]|L], PerDict, DailyDict) ->
    Key = util_type:bitstring_to_term(KeyBin),
    Value = util_type:bitstring_to_term(ValueBin),
    NewPerDict = dict:store(Key, Value, PerDict),
    split_data2(L, NewPerDict, DailyDict);
split_data2([[KeyBin, ValueBin, ?DAILY_DATA]|L], PerDict, DailyDict) ->
    Key = util_type:bitstring_to_term(KeyBin),
    Value = util_type:bitstring_to_term(ValueBin),
    NewDailyDict = dict:store(Key, Value, DailyDict),
    split_data2(L, PerDict, NewDailyDict);
split_data2([_|L], PerDict, DailyDict) ->
    split_data2(L, PerDict, DailyDict).

%% @doc 获取数据
get_value_from_dict(Dict, Key, Default) ->
    case dict:find(Key, Dict) of
        error -> Default;
        {ok, Value} -> Value
    end.

%% @doc 设置数据
put_value_to_dict(Dict, Key, Value, DailyFlag) ->
    update_db(Key, Value, DailyFlag),
    dict:store(Key, Value, Dict).

%% @doc 增加计数
add_count_to_dict(Dict, Key, AddNum, DailyFlag) ->
    case get_value_from_dict(Dict, Key, 0) of
        Value when is_integer(Value) ->
            NewVal = Value + AddNum,
            update_db(Key, NewVal, DailyFlag),
            dict:store(Key, NewVal, Dict);
        _ -> Dict
    end.

%% @doc 列表append数据
append_value_to_dict(Dict, Key, Value, MaxLen, DailyFlag) ->
    case get_value_from_dict(Dict, Key, []) of
        OldValue when is_list(OldValue) ->
            NewValue = case MaxLen =:= 0 of
                           true -> [Value|OldValue];
                           false -> lists:sublist([Value|OldValue], MaxLen)
                       end,
            update_db(Key, NewValue, DailyFlag),
            dict:store(Key, NewValue, Dict);
        _ ->
            update_db(Key, [Value], DailyFlag),
            dict:store(Key, [Value], Dict)
    end.

%% @doc 数据持久化
-define(SQL_GLOBAL_DATA_INSERT, <<"replace into `global_data` (`key`, `value`, `daily_flag`) values ('~s', '~s', ~p)">>).
update_db(Key, Value, DailyFlag)->
    KeyBin = util_type:term_to_bitstring(Key),
    ValueBin = util_type:term_to_bitstring(Value),
    Sql = io_lib:format(?SQL_GLOBAL_DATA_INSERT, [KeyBin, ValueBin, DailyFlag]),
    db:execute(Sql).

%% 日清数据处理
-define(SQL_GLOBAL_DAILY_DATA_DELETE, <<"delete from `global_data` where `daily_flag`=1">>).
daily_clear_db() ->
    Sql = io_lib:format(?SQL_GLOBAL_DAILY_DATA_DELETE, []),
    db:execute(Sql).
