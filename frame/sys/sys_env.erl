%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 系统环境变量管理器
%%% @end
%%% Created : 07. 十二月 2018 11:34
%%%-------------------------------------------------------------------
-module(sys_env).
-author("suyang").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([get/1,
    get/2,
    set/2,
    save/2,
    del/1
]).

-define(SERVER, ?MODULE).
-include("common.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%% @spec get(Key) -> term()
%% Key = atom()
%% @doc 取出相应设置
get(Key) when is_atom(Key) ->
    case ets:lookup(sys_env, Key) of
        [{_Key, Val}] -> Val;
        _ ->
            undefined
    end.

get(Key, Def) when is_atom(Key) ->
    case ets:lookup(sys_env, Key) of
        [{_Key, Val}] -> Val;
        _ -> Def
    end.

%% @spec set(Key, Val) -> ok
%% Key = atom()
%% Val = term()
%% @doc 动态修改设置
set(Key, Val) when is_atom(Key) ->
    ets:insert(sys_env, {Key, Val}).

%% @spec save(Key, Val) -> ok | {error, Err}
%% Key = atom()
%% Val = term()
%% Err = term()
%% @doc 设置并保存指定key的值到数据库
save(Key, Val) when is_atom(Key) ->
    set(Key, Val),
    dets:insert(sys_env, {Key, Val}).

%% @spec del(Key) -> ok | {error, Err}
%% Key = atom()
%% Err = term()
%% @doc 删除一个键值对
del(Key) when is_atom(Key) ->
    ets:delete(sys_env, Key),
    dets:delete(sys_env, Key).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(SupName, NodeType) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SupName, NodeType], []).

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
init([SupName, NodeType]) ->
    process_flag(trap_exit, true),
    ets:new(sys_env, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    dets:open_file(sys_env, [{file, "../log/env/sys_env.dets"}, {keypos, 1}, {type, set}]),
    dets:to_ets(sys_env, sys_env),
    set(sup_name, SupName),
    set(node_type, NodeType),
    load_app_config(main:get_all_app()),
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
    dets:close(sys_env),
    ?INFO("sys_env close"),
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
%% @doc 加载application的配置
load_app_config([]) ->
    ok;
load_app_config([H | T]) ->
    case application:get_all_env(H) of
        Kvs when is_list(Kvs) ->
            do_load(Kvs),
            load_app_config(T);
        _ ->
            ?ERROR_MSG(" loadding application config err: ~w", [H]),
            load_app_config(T)
    end.

do_load([]) ->
    ok;
do_load([{K, V} | T]) ->
    ets:insert(sys_env, {K, V}),
    do_load(T);
do_load([H | T]) ->
    ?ERROR_MSG("err config: ~w", [H]),
    do_load(T).