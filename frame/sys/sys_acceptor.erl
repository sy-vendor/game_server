%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% TCP Acceptor
%%% @end
%%% Created : 07. 十二月 2018 13:09
%%%-------------------------------------------------------------------
-module(sys_acceptor).
-author("suyang").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).
-include("common.hrl").
-include("conn.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [LSock], []).

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
init([LSock]) ->
    self() ! loop,
    {ok, {LSock}}.

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
handle_info(loop, State = {LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end)),
            self() ! loop, %% 继续等待下一个
            {noreply, State};
        {error, Reason} ->
            ?ERROR_MSG("accept socket unknow err:~w", [Reason]),
            {noreply, State}
    end;
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
%% 接受一个连接
accept(Socket) ->
    case gen_tcp:recv(Socket, 0, 10000) of
        {ok, Data} ->
            HeadInfo = erlang:binary_to_list(Data),
            HeaderList = string:tokens(HeadInfo, "\r\n"),
            HeaderTupleList = [list_to_tuple(string:tokens(Header, ": ")) || Header <- HeaderList],
            SecWebSocketKey = proplists:get_value("Sec-WebSocket-Key", HeaderTupleList),
            ServerKey = sys_env:get(server_key),
            Sha = crypto:hash(sha, [list_to_binary(SecWebSocketKey), util_type:to_binary(ServerKey)]),
            Base64 = base64:encode(Sha),
            HandshakeHeader = [
                <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                <<"Upgrade: websocket\r\n">>,
                <<"Connection: Upgrade\r\n">>,
                <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                <<"\r\n">>
            ],
            ok = gen_tcp:send(Socket, HandshakeHeader),
            create(?CLIENT_GAME, Socket);
        Else ->
            Peers = inet:peernames(Socket),
            Sock = inet:socknames(Socket),
            ?ERROR_MSG("handle fail two:~w,~w,~w", [Peers, Sock, Else]),
            gen_tcp:close(Socket)
    end.


create(?CLIENT_GAME, Socket) -> create_conn(Socket);
create(_Data, Socket) ->
    Peers = inet:peernames(Socket),
    Sock = inet:socknames(Socket),
    ?ERROR_MSG("Handshake fail:~w,~w", [Peers, Sock]),
    gen_tcp:close(Socket).


%% 创建连接进程
create_conn(Socket) ->
    try
        {ok, {Ip, Port}} = inet:peername(Socket),
        {ok, Pid} = sys_conn:create(Socket, Ip, Port),
        ok = gen_tcp:controlling_process(Socket, Pid)
    catch
        T:X ->
            ?ERROR_MSG("handle fail[~w : ~w](~w)", [T, X, erlang:get_stacktrace()]),
            gen_tcp:close(Socket)
    end.
