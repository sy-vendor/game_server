-module(game_server_auth).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    generate_token/1,
    validate_token/1,
    check_permission/2,
    revoke_token/1
]).

-record(token, {
    id,
    user_id,
    permissions = [],
    created_at,
    expires_at
}).

-record(state, {
    tokens = #{},
    rate_limits = #{}
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate_token(UserId) ->
    gen_server:call(?MODULE, {generate_token, UserId}).

validate_token(Token) ->
    gen_server:call(?MODULE, {validate_token, Token}).

check_permission(Token, Permission) ->
    gen_server:call(?MODULE, {check_permission, Token, Permission}).

revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}).

%% Callback Functions
init([]) ->
    lager:info("Auth system initialized"),
    {ok, #state{}}.

handle_call({generate_token, UserId}, _From, State) ->
    TokenId = game_server_util:generate_id(),
    Now = game_server_util:timestamp(),
    ExpiresAt = Now + 24 * 60 * 60 * 1000, % 24 hours
    
    Token = #token{
        id = TokenId,
        user_id = UserId,
        permissions = [<<"game:play">>],
        created_at = Now,
        expires_at = ExpiresAt
    },
    
    NewTokens = maps:put(TokenId, Token, State#state.tokens),
    lager:info("Token generated for user ~p", [UserId]),
    
    {reply, {ok, TokenId}, State#state{tokens = NewTokens}};

handle_call({validate_token, TokenId}, _From, State) ->
    case maps:find(TokenId, State#state.tokens) of
        {ok, Token} ->
            Now = game_server_util:timestamp(),
            case Token#token.expires_at > Now of
                true ->
                    {reply, {ok, Token}, State};
                false ->
                    NewTokens = maps:remove(TokenId, State#state.tokens),
                    {reply, {error, token_expired}, State#state{tokens = NewTokens}}
            end;
        error ->
            {reply, {error, invalid_token}, State}
    end;

handle_call({check_permission, TokenId, Permission}, _From, State) ->
    case maps:find(TokenId, State#state.tokens) of
        {ok, Token} ->
            case lists:member(Permission, Token#token.permissions) of
                true ->
                    {reply, {ok, Token#token.user_id}, State};
                false ->
                    {reply, {error, permission_denied}, State}
            end;
        error ->
            {reply, {error, invalid_token}, State}
    end;

handle_call({revoke_token, TokenId}, _From, State) ->
    case maps:find(TokenId, State#state.tokens) of
        {ok, _Token} ->
            NewTokens = maps:remove(TokenId, State#state.tokens),
            lager:info("Token revoked: ~p", [TokenId]),
            {reply, ok, State#state{tokens = NewTokens}};
        error ->
            {reply, {error, token_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Auth system terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 