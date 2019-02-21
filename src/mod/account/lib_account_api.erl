%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 微信相关
%%% @end
%%% Created : 09. 一月 2019 20:58
%%%-------------------------------------------------------------------
-module(lib_account_api).
-author("suyang").

%% API
-export([wechat_login/2,
    update_firend_data/1]).

-include("common.hrl").
-include("logic.hrl").

-define(RES_SYS_BUSY, 1).  %% 系统繁忙，此时请开发者稍候再试
-define(RES_HTTP_SUC, 0).  %% 请求成功
-define(RES_CODE_INVALID, 40029).  %% code 无效
-define(RES_FREQUENCY_LIMIT, 45011).  %% 频率限制，每个用户每分钟100次

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 微信登陆检查
wechat_login(WxCode, Socket) ->
    {ErrCode, OpenId} = get_player_info(WxCode),
    {ok, Bin} = lib_proto:pack(11802, #'WechatLoginRes'{openId = OpenId, code = ErrCode}),
    lib_send:send_to_socket(Socket, Bin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 微信登陆
get_player_info(WxCode) ->
    case request_get("https://api.weixin.qq.com/sns/jscode2session", [{appid, ?APP_ID}, {secret, ?APP_SECRET}, {js_code, util_type:to_list(WxCode)}, {grant_type, ?GRANT_TYPE}]) of
        {ok, Reply} ->
            case json:decode(Reply) of
                {struct, List} ->
                    case lists:keyfind(<<"errcode">>, 1, List) of
                        false ->
                            case lists:keyfind(<<"session_key">>, 1, List) of
                                {_, SessionKey} -> put(session_key, util_type:to_list(SessionKey));
                                _ -> skip
                            end,
                            case lists:keyfind(<<"openid">>, 1, List) of
                                {_, OpenId} ->
                                    put(openid, util_type:to_list(OpenId)),
                                    {?RES_HTTP_SUC, util_type:to_list(OpenId)};
                                _ -> {?RES_SYS_BUSY, []}
                            end;
                        {_, Val} -> {Val, []}
                    end;
                _R -> {?RES_SYS_BUSY, []}
            end;
        _R -> {?RES_SYS_BUSY, []}
    end.

%% @doc 请求网页数据
request_get(Url, Arguments) ->
    case do_request_get(Url, Arguments) of
        {ok, {_, _, Rs}} ->
            {ok, Rs};
        _Error ->
            ?ERROR_MSG("=========WeChat App=======_Error:~w", [_Error]),
            {error, false}
    end.
%% @spec request_get(Url,Arguments) -> {ok,{_,_,Rs}} | {error,Error}
do_request_get(Url, Arguments) ->
    Url2 =
        case Arguments of
            [] ->
                Url;
            _ ->
                Arguments2 = lists:map(fun({K, V}) -> lists:concat([K, "=", V]) end, Arguments),
                Arguments3 = util_type:list_to_string(Arguments2, [], "&", []),
                case lists:member($?, Url) of
                    true ->
                        Url ++ "&" ++ Arguments3;
                    false ->
                        Url ++ "?" ++ Arguments3
                end
        end,
    httpc:request(get, {Url2, []}, [{timeout, 3000}], []).

%% @doc 数据上传
update_firend_data(_List) ->
    SessionKey = get(session_key),
    OpenId = get(openid),
    AccessToken = get_access_token(),
    Data = lists:concat(['{"starNum:"',300, '"}']),
    SignaTureN = hmac_sha(util_type:to_list(SessionKey), Data),
    PostDataFc = util:fbin("access_token=~s&signature=~s&openid=~s&sig_method=~s", [AccessToken, util_type:to_list(SignaTureN), OpenId, "hmac_sha256"]),
    case httpc:request(post, {"https://api.weixin.qq.com/wxa/set_user_storage", [], "application/x-www-form-urlencoded", PostDataFc}, [], []) of
        {ok, {_, _, _Body}} -> ok;
        {error, Reason} -> ?ERROR_MSG("update firend data is err Reason:~w", [Reason])
    end.

get_access_token() ->
    case request_get("https://api.weixin.qq.com/cgi-bin/token", [{grant_type, "client_credential"}, {appid, ?APP_ID}, {secret, ?APP_SECRET}]) of
        {ok, Reply} ->
            case json:decode(Reply) of
                {struct, List} ->
                    case lists:keyfind(<<"access_token">>, 1, List) of
                        false -> [];
                        {_, Val} -> util_type:to_list(Val)
                    end;
                _ -> []
            end;
        _R -> []
    end.

hmac_sha(SessionKey, Data) ->
    <<X:256/big-unsigned-integer>> = crypto:hmac(sha256, SessionKey, Data),
    lists:flatten(io_lib:format("~64.16.0b", [X])).


