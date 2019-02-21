%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 签到
%%% @end
%%% Created : 19. 十二月 2018 10:29
%%%-------------------------------------------------------------------
-module(lib_sign_api).
-author("suyang").

%% API
-export([init/3,
    save/2,
    sign_reward/2,
    send_sign_info/1,
    daily_reset/0]).

-include("role.hrl").
-include("sign.hrl").
-include("common.hrl").
-include("err_code.hrl").
-include("logic.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 登陆初始化
init(_NowTime, _LastLogOutTime, Role) ->
    Sql = io_lib:format(?SQL_ROLE_SIGN_FETCH, [Role#role.role_id]),
    case db:get_all(Sql) of
        {ok, [[IsSign, TotalSignCnt, SignTime]]} ->
            RoleSign = #role_sign{is_sign = IsSign, total_sign_cnt = TotalSignCnt, sign_time = SignTime},
            put(?DICT_ROLE_SIGN, RoleSign),
            % 重置数据
            daily_reset();
        _ ->
            put(?DICT_ROLE_SIGN, #role_sign{})
    end.

%% @doc 下线保存
save(_NowTime, Role) ->
    RoleSign = get(?DICT_ROLE_SIGN),
    case RoleSign of
        #role_sign{is_sign = IsSign, total_sign_cnt = TotalSignCnt, sign_time = SignTime, sign = Sign} when Sign =:= true ->
            DbRoleSign = [[Role#role.role_id, IsSign, TotalSignCnt, SignTime]],
            util:insert_values(?SQL_ROLE_SIGN_REPLACE_P1, ?SQL_ROLE_SIGN_REPLACE_P2, DbRoleSign),
            RoleSignN = RoleSign#role_sign{sign = false},
            put(?DICT_ROLE_SIGN, RoleSignN);
        _ -> ignore
    end.

%% @doc 在线跨天
daily_reset() ->
    RoleSign = get(?DICT_ROLE_SIGN),
    Now = util_time:unixtime(),
    case RoleSign of
        undefined ->
            ignore;
        #role_sign{total_sign_cnt = TotalSignCnt, sign_time = SignTime, is_sign = IsSign} when TotalSignCnt >= 7 ->
            IsSignN = ?IF(util_time:is_same_day(Now, SignTime), IsSign, ?SIGN_FALSE),
            RoleSignN = RoleSign#role_sign{is_sign = IsSignN, total_sign_cnt = 0, sign = true},
            put(?DICT_ROLE_SIGN, RoleSignN);
        #role_sign{sign_time = SignTime, is_sign = IsSign} ->
            IsSignN = ?IF(util_time:is_same_day(Now, SignTime), IsSign, ?SIGN_FALSE),
            RoleSignN = RoleSign#role_sign{is_sign = IsSignN, sign = true},
            put(?DICT_ROLE_SIGN, RoleSignN)
    end.

%% @doc 签到领取
sign_reward(Type, Role) ->
    case catch check_sign_reward(Type) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11204, #'SignRewardRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, Reward, RoleSign, TotalSignCnt} ->
            % 数据变化
            RoleN = lib_role_asset:add_assets(Reward, Role),
            RoleSignN = RoleSign#role_sign{is_sign = ?SIGN_TRUE, sign_time = util_time:unixtime(), total_sign_cnt = TotalSignCnt + 1, sign = true},
            put(?DICT_ROLE_SIGN, RoleSignN),
            {ok, Bin} = lib_proto:pack(11204, #'SignRewardRes'{code = ?SUCCESS}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 推送签到信息
send_sign_info(Role) ->
    RoleSign = get(?DICT_ROLE_SIGN),
    RoleSignN = ?IF(is_record(RoleSign, role_sign), RoleSign, #role_sign{}),
    #role_sign{total_sign_cnt = TotalSignCnt, is_sign = IsSign} = RoleSignN,
    Args = ?IF(IsSign =:= ?SIGN_TRUE, true, false),
    {ok, Bin} = lib_proto:pack(11202, #'SignInfoRes'{totalSignCnt = TotalSignCnt, isSign = Args}),
    lib_send:send_to_role(Role, Bin),
    {ok}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 签到领奖检查
check_sign_reward(Type) ->
    RoleSign = get(?DICT_ROLE_SIGN),
    % 判断系统是否加载成功
    ?IF(is_record(RoleSign, role_sign), ignore, erlang:throw({error, ?ERR_COMMON_SYS})),
    #role_sign{is_sign = IsSign, total_sign_cnt = TotalSignCnt} = RoleSign,
    % 判断今日是否签到
    ?IF(IsSign =:= ?SIGN_TRUE, erlang:throw({error, ?ERR_COMMON_ALREADY_SIGN}), ignore),
    Reward = conf_sign:get_sign_reward(TotalSignCnt + 1),
    ?IF(Reward =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    [NormalReward, LuxuryReward] = Reward,
    RewardList = ?IF(Type =:= ?SIGN_TYPE_LUXURY, [{?TYPE_DIAMOND, LuxuryReward}], [{?TYPE_DIAMOND, NormalReward}]),
    {true, RewardList, RoleSign, TotalSignCnt}.