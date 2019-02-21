%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 邀请好友相关
%%% @end
%%% Created : 19. 十二月 2018 15:28
%%%-------------------------------------------------------------------
-module(lib_invite_api).
-author("suyang").

%% API
-export([init/3, save/2, b_invite_handle/4, invite_reward/2, send_invite_info/1]).

-export([invite_handle_online/3]).

-include("role.hrl").
-include("common.hrl").
-include("err_code.hrl").
-include("invite.hrl").
-include("logic.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 初始化邀请玩家数据
init(_NowTime, _LastLogOutTime, Role) ->
    #role{role_id = RoleId} = Role,
    F = fun([BInviteId, BInviteAccount, BInviteUrl, Sort, IsReward]) ->
        #role_invite{b_invite_id = BInviteId, b_invite_account = BInviteAccount, b_invite_url = BInviteUrl, sort = Sort, is_reward = IsReward} end,
    Sql = io_lib:format(?SQL_ROLE_INVITE_FETCH, [RoleId]),
    InviteList = case db:get_all(Sql) of
                     {ok, List} -> lists:map(F, List);
                     _ -> []
                 end,
    % 保存数据
    put(?DICT_ROLE_INVITE, InviteList).

%% @doc 下线保存邀请信息
save(_NowTime, Role) ->
    #role{role_id = RoleId} = Role,
    InviteList = get(?DICT_ROLE_INVITE),
    case InviteList of
        [] -> ignore;
        _ ->
            F = fun(RoleInvite, {InviteListAcc, DbInviteListAcc}) ->
                #role_invite{b_invite_id = BInviteId, b_invite_account = BInviteAccount, b_invite_url = BInviteUrl, sort = Sort, sign = Sign, is_reward = IsReward} = RoleInvite,
                case Sign of
                    false -> {[RoleInvite | InviteListAcc], DbInviteListAcc};
                    true ->
                        DbInvite = [RoleId, BInviteId, util_type:to_binary(BInviteAccount), util_type:to_binary(BInviteUrl), Sort, IsReward],
                        {[RoleInvite#role_invite{sign = false} | InviteListAcc], [DbInvite | DbInviteListAcc]}
                end
                end,
            {InviteListAccN, DbInviteListAccN} = lists:foldl(F, {[], []}, InviteList),
            case DbInviteListAccN of
                [] -> ignore;
                _ ->
                    util:insert_values(?SQL_ROLE_INVITE_REPLACE_P1, ?SQL_ROLE_INVITE_REPLACE_P2, DbInviteListAccN),
                    put(?DICT_ROLE_INVITE, InviteListAccN)
            end
    end.

%% @doc 被邀请进入玩家处理
b_invite_handle(InviteAccount, Url, Account, RoleId) ->
    InviteAccountN = util_type:to_binary(util_type:to_list(InviteAccount)),
    case lib_role_db:get_role_id_by_account(InviteAccountN) of
        {ok, [InviteId]} ->
            case lib_role_aid:is_online(InviteId) of
                true ->
                    ?INFO("===========>this is online InviteAccount:~w, RoleId:~w", [InviteAccount, RoleId]),
                    svr_role:apply_cast(InviteId, lib_invite_api, invite_handle_online, [RoleId, Url, Account]);
                false ->
                    invite_handle_offline(RoleId, Url, InviteId, Account)
            end;
        _ERR -> ignore
    end.


%% @doc 邀请玩家领取奖励
invite_reward(Id, Role) ->
    case catch check_invite_reward(Id) of
        {error, ErrCode} ->
            {ok, Bin} = lib_proto:pack(11404, #'InviteRewardRes'{code = ErrCode}),
            lib_send:send_to_role(Role, Bin),
            {ok};
        {true, InviteList, RoleInvite, Reward} ->
            % 发送奖励
            RoleN = lib_role_asset:add_assets([{?TYPE_DIAMOND, Reward}], Role),
            % 改变数据
            RoleInviteN = RoleInvite#role_invite{is_reward = ?REWARD_SUCCESS, sign = true},
            InviteListN = lists:keystore(Id, #role_invite.sort, InviteList, RoleInviteN),
            put(?DICT_ROLE_INVITE, InviteListN),
            % 协议推送
            {ok, Bin} = lib_proto:pack(11404, #'InviteRewardRes'{id = Id}),
            lib_send:send_to_role(RoleN, Bin),
            {ok, RoleN}
    end.

%% @doc 查询显示数
send_invite_info(Role)->
    InviteList = get(?DICT_ROLE_INVITE),
    Data = switch_to_data(InviteList),
    {ok, Bin} = lib_proto:pack(11402, #'InviteInfoRes'{infos = Data}),
    lib_send:send_to_role(Role, Bin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 数据库操作 得到邀请列表
sql_invite_list(RoleId) ->
    F = fun([BInviteId, BAccount, BInviteUrl, Sort, _IsReward]) ->
        #role_invite{b_invite_id = BInviteId, b_invite_account = BAccount, b_invite_url = BInviteUrl, sort = Sort} end,
    Sql = io_lib:format(?SQL_ROLE_INVITE_FETCH, [RoleId]),
    case db:get_all(Sql) of
        {ok, List} -> lists:map(F, List);
        _ -> []
    end.

%% @doc 玩家在线处理
invite_handle_online(InviteId, InviteUrl, Account) ->
    InviteList = get(?DICT_ROLE_INVITE),
    case lists:keyfind(InviteId, #role_invite.b_invite_id, InviteList) of
        false -> invite_handle_online(InviteId, InviteUrl, Account, InviteList);
        _ -> ignore
    end.
invite_handle_online(InviteId, InviteUrl, Account, InviteList) ->
    RoleInvite = #role_invite{b_invite_id = InviteId, b_invite_account = Account, b_invite_url = InviteUrl, sort = length(InviteList) + 1, sign = true},
    InviteListN = [RoleInvite | InviteList],
    put(?DICT_ROLE_INVITE, InviteListN).

%% @doc 玩家离线处理
invite_handle_offline(InviteId, InviteUrl, RoleId, Account) ->
    InviteList = sql_invite_list(RoleId),
    case lists:keyfind(InviteId, #role_invite.b_invite_id, InviteList) of
        false ->
            DbInviteList = [[RoleId, InviteId, util_type:to_binary(Account), util_type:to_binary(InviteUrl), length(InviteList) + 1, 0]],
            util:insert_values(?SQL_ROLE_INVITE_REPLACE_P1, ?SQL_ROLE_INVITE_REPLACE_P2, DbInviteList);
        _ -> ignore
    end.

%% @doc 邀请奖励领取检查
check_invite_reward(Id) ->
    % 判断是否为邀请礼包
    Type = conf_gift:get_gift_type(Id),
    ?IF(Type =/= ?INVITE_GIFT, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    InviteList = get(?DICT_ROLE_INVITE),
    % 判断数据是否初始化到
    ?IF(InviteList =:= undefined, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    % 判断是否达到邀请数
    Condition = conf_gift:get_gift_condition(Id),
    ?IF(length(InviteList) < Condition, erlang:throw({error, ?ERR_COMMON_REWARD_LIMIT}), ignore),
    % 判断奖励是否领取
    ?IF(lists:keyfind(Condition, #role_invite.sort, InviteList) =:= false, erlang:throw({error, ?ERR_COMMON_SYS}), ignore),
    #role_invite{is_reward = IsReward} = RoleInvite = lists:keyfind(Condition, #role_invite.sort, InviteList),
    ?IF(IsReward =:= ?REWARD_SUCCESS, erlang:throw({error, ?ERR_COMMON_REWARD_ALREADY}), ignore),
    Reward = conf_gift:get_gift_diamond(Id),
    {true, InviteList, RoleInvite, Reward}.

%% @doc 转换协议数据
switch_to_data(BinList) ->
    F= fun(#role_invite{b_invite_account = TokenId, is_reward = IsReward, b_invite_url = BInviteUrl, sort = Sort})->
        Args = ?IF(IsReward =:= ?REWARD_SUCCESS, true, false),
        #'InviteInfo'{tokenId = TokenId, isReward = Args, headUrl = BInviteUrl, sort = Sort} end,
    lists:map(F, BinList).