%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 邀请好友协议处理
%%% @end
%%% Created : 19. 十二月 2018 16:33
%%%-------------------------------------------------------------------
-module(lib_invite_rpc).
-author("suyang").

%% API
-export([handle/3]).

-include("common.hrl").
-include("role.hrl").
-include("logic.hrl").



%% 邀请玩家信息请求
handle(11401, #'InviteInfoReq'{}, Role) ->
    lib_invite_api:send_invite_info(Role),
    {ok};
%% 邀请玩家领取奖励
handle(11403, #'InviteRewardReq'{id = Id}, Role) ->
    lib_invite_api:invite_reward(Id, Role);
%% 容错处理
handle(_Cmd, _Data, #role{account = Account}) ->
    ?ERROR_MSG("[~s]receive unknow data[~w]: ~w", [Account, _Cmd, _Data]),
    {ok}.