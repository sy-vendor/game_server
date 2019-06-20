%%-------------------------------------------------------
%% @File     : battle.hrl
%% @Brief    : 战斗头文件
%% @Author   : sy
%% @Date     : 2019-05-24
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__BATTLE_H__).
-define(__BATTLE_H__, 0).

%% 战斗主体类型
-define(BATTLE_NONE, 0).
-define(BATTLE_MON, 1).             %% 怪物对象
-define(BATTLE_ROLE, 2).            %% 玩家对象

-endif.