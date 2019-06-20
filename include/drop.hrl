%%-------------------------------------------------------
%% @File     : drop.hrl
%% @Brief    : 物品掉落
%% @Author   : sy
%% @Date     : 2019-05-28
%%---------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__DROP_H__).
-define(__DROP_H__, 0).

%% 掉落物品类型
-define(DROP_EXP,           0).         %% 经验
-define(DROP_COIN,          1).         %% 铜币
-define(DROP_BCOIN,         2).         %% 绑定铜币
-define(DROP_GOLD,          3).         %% 元宝
-define(DROP_BGOLD,         4).         %% 绑定元宝
-define(DROP_BIND_GOLD,     5).         %% 仅绑定元宝
-define(DROP_EQUIP,         6).         %% 装备
-define(DROP_BIND_COIN,     7).         %% 仅绑定铜币
-define(DROP_ITEM,          8).         %% 道具

-endif.  %% _DROP_H__
