%%-------------------------------------------------------
%% @File     : goods_sql.hrl
%% @Brief    : 物品SQL定义
%% @Author   : sy
%% @Date     : 2019-05-23
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__GOODS_SQL_H__).
-define(__GOODS_SQL_H__, 0).

%% =========================== 货币更新相关 ===========================
-define(SQL_PLAYER_UPDATE_GOLD, <<"update `role_info` set `total_got_gold`=~p, `gold`=~p, `bgold`=~p where `role_id`=~p">>).
-define(SQL_PLAYER_UPDATE_COIN, <<"update `role_info` set `coin`=~p, `bcoin`=~p where `role_id`=~p">>).
-define(SQL_PLAYER_UPDATE_CURRENCY, <<"update `role_info` set `total_got_gold`=~p, `gold`=~p, `bgold`=~p, `coin`=~p, `bcoin`=~p where `role_id`=~p">>).


-endif.  %% __GOODS_SQL_H__