%%-------------------------------------------------------
%% @File     : merge.hrl
%% @Brief    : merge头文件
%% @Author   : sy
%% @Date     : 2018-12-18
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__MERGE_H__).
-define(__MERGE_H__, 1).

%% 进程字典定义
-define(DICT_ROLE_MAP, dict_role_map).    %% 合并地图数据

-define(DICT_MAX_TYPE_LOOP, dict_max_type_loop).      %% 最大类型

%% 合并数据
-record(role_merge, {
    pos = [],       %% {PosId, PosType} | Pos
    max_type = 1,   %% 最大类型
    next_type = 1,  %% 下一生产类型
    worth = 0,      %% 价值
    sign = false    %% 更新标志
}).


%% 数据库相关
-define(SQL_ROLE_MERGE_FETCH, <<"select `pos`, `max_type`, `next_type`, `worth`  from `role_merge` where `role_id`=~p">>).
-define(SQL_ROLE_MERGE_REPLACE_P1, <<"replace into `role_merge` (`role_id`, `pos`, `max_type`, `next_type`, `worth`) values ~s">>).
-define(SQL_ROLE_MERGE_REPLACE_P2, <<"(~p, '~s', ~p, ~p, ~p)">>).

-endif.  %% __MERGE_H__