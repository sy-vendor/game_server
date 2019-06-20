%%-------------------------------------------------------
%% @File     : global.hrl
%% @Brief    : 全局数据键值对定义
%% @Author   : sy
%% @Date     : 2019-05-15
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__GLOBAL_DATA_H__).
-define(__GLOBAL_DATA_H__, 0).

%% 全局数据类型
-define(PERMANENT_DATA, 0).      %% 永久数据
-define(DAILY_DATA, 1).          %% 日常数据

-endif.  %% __GLOBAL_DATA_H__