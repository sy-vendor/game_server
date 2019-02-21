%%-------------------------------------------------------
%% @File     : global.hrl
%% @Brief    : 全局数据头文件
%% @Author   : sy
%% @Date     : 2018-12-19
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__GLOBAL_H__).
-define(__GLOBAL_H__, 1).

%% 全局数据类型
-define(PERMANENT_DATA, 0).      %% 永久数据
-define(DAILY_DATA, 1).          %% 日常数据

-endif.  %% __GLOBAL_H__