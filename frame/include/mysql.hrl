%%-------------------------------------------------------
%% @File     : mysql.hrl
%% @Brief    : mysql头文件
%% @Author   : sy
%% @Date     : 2018-12-07
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__MYSQL_H__).
-define(__MYSQL_H__, 1).


-record(mysql_result, {
    fieldinfo=[],
    rows=[],
    affectedrows=0,
    error=""
}).

-endif.  %% __MySql_H__