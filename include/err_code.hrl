%%-------------------------------------------------------
%% @File     : err_code.hrl
%% @Brief    : 通用错误码定义
%% @Author   : sy
%% @Date     : 2019-05-14
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(_ERR_CODE_H__).
-define(_ERR_CODE_H__, 0).

%% 辅助宏
-define(ERROR_TOC(Role, ErrCode), lib_role_aid:notify_ret_code(Role, ErrCode)).
-define(ERROR_TOC(Role, ErrCode, Args), lib_role_aid:notify_ret_code(Role, ErrCode, Args)).
-define(ERROR_TOC_KF(RoleNode, Role, ErrCode), lib_role_aid:notify_ret_code_kf(RoleNode, Role, ErrCode)).
-define(ERROR_TOC_KF(RoleNode, Role, ErrCode, Args), lib_role_aid:notify_ret_code_kf(RoleNode, Role, ErrCode, Args)).

-define(ERR_COMMON_SYSTEM, 10000).                      %% 系统错误
-define(ERR_COMMON_SERVICE_NOT_START, 10001).           %% 系统服务未启动
-define(ERR_COMMON_BAD_CONF, 10002).                    %% 配置错误
-define(ERR_COMMON_LV_LIMIT, 10003).                    %% 尚未达到开放等级
-define(ERR_COMMON_VIP_LIMIT, 10004).                   %% 请提升VIP等级
-define(ERR_COMMON_GOODS_LIMIT, 10005).                 %% 所需物品数量不足
-define(ERR_COMMON_COIN_LIMIT, 10006).                  %% 所需铜币不足
-define(ERR_COMMON_GOLD_LIMIT, 10007).                  %% 所需元宝不足
-define(ERR_COMMON_BCOIN_LIMIT, 10008).                 %% 所需绑定铜币不足
-define(ERR_COMMON_BGOLD_LIMIT, 10009).                 %% 所需绑定元宝不足
-define(ERR_COMMON_AWARD_IS_FETCHED, 10010).            %% 奖励已被领取
-define(ERR_COMMON_AWARD_FETCH_LIMIT, 10011).           %% 奖励领取条件不足
-define(ERR_COMMON_TIMES_LIMIT, 10012).                 %% 已经到达次数上限
-define(ERR_COMMON_CONDITION_LIMIT, 10013).             %% 需求条件不满足
-define(ERR_COMMON_MAX_LV, 10014).                      %% 已经达到最大等级
-define(ERR_COMMON_CD_LIMIT, 10015).                    %% 处于冷却时间中
-define(ERR_COMMON_NOT_IN_CD, 10016).                   %% 非冷却时间中
-define(ERR_COMMON_OPEN_DAYS_LIMIT, 10017).             %% 开服天数限制
-define(ERR_COMMON_MERGE_DAYS_LIMIT, 10018).            %% 合服天数限制
-define(ERR_COMMON_GOODS_NO_EXIST, 10019).              %% 物品不存在
-define(ERR_COMMON_CAREER_NO_EXIST, 10020).             %% 职业不存在
-define(ERR_COMMON_SEX_NO_EXIST, 10021).                %% 性别不存在
-define(ERR_COMMON_NAME_ILLEGAL_STRING, 10022).         %% 名字存在非法字符串，请重新输入
-define(ERR_COMMON_NAME_LEN_TOO_LONG, 10023).           %% 名字长度为2~6个汉字
-define(ERR_COMMON_NAME_CONTENT_SENSITIVE, 10024).      %% 名字存在敏感字符，请重新输入
-define(ERR_COMMON_NAME_ALREADY_USED, 10025).           %% 该名字已经被使用，请重新输入
-define(ERR_COMMON_CAREER_NO_MATCH, 10026).             %% 您不符合职业限制
-define(ERR_COMMON_LEVEL_LIMIT, 10027).                 %% 尚未达到限制等级
-define(ERR_COMMON_REACH_MAX_LEVEL, 10028).             %% 已经达到最大等级
-define(ERR_COMMON_ROLE_DEAD, 10029).                   %% 角色已经死亡
-define(ERR_COMMON_NO_PK_VALUE, 10030).                 %% 不存在该战斗模式
-define(ERR_COMMON_ROLE_OFFLINE, 10031).                %% 该玩家暂时不在线
-define(ERR_COMMON_ROLE_NOT_EXIST, 10032).              %% 该玩家不存在
-define(ERR_COMMON_ROLE_NOT_IN_GUILD, 10033).           %% 没有加入任何帮派
-define(ERR_COMMON_ROLE_NOT_IN_TEAM, 10034).            %% 没有加入任何队伍
-define(ERR_COMMON_ARGS_ERROR, 10035).                  %% 系统参数错误
-define(ERR_COMMON_SCORE_LIMIT, 10036).                 %% 所需积分不够
-define(ERR_COMMON_NOT_OPEN_ACTIVITY, 10067).           %% 活动尚未开启
-define(ERR_COMMON_GIVE_AWARD_SUCC, 10068).             %% 奖励成功发放
-define(ERR_COMMON_SCENE_NOT_EXIST, 10069).             %% 该场景不存在
-define(ERR_COMMON_NOT_AWARD_CAN_GET, 10070).           %% 没有任何奖励可以领取
-define(ERR_COMMON_BAN_USE_ITME, 10071).                %% 您正处于禁止使用物品状态中
-define(ERR_COMMON_ROLE_HP_FULL, 10072).                %% 您已经是满血状态中
-define(ERR_COMMON_CHARGE_LIMIT, 10073).                %% 查不到您的任何充值记录
-define(ERR_COMMON_SEX_NO_MATCH, 10074).                %% 您不符合性别限制
-define(ERR_COMMON_LOGINDAY_NOT_ENOUGH, 10075).         %% 您的登录天数不足
-define(ERR_COMMON_ALREADY_ACTIVE, 10076).              %% 该系统已经激活

-define(ERR_LOGIN_IP_BAN, 90000).                       %% 当前IP已被封禁
-define(ERR_LOGIN_DEVICE_BAN, 90001).                   %% 当前设备已被封禁
-define(ERR_LOGIN_ACC_BAN, 90002).                      %% 当前账号已被封禁
-define(ERR_LOGIN_ACC_BAN_GOLD, 90003).                 %% 当前账号已被封禁（买卖钻石）
-define(ERR_LOGIN_ACC_BAN_ALWAYS, 90004).               %% 当前账号已被封禁（永久）
-define(ERR_LOGIN_ACC_BAN_UNFAIR, 90005).               %% 当前账号已被封禁（不正当竞争）
-define(ERR_LOGIN_ROLE_EXIST, 90006).                   %% 当前账号已经存在角色
-define(ERR_LOGIN_TIMES_LIMIT, 90007).                  %% 登陆次数太频繁
-define(ERR_LOGIN_STATE_INNORMAL, 90008).               %% 登陆状态不正常
-define(ERR_LOGIN_TICKET_UNPASSED, 90009).              %% 通行证验证不通过
-define(ERR_LOGIN_SUCCESSFUL, 90010).                   %% 登录成功
-define(ERR_LOGIN_KICKOUT, 90011).                      %% 被踢下线
-define(ERR_LOGIN_SERVER_DOWN, 90012).                  %% 服务器维护中

-define(ERR_GOODS_NOT_DROP_TYPE, 95119).					      %% 掉落类型不存在

-endif.