%% ------------------------------
%% auther: sy
%%
%% date:   2019/05/15
%% ------------------------------

{
    application, game,
    [   
        %% 应用程序的描述
        {description, "This is game server."},

        %% 应用程序的版本
        {vsn, "1.0a"},

        %% 应用程序启动时调用模块列表
        {modules, [game]},

        %% 注意这个配置节是指定当前应用程序依赖哪些应用程序，类似Windows服务的依赖关系
        {applications, [kernel, stdlib, sasl]},

        %% 启动application的时候回调模块文件名称和Module:start/2函数的参数
        {mod, {game, []}},

        %% 应用程序的注册名称
        {registered, []},

        %% 开启阶段调用
        {start_phases, []},

        %% 参数变量配置，以key-value的形式组织配置数据，可以用application:get_env/2读取。
        {env, [
                {server, "T1"},
                {log_level, 3},                 %% 日志级别
                {log_path, "../log"},                 %% 日志路径
                {ticket, "game"},               %% 私匙
                {tcp_acceptor_num, 12},         %% 启动acceptor的数量

            %% TCP 设置
                {tcp_options, [
                    binary
                    ,{packet, 0}
                    ,{active, false}
                    ,{reuseaddr, true}
                    ,{nodelay, false}
                    ,{delay_send, true}
                    ,{exit_on_close, false}
                    ,{send_timeout, 10000}
                    ,{send_timeout_close, false}
                ]
                },

            %% 数据库
                {db_host, "localhost"},
                {db_port, 3306},
                {db_user, "root"},
                {db_pass, "123456"},
                {db_name, "game"},
                {db_encode, utf8mb4},

            %% 日志数据库
                {log_db_host, "localhost"},
                {log_db_port, 3306},
                {log_db_user, "root"},
                {log_db_pass, "123456"},
                {log_db_name, "game_log"},
                {log_db_encode, utf8mb4},

                {debug, true},          %%  环境信息
                {zone, "cn"},           %%  区域信息
                {plat_id, 10001},       %%  平台名称
                {platform, "dev"},      %%  平台标识
                {server_num, 10001},    %%  服务器ID
                {hf_server_list, []}    %%  合服列表
           ]
        }
    ]   
}.