{application, main,
    [
        {description, "server"},
        {vsn, "0.1"},
        {modules, []},
        {registered, [main]},
        {applications, [kernel, stdlib]},
        {mod, {main, []}},
        {start_phases, []},
        {env, [
                {server_no, 1}
                ,{port, 9000}  %% 游戏端口
                ,{web_port, 10008} %%后台端口
                ,{game_ready, true} %%是否开放端口
                ,{gm, false} %%是否开放GM
                ,{ip, "127.0.0.1"}   %% 自己的ip
                ,{web_ip, [{192, 168, 0, 152}, {192, 168, 0, 123}]}  %% 游戏后台ip
                ,{center, 'center@192.168.0.79'}  %%中央服节点
                ,{local_center, true}              %%是否是本地服
                ,{loginAppId, ""}
                ,{loginAppSecret, ""}
                
                ,{db_cfg,                          %% 数据库配置
                        {
                        "127.0.0.1"                  %% ip
                        ,3306                        %% 端口
                        ,"root"                      %% 用户名
                        ,"123456"                    %% 密码
                        ,"server_plane"                  %% 表名
                        ,utf8mb4                     %% 数据格式
                        ,5                           %% 最小连接数
                        ,10                          %% 最大连接数
                      }
                }
            ]}
    ]
}.
