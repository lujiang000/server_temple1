
%% 单个计时器
-record(timer, {
        id                 %% 自定义结构{type, id}
        ,mfa               %% 回调方法{module, function, args:[]} 返回值一定要是#role{}
        ,end_time          %% 结束时间
        ,time              %% 持续时间
}).

%% 倒计时控制器
-record(role_timer, {
        ref               %% #ref
        ,now              %% 当前正在运行的倒计时
        ,list = []        %% 等待运行的倒计时
}).


%% 人物数据结构版本号
-define(role_var, 0).
-record(role, {
        var = ?role_var           %% 人物版本号
        ,role_id = 0              %% 人物唯一id
        ,name = ""                %% 人物名字
        ,pid                      %% 进程pid
        ,socket_pid               %% 连接进程pid
        ,icon = ""                %% 头像
        ,gold = 10000             %% 金币
        ,secret = 0               %% 登陆密匙
        ,openid = ""              %% openid
        ,ip = ""                  %% ip地址
        ,loop_counter = 0         %% 循环次数
        ,need_sync = false        %% 是否需要保存
        ,sex = 0                  %% 性别
        ,register_time = 0        %% 注册时间
        ,login_time = 0           %% 最后一次登陆时间
        ,off_time = 0             %% 最后一次离线时间
        ,charge = 0               %% 总充值额(分)
        ,daily_value = []         %% 每日次数记录{key, value}
        ,diamond = 0              %% 钻石
        ,country  = ""            %% 国家
        ,province = ""            %% 省份
        ,city = ""                %% 城市
        ,role_timer = #role_timer{} %% 倒计时控制器
        ,parent_id = 0            %% 上级id
        ,data = []                %% 数据{key, value}
    }
).



%% 每日数据类型
-define(daily_luck_turn, 1).  %% 每日转盘
-define(daily_invite_firend, 2).     %% 邀请好友数量

%% 每日数据限制
-define(max_luck_turn, 3).    %% 每天最多3次
-define(max_invite_firend, 3).    %% 每天最多3次

%% 在线玩家数据结构
-record(online_role, {
        role_id
        ,name = ""
        ,pid
        ,socket_pid
        ,icon = ""
        ,secret = 0
        ,gold = 0
    }
).

