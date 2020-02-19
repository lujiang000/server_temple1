-record(rank_role, {
        id = 0                  %%单个榜的唯一id
        ,role_id = 0
        ,name = ""
        ,icon = ""
        ,vip = 1
        ,sign = ""           %% 签名
        ,value1 = 0          %% 排序字段预留
        ,value2  
        ,value3 
        ,value4 
        ,value5 
    }
).

-record(rank, {
        type = 0
        ,list = []
        ,last_val = 0
        ,len = 0
    }
).

-record(rank_log, {
        type = 0
        ,list = []
        ,time = 0
        ,len = 0
    }
).

-define(rank_score, 1).  %% 积分排名

-record(rank_config, {
        min = 1            %%最低要求值
        ,len = 50            %%最大长度
        ,zone = rank_zone_1            %% 排行区号
        ,keys = [#rank_role.value1]
    }
).


