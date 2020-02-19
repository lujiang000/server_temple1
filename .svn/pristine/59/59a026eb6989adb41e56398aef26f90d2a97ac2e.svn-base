%%----------------------------------------------------
%% @doc web通讯协议配置
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_config).
-export([
        get_tos/1
        ,get_toc/1
    ]
).

%% {字段名(atom), 属性(required, optional, repeated)， 类型(string, number, RecordName)}

%% 微信登陆
get_tos(wx_login) ->
    [
        {code,       required, string}
        ,{name,      optional, string}
        ,{icon,      optional, string}
        ,{sex,       optional, number}
        ,{country,   optional, string}
        ,{provice,   optional, string}
        ,{city,      optional, string}
        ,{parent_id, optional, number}
    ];
%% 账号登陆
get_tos(account_login) ->
    [
        {open_id,    required, string}
        ,{name,      optional, string}
        ,{icon,      optional, string}
        ,{sex,       optional, number}
        ,{country,   optional, string}
        ,{provice,   optional, string}
        ,{city,      optional, string}
        ,{parent_id, optional, number}
    ];

%% 密匙登陆
get_tos(secret_login) ->
    [
        {role_id,    required, number}
        ,{secret,    required, number}
    ];

%% 获取数据
get_tos(get_data) ->
    [
        {game_id,  required, number}
    ];


%% 设置数据
get_tos(set_data) ->
    [
        {game_id,   required, number}
        ,{data,     repeated, string}
    ];

%% 设置排行数据
get_tos(set_rank) ->
    [
        {value,  required, number}  %% 排行分数，米
    ];

%% 获取排行数据
get_tos(get_rank) ->
    [
        {start, required, number}  %% 开始排名，0开始
        ,{num,   required, number}  %% 获取排名数量
        ,{flag,  required, number}  %% 向下去或者向上取数据，0向下，1向上
    ];

%% 错误日志
get_tos(error_log) ->
    [
        {log, required, string}
    ];

%% 同步用户数据
get_tos(sync_data) ->
    [
        {name,  required, string}  %% 名字
        ,{icon,  required, string}  %% 头像
        ,{sex,   required, number}  %% 性别
        ,{country, required, string}  %% 国家
        ,{province, required, string}  %% 省份
        ,{city,    required, string}  %% 城市
    ].

%% 排行数据返回
get_toc(get_rank) ->
    [
        {list, repeated, [{num, name, value}]}  %% 排行列表，{名次， 名字， 排行值}
        ,{num, required, number} %% 自己的排名，0代表未上榜
        ,{value, required, number} %% 自己的排行值
    ];


%%登陆数据返回 
get_toc(login) ->
    [
        {secret,     required, number}
        ,{role_id,   required, number}
        ,{nick_name, optional, string}
        ,{icon,      optional, string}
        ,{gold,      optional, number}
        ,{diamond,   optional, number}
        ,{openid,    optional, string}
    ].

