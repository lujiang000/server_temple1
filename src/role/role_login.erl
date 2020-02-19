%%----------------------------------------------------
%% @doc 人物登陆处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_login).
-export([
        do/1
    ]).

-include("role.hrl").
-include("common.hrl").
-include("rank.hrl").


%% 登陆相关处理
do(Role = #role{login_time = Time}) -> 
    Now = date:unixtime(),
    Role1 = case date:is_same_day(Time, Now) of
        true ->
            Role;
        _ ->
            role:do_zero_flush(Role)
    end,
    RoleEnd = Role1,
    role_data:sync_online_role(RoleEnd),
    RoleEnd#role{login_time = Now}.


