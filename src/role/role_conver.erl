%%----------------------------------------------------
%% @doc 人物数据转换 前端数据
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_conver).
-export([
        to_online/1
        ,to_login_role/1
]).

-include("role.hrl").
-include("all_pb.hrl").

%% 转换在线数据
to_online(#role{role_id = RoleID, name = Name, icon = Icon, pid = Pid, socket_pid = SocketPid, gold = Gold, secret = Secret}) ->
    #online_role{role_id = RoleID, name = Name, pid = Pid, socket_pid = SocketPid, icon = Icon, gold = Gold, secret = Secret}.

%% 登陆需要数据
to_login_role(#role{role_id = RoleID, openid = OpenID, name = Name, icon = Icon,  gold = Gold, diamond = Diamond}) ->
  #p_role_info{role_id = RoleID, nick_name = Name, openid = OpenID, icon = Icon,  gold = Gold, diamond = Diamond}.



