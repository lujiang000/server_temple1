%%----------------------------------------------------
%% @doc 短链接登陆
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_login_rpc).
-export([
        handle/3
    ]
).

-include("common.hrl").
-include("conn.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

%% 微信登陆
handle(wx_login, Data, Socket) ->
    {ok, {Ip, _Port}} = inet:peername(Socket),
    case login:wx_login(Data, #conn{ip = Ip}) of
        {ok, #p_role_info{role_id = RoleId, openid = OpenId}, Id, _NewState} ->
            {ok, [{role_id, RoleId}, {secret, Id}, {openid, OpenId}, {new_user, false}]};
        {ok, #p_role_info{role_id = RoleId, openid = OpenId}, Id, _, _NewState} ->
            {ok, [{role_id, RoleId}, {secret, Id}, {openid, OpenId}, {new_user, true}]};
        {false, Reason} ->
            {false, Reason}
    end;

%% 账号登陆
handle(account_login, Data, Socket) ->
    {ok, {Ip, _Port}} = inet:peername(Socket),
    case login:login(Data, #conn{ip = Ip}) of
        {ok, #p_role_info{role_id = RoleId}, Id, _NewState} ->
            {ok, [{role_id, RoleId}, {secret, Id}, {new_user, false}]};
        {ok, #p_role_info{role_id = RoleId}, Id, _, _NewState} ->
            {ok, [{role_id, RoleId}, {secret, Id}, {new_user, true}]};
        {false, Reason} ->
            {false, Reason}
    end;

%% 断线重连
handle(secret_login, {RoleId, Secret}, Socket) ->
    {ok, {Ip, _Port}} = inet:peername(Socket),
    case login:secret_login(RoleId, Secret, #conn{ip = Ip}) of
        {ok, #p_role_info{role_id = RoleId}, Id, _NewState} ->
            {ok, [{role_id, RoleId}, {secret, Id}, {new_user, false}]};
        {false, Reason} ->
            {false, Reason}
    end;

handle(_, _, _) ->
    {false, ?error_act}.
