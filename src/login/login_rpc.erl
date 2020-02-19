%%----------------------------------------------------
%% @doc 登陆协议入口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(login_rpc).
-export([
        handle/3
    ]).

-include("all_pb.hrl").
-include("common.hrl").
-include("conn.hrl").
-include("error_msg.hrl").

%% 心跳包
handle(1098, _, State) ->
    {ok, #m_1098_toc{}, State};

%% 测试
handle(1097, _, State) ->
    {ok, #m_1097_toc{msg = "你好"}, State};

%% 账号登陆过了
handle(_, _, #conn{pid_object = Pid}) when Pid =/= undefined-> 
    {false, ?error_repeat_login};

%% 普通登陆
handle(1001, #m_1001_tos{openid = OpenId, name = Name, icon = Icon, sex = Sex, country = Country, province = Province, city = City, parent_id = ParentId}, State) ->
    case login:login({OpenId, Name, Icon, Sex, Country, Province, City, ParentId}, State) of
        {ok, Info, Id, NewState} ->
            {ok, #m_1001_toc{info = Info, id = Id}, NewState};
        {false, Reason} ->
            {false, Reason}
    end;

%% 微信登陆
handle(1002, #m_1002_tos{code = Code, name = Name, icon = Icon, sex = Sex, country = Country, province = Province, city = City, parent_id = ParentId}, State) ->
    case login:wx_login({Code, Name, Icon, Sex, Country, Province, City, ParentId}, State) of
        {ok, Info, Id, NewState} ->
            {ok, #m_1002_toc{info = Info, id = Id}, NewState};
        {false, Reason} ->
            {false, Reason}
    end;


    
%% 断线重连
handle(1003, #m_1003_tos{role_id = RoleId, id = Id}, State) ->
    case login:secret_login(RoleId, Id, State) of
        {ok, Info, Id, NewState} ->
            {ok, #m_1003_toc{info = Info, id = Id}, NewState};
        {false, Reason} ->
            {false, Reason}
    end;


handle(_Cmd, _Data, _) ->
    ?ERR("错误的协议:~w:data:~w", [_Cmd, _Data]),
    ok.
