%%----------------------------------------------------
%% @doc 登陆处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(login).
-export([
        wx_login/2
       ,secret_login/3
       ,login/2
    ]).


-include("common.hrl").
-include("role.hrl").
-include("conn.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

-define(default_icon_list, [
    "http://www.zhongchuanxinxi.com/baping/public/user/ZmAA5a5c743b9d5bd6445_thumb.png"
    ,"baping/public/user/epUm5a638dc7240f41476_thumb.png"
    ,"http://wx.qlogo.cn/mmopen/vi_32/Q0j4TwGTfTLxAy8ugneYiccpB9ThqLMDBIJZ2ticdMMEKmPq1SAEdnLx7nxo22GcbmseTc2rAosDpVmAvfYcPe8A/0"
    ,"http://wx.qlogo.cn/mmopen/vi_32/DYAIOgq83eoiaE7OvwXbU80WFWAs3FN9VaY21rcCdg1tcD7FC2PzGeeMNQr8HK7OgUbiaymTicxmDlpCFm7yfubAA/0"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048355&di=8d516bede6e3eb3751aeb9f32a4e0fe1&imgtype=0&src=http%3A%2F%2Fimg.18183.com%2Fuploads%2Fallimg%2F183%2F180120%2F201-1P1201A533.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048355&di=8d516bede6e3eb3751aeb9f32a4e0fe1&imgtype=0&src=http%3A%2F%2Fimg.18183.com%2Fuploads%2Fallimg%2F183%2F180120%2F201-1P1201A533.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048354&di=9685ea2fb712426fd1cb279e85da0993&imgtype=0&src=http%3A%2F%2Fimg.9553.com%2Fuploadfile%2F2018%2F0119%2F20180119024738725.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048354&di=fde76d6264810935d81667d10a139154&imgtype=0&src=http%3A%2F%2Fupload.techweb.com.cn%2Fs%2F640%2F2018%2F0122%2F1516603531513.jpg"
    ,"https://timgsa.baidu.com/timg?image&quality=80&size=b9999_10000&sec=1516879048353&di=1f391b732c65ffc84cff801f4c5eb08e&imgtype=0&src=http%3A%2F%2Fdown.52pk.com%2Fuploads%2F180119%2F5010_094843_7422.jpg"
]).

%% 断线重连
secret_login(RoleId, Id, State = #conn{ip = Ip}) ->
    case role_data:get_online_role(RoleId) of
        {ok, #online_role{pid = Pid, secret = Id}} ->    %% 在线断线重连
            case is_process_alive(Pid) of
                true ->
                    LoginRole = gen_server:call(Pid, {reconnect, self(), Ip}),
                    {ok, LoginRole, Id, State#conn{pid_object = Pid}};
                _ ->
                    {false, ?error_screat}
            end;
        _ ->
            {false, ?error_screat}
    end.

%% 获取openid
get_openid(Code) ->
    PropList = [
        {appid, sys_env:get_env(loginAppId)},
        {secret, sys_env:get_env(loginAppSecret)},
        {js_code, Code},
        {grant_type, authorization_code}
    ],
    Params = string:join([atom_to_list(Key) ++ "=" ++ util:to_list(Val) || {Key, Val} <- PropList], "&"),
    Url = "https://api.weixin.qq.com/sns/jscode2session",
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    case httpc:request(get, {Url++"?"++Params, Headers}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _List, Result}} ->
            case catch json:decode(erlang:list_to_binary(Result), [{object_format,proplist}]) of
                Json when is_list(Json) ->
                    case lists:keyfind(<<"openid">>, 1, Json) of
                        {_, OpenID} ->
                            {ok, OpenID};
                        _ ->
                            ?ERR("微信获取openid失败：~w", [Json]),
                            ""
                    end;
                _ErrorJson ->
                    ?ERR("微信获取openid失败：json解析错误", []),
                    ""
            end;
        _ ->
            ?ERR("微信获取openid失败：timeout", []),
            ""
    end.

%% 微信登陆
wx_login({Code, NickName, Icon, Sex, Country, Province, City, ParentId}, State) ->
    case get_openid(Code) of
        {ok, OpenId} ->
            login({OpenId, NickName, Icon, Sex, Country, Province, City, ParentId}, State);
        _ ->
            {false, ?error_busy}
    end.

%%web_login({Code, NickName, Icon, Sex, Country, Province, City, ParentId}) ->
%%    case get_openid(Code) of
%%        {ok, OpenId} ->
%%            login({OpenId, NickName, Icon, Sex, Country, Province, City, ParentId}, #conn{});
%%        _ ->
%%            {false, ?error_busy}
%%    end.




%%登陆
login({OpenId, NickName, Icon, Sex, Country, Province, City, ParentId1}, State = #conn{ip = IP}) ->
    case db:get_one("select role_id from role where openid = ?", [OpenId]) of
        {ok, RoleId} when is_integer(RoleId) ->   %% 老玩家
            ParentId = case RoleId =:= ParentId1 of
                           true -> 0;
                           _ -> ParentId1
                       end,
            case role_data:get_online_role(RoleId) of
                {ok, #online_role{pid = Pid, secret = Secret}} ->    %% 在线断线重连
                    case catch  gen_server:call(Pid, {reconnect, self(), IP, ParentId}) of
                        LoginRole = #p_role_info{} ->
                            {ok, LoginRole, Secret, State#conn{pid_object = Pid, role_id = RoleId}};
                        _ ->
                            ets:delete(online_role, RoleId),
                            {false, ?error_busy}
                    end;
                _ ->  
                    Secret = sys_rand:rand(10000, 999999),
                    case role_data:get_role_from_dets(RoleId) of
                        {ok, Term} ->
                            case role_var:update_var(Term) of
                                {ok, Role = #role{}} ->
                                    {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, secret = Secret, parent_id = ParentId}),
                                    {ok, role_conver:to_login_role(Role), Secret, State#conn{pid_object = Pid, role_id = RoleId}};
                                _Err ->
                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                    {false, ?error_role_data}
                            end;
                        [] ->
                            case db:get_one("select info from role where role_id = ?", [RoleId]) of
                                {ok, Info} ->
                                    case util:string_to_term(Info) of
                                        {ok, Term} ->
                                            case role_var:update_var(Term) of
                                                {ok, Role = #role{}} ->
                                                    {ok, Pid} = role:start(Role#role{socket_pid = self(), ip = IP, secret = Secret, parent_id = ParentId}),
                                                    {ok, role_conver:to_login_role(Role), Secret, State#conn{pid_object = Pid, role_id = RoleId}};
                                                _Err ->
                                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                                    {false, ?error_role_data}
                                            end;
                                        {error, _Err}->
                                            ?ERR("~w的扩展数据无法正确转换成term(): ~w", [RoleId, _Err]),
                                            {false, ?error_role_data}
                                    end;
                                _Err ->
                                    ?ERR("数据库读取玩家数据错误~w", [_Err]),
                                    {false, ?error_busy}
                            end;
                        _Err ->
                            {false, ?error_busy}
                    end
            end;
        {ok, undefined} ->
            create({OpenId, NickName, Icon, Sex, Country, Province, City, ParentId1}, State);
        _Err ->
            ?ERR("数据库读取玩家数据错误~w", [_Err]),
            {false, ?error_busy}
    end.


%% 创建一个新玩家
create({OpenId, NickName, Icon, Sex, Country, Province, City, ParentId}, State = #conn{ip = Ip}) ->
    Secret = sys_rand:rand(10000, 9999999),
    RoleId = auto_increment:get_auto_id(role),
    Now = date:unixtime(),
    Role = #role{
        role_id = RoleId
        ,socket_pid = self()
        ,register_time = Now
        ,login_time = Now
        ,openid = OpenId
        ,ip = Ip
        ,name = NickName
        ,icon = Icon
        ,sex = Sex
        ,secret = Secret
        ,country =  Country
        ,province =  Province
        ,city = City
        ,parent_id = ParentId
    },
    case role_data:new_to_db(Role) of
        ok ->
            {ok, Pid} = role:start(Role),
            role_account_mgr:registe(0),
            {ok, role_conver:to_login_role(Role), Secret, true, State#conn{pid_object = Pid, account = RoleId}};
        _ ->
            {false, ?error_busy}
    end.



