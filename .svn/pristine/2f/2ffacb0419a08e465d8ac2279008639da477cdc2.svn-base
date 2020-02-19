%%----------------------------------------------------
%% @doc web的前后端通讯路由
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_rpc).

-export([
        handle/4
        ,check/2
    ]).

-include("error_msg.hrl").
-include("all_pb.hrl").
-include("role.hrl").
-include("common.hrl").


%% 接口数据过来
handle(web_login_rpc, Cmd, Data, Socket) ->
    case check(Cmd, Data) of
        {ok, NewData} ->
            web_login_rpc:handle(Cmd, NewData, Socket);
        {false, Reason} ->
            {false, Reason}
    end;
handle(Mod, Cmd, Data, _Socket) ->
    case get_role_pid(Data) of
        {ok, Pid} ->
            case check(Cmd, Data) of
                {ok, NewData} ->
                    role:handle_web_rpc(Pid, Mod, Cmd, NewData);
                {false, Reason} ->
                    {false, Reason}
            end;
        {false, Reason} ->
            {false, Reason}
    end.


%% 获取人物的pid
get_role_pid(Data) ->
    case lists:keyfind(role_id, 1, Data) of
        {role_id, RoleId} ->
            {secret, Id} = lists:keyfind(secret, 1, Data),
            case role_data:get_online_role(RoleId) of
                {ok, #online_role{secret = Id, pid = Pid}} ->
                    {ok, Pid};
                _ ->
                    {false, ?error_screat}
            end;
        _ ->
            {false, ?error_act}
    end.

%% 检查数据是否合法
check(Cmd, Data) ->
    case check_config(Cmd, Data) of
        {ok, NewData} ->
            {ok, NewData};
        {false, Reason} ->
            {false, Reason}
    end.


%% 登陆处理
login(Data) ->
    case check_config(login, Data) of
        {ok, NewData} ->
            case login:web_login(NewData) of
                {ok, Info, _Id, _NewState} ->
                    List = record_info(fields, p_role_info),
                    [_ | List1] = erlang:tuple_to_list(Info),
                    List2 = lists:zip(List, List1),
                    {ok, List2};
                {false, Reason} -> 
                    {false, Reason}
            end;
        {false, Reason} ->
            {false, Reason}
    end.



%% 根据webconfig解析json数据
check_config(Type, Data) ->
    Config = web_config:get_tos(Type),
    do_config(Config, Data, []).

do_config([{Key, Option, Type} | L], Data, List) ->
    case lists:keyfind(Key, 1, Data) of
        {Key, Value} ->
            case check_value(Value, Type) of
                true ->
                    do_config(L, Data, [Value | List]);
                _ ->
                    {false, ?error_act}
            end;
        _ ->
            case Option of
                optional -> 
                    Value = default(Type),
                    do_config(L, Data, [Value | List]);
                repeated ->
                    do_config(L, Data, [[] | List]);
                _ ->
                    {false, ?error_act}
            end
    end;
do_config([], _Data, List) ->
    {ok, erlang:list_to_tuple(lists:reverse(List))}.



%% 类型的默认值
default(string) -> "";
default(number) -> 0.

%% 检查类型是否合法
check_value(Value, string) ->
    erlang:is_list(Value) orelse erlang:is_binary(Value);
check_value(Value, number) ->
    erlang:is_integer(Value).
