%%----------------------------------------------------
%% @doc 人物相关处理工具
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_lib).
-export([
        do_cost/2
        ,do_add/2
        ,do_cost_gold/2
        ,do_cost_diamond/2
        ,do_add_gold/2
        ,send_buff_begin/0
        ,send_buff_flush/0
        ,send_buff_clean/0
        ,get_value/2
        ,get_value/3
        ,set_value/3
        ,add_value/3
        ,add_value/2
    ]

).

-include("role.hrl").
-include("common.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").


%% 获取人物每日数据,不带默认值，返回0
get_value(_Role = #role{daily_value = List}, Type) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Value} -> Value;
        _ -> 0
    end.

%% 获取人物每日数据,带默认值，返回默认值
get_value(_Role = #role{daily_value = List}, Type, Default) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Value} -> Value;
        _ -> Default
    end.


%% 设置人物每日数据
set_value(Role = #role{daily_value = List}, Type, Value) ->
    NewList = lists:keystore(Type, 1, List, {Type, Value}),
    Role#role{daily_value = NewList}.

%% 增加人物每日数据 仅限数据为integer
add_value(Role, Type) ->
    add_value(Role, Type, 1).

%% 增加人物每日数据 仅限数据为integer
add_value(Role = #role{daily_value = List}, Type, Add) ->
    NewList = case lists:keyfind(Type, 1, List) of
        {Type, Value} -> 
            lists:keyreplace(Type, 1, List, {Type, Value + Add});
        _ ->
            [{Type, Add} | List]
    end,
    Role#role{daily_value = NewList}.


%% 注意 必须要在人物进程调用，并且是begin 后面必须要flush或者clean
%% 消息缓冲推送
send_buff_begin() ->
    case get(send_buff) of
        undefined  ->
            put(send_buff, []);
        _ ->
            ?ERR("缓冲池已经开始了", []),
            ok
    end.

send_buff_flush() ->
    case get(socket_pid) of
        Pid when is_pid(Pid) ->
            case get(send_buff) of
                List when is_list(List)->
                    [Pid ! {tcp_send, Data} || Data<-List],
                    put(send_buff, undefined);
                _ ->
                    ?ERR("缓冲池没有数据", []),
                    ok
            end;
        _ ->
            ?ERR("不在人物进程不能调用", [])
    end.

send_buff_clean() ->
    put(send_buff, undefined).


%% 扣除指定资产
%% 金币
do_cost_gold(Role = #role{gold = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value - Cost}]}),
    {ok, Role#role{gold = Value - Cost}};
do_cost_gold(_, _) ->
    {false, ?error_gold}.

%% 钻石
do_cost_diamond(Role = #role{diamond = Value}, Cost) when Value >= Cost->
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = diamond, num = Value - Cost}]}),
    {ok, Role#role{diamond = Value - Cost}};
do_cost_diamond(_, _) ->
    {false, ?error_gold}.

%% 批量扣除资产
do_cost(Role, []) -> {ok, Role};
do_cost(Role, List) -> 
    do_cost(Role, List, []).

do_cost(Role, [], []) -> {ok, Role};
do_cost(Role, [], List) -> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-List]}),
    {ok, Role};

%% 金币
do_cost(Role = #role{gold = OldValue}, [{gold, Value} | L], List) when OldValue >= Value->
    do_cost(Role#role{gold = OldValue - Value}, L, [{gold, OldValue - Value} | List]);
do_cost(Role = #role{diamond =  OldValue}, [{diamond, Value} | L], List) when OldValue >= Value->
    do_cost(Role#role{gold = OldValue - Value}, L, [{diamond, OldValue - Value} | List]);
do_cost(_Role, [{_Type, _} | _L], _List) ->
    {false, ?error_act}.



%% 增加指定资产
%% 金币
do_add_gold(Role = #role{gold = Value}, Add) ->
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value + Add}]}),
    {ok, Role#role{gold = Value + Add}}.

%% 批量增加资产
do_add(Role, []) -> {ok, Role};
do_add(Role, List) ->
    do_add(Role, List, []).

do_add(Role, [], []) ->  {ok, Role};
do_add(Role, [], List) -> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-List]}),
    {ok, Role};

%% 金币
do_add(Role = #role{gold = OldValue}, [{gold, Value} | L], List) -> 
    do_add(Role#role{gold = OldValue + Value}, L, [{gold, OldValue + Value} | List]);
%% 钻石
do_add(Role = #role{diamond = OldValue}, [{diamond, Value} | L], List) ->
    do_add(Role#role{diamond = OldValue + Value}, L, [{diamond, OldValue + Value} | List]);
%% buff
do_add(Role, [{buff, Value} | L], List) ->
    NewRole = role_buff:add_buff(Role, Value),
    do_add(NewRole, L, List);

do_add(Role, [{Type, _} | L], List) ->
    ?ERR("增加未知的资产类型:~w", [Type]),
    do_add(Role, L, List).



