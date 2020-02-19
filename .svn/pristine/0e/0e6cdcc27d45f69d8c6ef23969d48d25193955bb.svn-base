%%----------------------------------------------------
%% @doc 短链接登陆
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_role_rpc).
-export([
        handle/3
    ]
).

-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").
-include("rank.hrl").
-include("all_pb.hrl").

%% 设置数据
handle(set_data, {Key, Data}, Role = #role{data = DataList}) ->
    NewList = lists:keystore(Key, 1, DataList, {Key, Data}),
    {ok, Role#role{data = NewList}};

%% 获取数据
handle(get_data, {Key}, _Role = #role{data = DataList}) ->
    Reply = case lists:keyfind(Key, 1, DataList) of
        {Key, Data} -> [{data, Data}];
        _ -> []
    end,
    {reply, Reply};

%% 设置数据排行榜数据
handle(set_rank, {Value}, Role = #role{data = DataList}) ->
    Key = rank_value,
    case lists:keyfind(Key, 1, DataList) of
        {rank_value, Value1} when Value > Value1 ->
            NewList = lists:keystore(Key, 1, DataList, {Key, Value}),
            rank:handle(?rank_score, Role, Value),
            {ok, Role#role{data = NewList}};
        false ->
            rank:handle(?rank_score, Role, Value),
            NewList = lists:keystore(Key, 1, DataList, {Key, Value}),
            {ok, Role#role{data = NewList}};
        _ ->
            ok
    end;

%% 获取数据
handle(get_rank, {Start, Num, UpDown}, Role = #role{data = DataList}) ->
    Key = rank_value,
    Default = case lists:keyfind(Key, 1, DataList) of
        {rank_value, Value0} -> Value0;
        _ -> 0
    end,
    {List, N, Value} = rank_mgr:get_rank_info(Role, Start, ?rank_score, Num, UpDown),
    Value1 = case N of
        0 -> Default;
        _ -> Value
    end,
    List1 = [[{num, Num1}, {name, Name}, {value, Value2}]|| #p_rank_info{num = Num1, name = Name, value1 = Value2}<-List],
    {reply, [{list, List1}, {num, N}, {value, Value1}]};

%% 同步玩家资料
handle(sync_data, {Name, Icon, Sex, Country, Province, City}, Role) ->
    {ok, Role#role{name = Name, sex = Sex, icon = Icon, country = Country, province = Province, city = City}};

%% 同步玩家资料
handle(error_log, {Msg}, #role{role_id = RoleId, name = Name}) ->
    case get(client_error_log) of
        Msg -> ok;
        _ ->
            put(client_error_log, Msg),
            log_db:log(client_error_log, insert, [RoleId, Name, Msg, date:unixtime()])
    end,
    ok;


handle(_, _, _) ->
    {false, ?error_act}.
