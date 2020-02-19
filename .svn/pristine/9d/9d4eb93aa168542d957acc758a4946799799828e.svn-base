%%----------------------------------------------------
%% 排行榜管理进程
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,lookup/1
        ,clean/1
        ,lookup/2
        ,get_rank_info/5
        ,lookup_list/1
        ,save_and_clean/2
        ,web_look/2
        ,get_old_rank_info/6
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("rank.hrl").
-include("role.hrl").
-include("all_pb.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 查询ets表
-spec lookup(pos_integer()) -> #rank{}.
lookup(Type) ->
    case catch ets:lookup(rank, Type) of
        [Rank = #rank{}] -> Rank;
        _ -> #rank{type = Type}
    end.

%% 获取前几的排行详细信息
lookup(Type, N) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List}] -> 
            lists:sublist(List, N);
        _ -> 
            []
    end.

%% 全部排行的列表 
lookup_list(Type) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List}] -> 
            List;
        _ -> 
            []
    end.

%% 获取排行详细信息，向下翻
get_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 0) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List, len = Len}] when Start < Len + 1-> 
            NewList = lists:sublist(List, Start + 1, Num),
            NewList1 = to_p_rank_info(NewList, Start + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        [#rank{list = List}] ->
            {Index, Value} = find_index(RoleId, List),
            {[], Index, Value};
        _ -> 
            {[], 0, 0}
    end;
%% 向上翻
get_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 1) ->
    {NewStart, NewNum} = case Start - 1 - Num >= 0 of
        true ->
            {Start -1 -Num, Num};
        _ ->
            {0, Start -1}
    end,
    case catch ets:lookup(rank, Type) of
        [#rank{list = List, len = Len}] when Start =< Len -> 
            NewList = lists:sublist(List, NewStart + 1, NewNum),
            NewList1 = to_p_rank_info(NewList, NewStart + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        _ -> 
            {[], 0, 0}
    end.

%% 获取以前排行详细信息，向下翻
get_old_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 0, Date) ->
    Time = date:unixtime(zero) - 86400 * Date,
    case catch ets:match_object(rank_log, #rank_log{type = Type, time = Time, _ = '_'}) of
        [#rank_log{list = List, len = Len}] when Start < Len + 1-> 
            NewList = lists:sublist(List, Start + 1, Num),
            NewList1 = to_p_rank_info(NewList, Start + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        [#rank_log{list = List}] ->
            {Index, Value} = find_index(RoleId, List),
            {[], Index, Value};
        _ -> 
            {[], 0, 0}
    end;
%% 向上翻
get_old_rank_info(_Role = #role{role_id = RoleId}, Start, Type, Num, 1, Date) ->
    {NewStart, NewNum} = case Start - 1 - Num >= 0 of
        true ->
            {Start -1 -Num, Num};
        _ ->
            {0, Start -1}
    end,
    Time = date:unixtime(zero) - 86400 * Date,
    case catch ets:match_object(rank_log, #rank_log{type = Type, time = Time, _ = '_'}) of
        [#rank_log{list = List, len = Len}] when Start =< Len -> 
            NewList = lists:sublist(List, NewStart + 1, NewNum),
            NewList1 = to_p_rank_info(NewList, NewStart + 1, []),
            {Index, Value} = find_index(RoleId, List),
            {NewList1, Index, Value};
        _ -> 
            {[], 0, 0}
    end.

%% 查找排行榜多少名
find_index(RoleId, List) ->
    case lists:keyfind(RoleId, #rank_role.role_id, List) of
        #rank_role{} ->
            find_index(RoleId, List, 1);
        _ ->
            {0, 0}
    end.
find_index(_RoleId, [], _N) -> {0, 0};
find_index(RoleId, [#rank_role{role_id = RoleId, value1 = Value} | _], N) -> {N, Value};
find_index(RoleId, [_ | L], N) -> 
    find_index(RoleId, L, N + 1).


%% 转换前端数据并且添加排名
to_p_rank_info([], _, List) -> List;
to_p_rank_info([#rank_role{id = Id, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = V1, sign = Sign} | L], Num, List) ->
    Data = #p_rank_info{num = Num, id = Id, role_id = RoleID, name = Name, icon = Icon, vip = Vip, value1 = V1, sign = Sign},
    to_p_rank_info(L, Num + 1, [Data | List]).


%% 清空一个排行
clean(Type) ->
    case catch ets:lookup(rank, Type) of
        [#rank{}] -> 
            ets:insert(rank, #rank{type = Type});
        _ -> 
            ok
    end.

%% 保存并且清空排行
save_and_clean(Type, Time) ->
    case catch ets:lookup(rank, Type) of
        [#rank{list = List, len = Len}] -> 
            ets:insert(rank_log, #rank_log{type = Type, list = List, time = Time, len = Len}),
            ets:insert(rank, #rank{type = Type});
        _ -> 
            ok
    end.

%% 后台查看排行榜信息
web_look(Type, Time) ->
    Zero = date:unixtime(zero),
    case Zero =:= Time of
        true -> 
            case catch ets:lookup(rank, Type) of
                [#rank{list = List}] -> 
                    [[{role_id, RoleId}, {name, Name}, {value, Value}]|| #rank_role{role_id = RoleId, name = Name, value1 = Value}<-List];
                _ -> 
                    []
            end;
        _ -> 
            case catch ets:match_object(rank_log, #rank_log{type = Type, time = Time, _ = '_'}) of
                [#rank_log{list = List}] -> 
                    [[{role_id, RoleId}, {name, Name}, {value, Value}]|| #rank_role{role_id = RoleId, name = Name, value1 = Value}<-List];
                _ -> 
                    []
            end
    end.


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    State = #state{},
    erlang:send_after(date:next_diff(23, 0, 0) * 1000, self(), stop_calc),
    erlang:send_after(date:next_diff(23, 59, 59) * 1000, self(), reward),
    ets:new(rank, [set, named_table, public, {read_concurrency, true}, {keypos, #rank.type}]),
    dets:open_file(rank, [{file, "./dets/rank.dets"},  {keypos, #rank.type}, {type, set}]),
    ets:new(rank_log, [duplicate_bag, named_table, public, {read_concurrency, true}, {keypos, #rank_log.type}]),
    dets:open_file(rank_log, [{file, "./dets/rank_log.dets"},  {keypos, #rank_log.type}, {type, duplicate_bag}]),
    ets:from_dets(rank, rank),
    ets:from_dets(rank_log, rank_log),
    self() ! init,
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    start_rank_zone(5),
    {noreply, State};

%% 停止数据计算
handle_info(stop_calc, State) ->
    erlang:send_after(date:next_diff(23, 0, 0) * 1000, self(), stop_calc),
    do_stop_calc([]),
    {noreply, State};

%% 发完奖励重新开始数据计算
handle_info(reward, State) ->
    Time = date:unixtime(zero),
    do_reward([], Time),
    erlang:send_after(date:next_diff(23, 59, 59) * 1000, self(), reward),
    timer:sleep(1000),
    do_start_calc([]),
    {noreply, State};

%% 目前没有需求保存的暂时不做保存
handle_info({save, Type, _Time}, State) ->
    clean(Type),
    {noreply, State};

handle_info({'EXIT', Pid, _}, State = #state{}) ->
    ?ERR("Pid:~p", [Pid]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("排行进程关闭......"),
    ets:to_dets(rank, rank),
    ets:to_dets(rank_log, rank_log),
    ?INFO("排行进程关闭完成"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% 启动排行榜运算进程
start_rank_zone(0) -> ok;
start_rank_zone(N) ->
    rank_zone:start_link(N),
    start_rank_zone(N - 1).


%% 通知计算进程停止接收数据
do_stop_calc([]) -> ok;
do_stop_calc([Type | L]) ->
    #rank_config{zone = Zone} = rank:get_rank_config(Type),
    Zone ! stop_calc,
    do_stop_calc(L).

%% 通知计算进程开始接收数据
do_start_calc([]) -> ok;
do_start_calc([Type | L]) ->
    #rank_config{zone = Zone} = rank:get_rank_config(Type),
    Zone ! start_calc,
    do_start_calc(L).

%% 发奖励
do_reward([], _) -> ok;
do_reward([_Type | L], Time) ->
    do_reward(L, Time).





