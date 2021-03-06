%%----------------------------------------------------
%% 本服日志处理进程
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(log_db).
-behaviour(gen_server).
-export([
        start_link/1
        ,log/3
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        list = []  %% {Type, Pool, values}
    }).

-define(max_num, 500).  %% 单次处理最大数据量
-define(time_tick, 5000). %% 没5秒处理一次数据

-include("common.hrl").

start_link(N) ->
    gen_server:start_link(?MODULE, [N], []).

%% 类型原子，Pool 暂时不做处理，数据列表[]
log(Type, Pool, Data) ->
    Data1 = formart_insert_values(Data),
    case Type of
        role ->
            db_log_1 ! {log, {Type, Pool, Data1}};
        gold_cost_log ->
            db_log_2 ! {log, {Type, Pool, Data1}};
        _ ->
            db_log_3 ! {log, {Type, Pool, Data1}}

    end.


init([N]) ->
    process_flag(trap_exit, true),
    Name = list_to_atom(lists:concat(["db_log_", N])),
    register(Name, self()),
    put(self_name, Name),
    State = #state{},
    ?INFO("[~w] 已经启动", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = #state{list = List}) ->
    NewList = do_db(?max_num, List, []),
    case List of
        [] -> ok;
        _ ->
            erlang:send_after(?time_tick, self(), tick)
    end,
    {noreply, State#state{list = NewList}};

handle_info({log, Log}, State = #state{list = List}) ->
    case List of
        [] -> 
            erlang:send_after(?time_tick, self(), tick);
        _ -> ok
    end,
    {noreply, State#state{list = [Log | List]}};

handle_info(collect, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{list = List}) ->
    do_db(-1, List, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_db(_N, [], List) -> 
    insert_db(List),
    [];
do_db(0, L, List) -> 
    insert_db(List),
    L;
do_db(N, [{Type, Pool, Sql}| L], List) -> 
    NewList = case lists:keyfind(Type, 1, List) of
        {Type, Pool, L1} ->
            lists:keyreplace(Type, 1, List, {Type, Pool, [Sql | L1]});
        _ -> 
            [{Type, Pool, [Sql]} | List]
    end,
    do_db(N -1, L, NewList).


insert_db([]) -> ok;
insert_db([{Type, Pool, Values} | L]) ->
%%    ?ERROR_MSG("数据库入库类型:~w,长度:~w", [Type,length(Values)]),
    Sql = sql(Type),
    insert_db(Sql, Pool, Values),
    insert_db(L).

insert_db(Sql, _Pool, Values) ->
    List = string:join(Values, ","),
    Sql1 = util:flist(Sql, [List]),
    ?INFO("~w~n",[Sql1]),
    ?ERR("Sql:~ts", [list_to_binary(Sql1)]),
    case db:exec(list_to_binary(Sql1)) of
        ok -> ok;
        {error,{'EXIT',{mysql_timeout,5000,{}}}} -> ok;
        _Err ->
            ?ERR("mysql sql error:~ts:~w~n", [Sql1, _Err])
    end.


%% 插入数据值转换
formart_insert_values(List) ->
    db:insert_vals_sql(List).




sql(role) -> "replace into role (role_id, openid, icon, name, register_time, gold, diamond, login_time, off_time,  info) values  ~ts";
sql(gold_cost_log) -> "insert into gold_cost_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(diamond_cost_log) -> "insert into diamond_cost_log (role_id, type, value_befor, cost_value, value_after, time) values ~ts";
sql(client_error_log) -> "insert into client_error_log (role_id, name, msg, time) values ~ts".

