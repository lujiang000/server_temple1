%%----------------------------------------------------
%% 人物进程相关处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role).
-behaviour(gen_server).
-export([start/1
        ,apply/3
        ,handle_rpc/5
        ,do_zero_flush/1
        ,do_change/3
        ,handle_web_rpc/4
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").
-include("rank.hrl").

%% 创建角色
start(Role = #role{}) ->
    gen_server:start(?MODULE, [Role], []).

apply(async, RolePid, {F}) ->
    RolePid ! {apply_async, {F}},
    ok;
apply(async, RolePid, {F, A}) ->
    RolePid ! {apply_async, {F, A}},
    ok;
apply(async, RolePid, {M, F, A}) ->
    RolePid ! {apply_async, {M, F, A}},
    ok;
apply(sync, RolePid, Mfa) when self() =:= RolePid ->
    {M, F, _, _} = hd(tl(util:get_stacktrace())),
    ?ERR("调用者[~w:~w]执行apply[~w]时发生错误：调用了自身", [M, F, Mfa]),
    {error, self_call};
apply(sync, RolePid, {F}) ->
    gen_server:call(RolePid, {apply_sync, F});
apply(sync, RolePid, {F, A}) ->
    gen_server:call(RolePid, {apply_sync, {F, A}});
apply(sync, RolePid, {M, F, A}) ->
    gen_server:call(RolePid, {apply_sync, {M, F, A}}).


%% 玩家协议调用
handle_rpc(Pid, Mod, Cmd, Data, Flag) ->
    catch Pid ! {rpc, Mod, Cmd, Data, Flag}.
%% 玩家协议调用
handle_web_rpc(Pid, Mod, Cmd, Data) ->
    catch gen_server:call(Pid, {web_rpc, Mod, Cmd, Data}).



init([Role = #role{socket_pid = SocketPid, role_id = RoleID}]) ->
    process_flag(trap_exit, true),
    put(socket_pid, SocketPid),
    link(SocketPid),
    SocketPid ! {save_role_pid, self(), RoleID},
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), zero_flush),
    NewRole = role_login:do(Role#role{pid = self()}),
    self() ! init,
    {ok, NewRole}.

%% 执行同步apply操作
handle_call({apply_sync, {F}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_call({apply_sync, {F, A}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_call({apply_sync, {M, F, A}}, _From, Role) ->
    handle_apply_sync_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);


handle_call(get_role, _From, Role) ->
    {reply, {ok, Role}, Role};

%% 短链接
handle_call({web_rpc, Mod, Cmd, Data}, _From, Role = #role{}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    Ref1 = erlang:send_after(600000, self(), stop),
    put(stop, Ref1),
    case Mod:handle(Cmd, Data, Role) of
        ok ->    
            {reply, ok, Role};
        {ok, NewRole} ->
            do_change(Cmd, NewRole, Role),
            {reply, ok, NewRole#role{need_sync = true}};
        {ok, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            {reply, {ok, Reply}, NewRole#role{need_sync = true}};
        {reply, Reply} ->
            {reply, {ok, Reply}, Role};
        {false, Reply} ->
            {reply, {false, Reply}, Role};
        {false, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            {reply, {false, Reply}, NewRole#role{need_sync = true}}
    end;

%% 断线重连
handle_call({reconnect, Pid, Ip}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    OldPid ! timeout,
    put(socket_pid, Pid),
    link(Pid),
    Pid ! {save_role_pid, self(), RoleID},
    NewRole = Role#role{socket_pid = Pid, ip = Ip},
    role_data:update_online_role(NewRole),
    {reply, role_conver:to_login_role(NewRole), NewRole};

%% 断线重连
handle_call({reconnect, Pid, Ip, ParentId}, _From, Role = #role{socket_pid = OldPid, role_id = RoleID}) ->
    case get(stop) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,
    sys_conn:pack_send(OldPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    OldPid ! timeout,
    put(socket_pid, Pid),
    link(Pid),
    Pid ! {save_role_pid, self(), RoleID},
    NewRole = Role#role{socket_pid = Pid, ip = Ip, parent_id = ParentId},
    role_data:update_online_role(NewRole),
    {reply, role_conver:to_login_role(NewRole), NewRole};


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rpc, Mod, Cmd, Data, Flag}, Role = #role{socket_pid = Pid}) ->
    case Mod:handle(Cmd, Data, Role) of
        ok ->    
            {noreply, Role};
        {ok, NewRole} ->
            do_change(Cmd, NewRole, Role),
            {noreply, NewRole#role{need_sync = true}};
        {ok, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            sys_conn:pack_send(Pid, Cmd, Flag, Reply),
            {noreply, NewRole#role{need_sync = true}};
        {reply, Reply} ->
            sys_conn:pack_send(Pid, Cmd, Flag, Reply),
            {noreply, Role};
        {false, Reply} ->
            sys_conn:pack_send_error(Pid, Cmd, Flag, Reply),
            {noreply, Role};
        {false, Reply, NewRole} ->
            do_change(Cmd, NewRole, Role),
            sys_conn:pack_send_error(Pid, Cmd, Flag, Reply),
            {noreply, NewRole#role{need_sync = true}}
    end;


handle_info(zero_flush, Role) ->
    NewRole = do_zero_flush(Role),
    erlang:send_after(date:next_diff(0, 0, 0) * 1000, self(), zero_flush),
    {noreply, NewRole};

handle_info(init, Role) ->
%%    NewRole = role_login:do(Role),
    self() ! loop,
    {noreply, Role};

%% 连接器进程异常退,只处理玩家 延迟10分钟
handle_info({'EXIT', SocketPid, _Why}, Role = #role{socket_pid = SocketPid}) ->
    Ref = erlang:send_after(600000, self(), stop),
    put(stop, Ref),
    role_data:update_online_role(Role),
    {noreply, Role};

handle_info(stop, Role) ->
    {stop, normal, Role};

%% 执行异步apply操作
handle_info({apply_async, {F}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role]), {undefined, F, []}, Role);
handle_info({apply_async, {F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(F, [Role | A]), {undefined, F, A}, Role);
handle_info({apply_async, {M, F, A}}, Role) ->
    handle_apply_async_return(catch erlang:apply(M, F, [Role | A]), {M, F, A}, Role);

%% 内部循环
handle_info(loop, Role = #role{loop_counter = C, need_sync = Sync, socket_pid = SocketPid}) ->
    %% 约每隔180秒执行一次GC
    case C rem 18 =:= 0 of
        false -> ignore;
        true -> 
            garbage_collect()
    end,
    %% 约每隔60秒检查一次数据保存需求
    S = case C rem 6 =:= 0 andalso Sync of
        false -> Sync;
        true ->
            role_data:save(Role),
            false
    end,
      %% 约每隔2分种检查连接进程是否堆积过多的消息
    case C rem 12 =:= 0 of
        false -> ignore;
        true ->
            case erlang:is_process_alive(SocketPid) of
                true ->
                    case process_info(SocketPid, message_queue_len) of
                        {_, QueueLen} when QueueLen > 10000 ->
                            ?ERR("连接进程堆积消息过多，断开连接"),
                            erlang:exit(SocketPid, kill);
                        _ -> ignore
                    end;
                _ ->
                    ignore
            end
    end,
    %% 10秒后进行下次循环
    erlang:send_after(10000, self(), loop),
    {noreply, Role#role{loop_counter = C + 1, need_sync = S}};


handle_info(kill_role, Role = #role{socket_pid = SockPid}) ->
    sys_conn:pack_send(SockPid, 1099, #m_1099_toc{error_code = ?error_login_other}),
    {stop, normal, Role};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Role) ->
    role_out:do(Role),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 处理异步apply的返回值
handle_apply_async_return({ok, NewRole}, {M, F, _A}, Role) when is_record(NewRole, role) ->
    do_change({M, F}, NewRole, Role),
    {noreply, NewRole#role{need_sync = true}};
handle_apply_async_return(ok, _Mfa, Role) ->
    {noreply, Role};
handle_apply_async_return(NewRole, {M, F, _A}, Role) when is_record(NewRole, role) ->
    do_change({M, F}, NewRole, Role),
    {noreply, NewRole#role{need_sync = true}};
handle_apply_async_return(Else, {M, F, A}, Role) ->
    ?ERR("角色[~ts]执行{~w, ~w, ~w}时得到错误的返回值格式:~w", [Role#role.name, M, F, A, Else]),
    {noreply, Role}.

%% 处理同步apply的返回值
handle_apply_sync_return({ok, Reply, NewRole}, {M, F, _A}, Role) when is_record(NewRole, role) ->
    do_change({M, F}, NewRole, Role),
    {reply, Reply, NewRole#role{need_sync = true}};
handle_apply_sync_return({ok, Reply}, _Mfa, Role) ->
    {reply, Reply, Role};
handle_apply_sync_return(Else, {M, F, A}, Role) ->
    ?ERR("角色[~ts]同步执行{~w, ~w, ~w}时得到错误的返回值格式:~w", [Role#role.name, M, F, A, Else]),
    {reply, Else, Role}.



%%变化值统一处理
do_change(Cmd, NewRole, Role)-> 
    do_gold_change(Cmd, NewRole, Role),
    do_diamond_change(Cmd, NewRole, Role).

%% 金币发生变化
do_gold_change(Cmd, _NewRole = #role{role_id = RoleID, gold = Value1}, _Role = #role{gold = Value2}) when Value1 =/= Value2-> 
    do_gold_log(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_gold_change(_, _, _) -> ok.

%% 钻石发生变化
do_diamond_change(Cmd, _NewRole = #role{role_id = RoleID, diamond = Value1}, _Role = #role{diamond = Value2}) when Value1 =/= Value2-> 
    do_diamond_log(Cmd, RoleID, Value2, Value1 - Value2, Value1);
do_diamond_change(_, _, _) -> ok.


%% 金币日志处理
do_gold_log(Cmd, RoleId, Value1, Cost, Value2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(gold_cost_log, insert, [RoleId, Type, Value1, Cost, Value2, date:unixtime()]).

%% 钻石日志处理
do_diamond_log(Cmd, RoleId, Value1, Cost, Value2) ->
    Type = get_cmd_type(Cmd),
    log_db:log(diamond_cost_log, insert, [RoleId, Type, Value1, Cost, Value2, date:unixtime()]).


%% 根据cmd判断是什么类型的操作
get_cmd_type(Cmd) when is_integer(Cmd) -> Cmd;
get_cmd_type(Cmd) -> 
    ?ERR("未知的调用:~w", [Cmd]),
    0.


%% 0点更新
do_zero_flush(Role = #role{}) -> 
    Now = date:unixtime(),
    Role#role{daily_value = [], login_time = Now}.
  





