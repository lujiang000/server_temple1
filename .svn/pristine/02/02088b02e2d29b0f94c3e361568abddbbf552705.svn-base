%%----------------------------------------------------
%% 人物数据管理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_data).
-behaviour(gen_server).
-export([start_link/0
        ,save/1
        ,save/2
        ,get_online_role/1
        ,new_to_db/1
        ,sync_out_role/1
        ,sync_online_role/1
        ,save_to_db/1
        ,get_role_log_name/1
        ,lookup_role/2
        ,get_role_from_dets/1
        ,delete_role/1
        ,update_role/3
        ,do_update_role/3
        ,get_online_num/0
        ,update_online_role/1
        ,get_role_dets_name/1
        ,get_all_online/0
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {num = 0
        ,near_login = []    %%近期登陆的玩家数据
    }).

-define(role_dets_num, 20).

-include("role.hrl").
-include("common.hrl").
-include("all_pb.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 从dets获取玩家信息
get_role_from_dets(RoleID) ->
    Dets = get_role_dets_name(RoleID),
    case dets:lookup(Dets, RoleID) of  %% 本服获取
        [Data] -> {ok, Data};
        [] -> 
            ?ERR("读取玩家[~w]数据为空", [RoleID]),
            [];
        _Err ->
            ?ERR("读取玩家[~w]数据出错:~w", [RoleID, _Err]),
            false
    end.



%% 玩家在线时保存到dets
save(Role) ->
    save(Role, local).
save(Role = #role{role_id = RoleID}, local) ->
    Dets = get_role_dets_name(RoleID),
    dets:insert(Dets, Role);

%% 保存到dets,并且保存到数据库，这里次数不要太频繁
save(Role, db) ->
    save_to_db(Role).

%% 查看在线玩家数据
get_online_role(RoleId) ->
    case ets:lookup(online_role, RoleId) of
        [Role] -> {ok, Role};
        _ -> false
    end.



%% 在线数据同步
sync_online_role(Role) ->
    OnlineRole = role_conver:to_online(Role),
    ?MODULE ! {add,OnlineRole},
    ets:insert(online_role, OnlineRole).

%% 更新在线数据
update_online_role(Role) ->
    OnlineRole = role_conver:to_online(Role),
    ets:insert(online_role, OnlineRole).

%% 玩家下线
sync_out_role(_Role = #role{role_id = RoleID}) ->
    ?MODULE ! delete,
    ets:delete(online_role, RoleID).

%% 获取在线玩家数量
get_online_num() ->
    gen_server:call(?MODULE, get_num).

%% 查看玩家单个字段的数据
lookup_role(RoleID, Type) ->
  L1 = record_info(fields, role),
  L2 =
  case ets:lookup(online_role, RoleID) of
      [#online_role{pid = Pid}] ->
        case catch gen_server:call(Pid, get_role) of
          {ok, Term} ->
            {ok, Role} = role_var:update_var(Term),
            [_|T] = tuple_to_list(Role),
            T;
          _ ->
            []
        end;
      _ ->
        case role_data:get_role_from_dets(RoleID) of
          {ok, Term} ->
            {ok, Role} = role_var:update_var(Term),
            [_|T] = tuple_to_list(Role),
            T;
          _ ->
            []
        end
    end,
  case catch lists:zip(L1, L2) of
    L when is_list(L) ->
      case is_list(Type) of
        true ->
          [lists:keyfind(Type1, 1, L) || Type1 <-Type];
        _ ->
          lists:keyfind(Type, 1, L)
      end;
    _ ->
      error
  end.

%% 更新玩家#role{}字段
update_role(RoleID, Key, Val) ->
    case ets:lookup(online_role, RoleID) of
        [#online_role{pid = Pid}] ->
      role:apply(async, Pid, {?MODULE, do_update_role, [Key, Val]}),
      case catch gen_server:call(Pid, get_role) of
        {ok, Role} ->
          do_update_role(Role, Key, Val);
        _ ->
          ok
      end;
    _ ->
      case role_data:get_role_from_dets(RoleID) of
        {ok, Role} ->
          {ok, NewRole} = do_update_role(Role, Key, Val),
          role_data:save(NewRole, db);
        _ ->
          ok
      end
  end,
  <<"ok">>.

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(online_role, [set, named_table, public, {keypos, #online_role.role_id}]),
    dets:open_file(near_login, [{file, "./dets/near_login.dets"}, {keypos, #online_role.role_id}, {type, set}]),
    open_dets(?role_dets_num),
    List = init_near_login(),
    State = #state{near_login = List},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(get_num, _From, State = #state{num = Num}) ->
    {reply, Num, State};

handle_call(get_near_login, _From, State = #state{near_login = List}) ->
    {reply, {ok, List}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add, OnlineRole = #online_role{role_id = RoleID}}, State = #state{num = Num, near_login = List}) ->
    NewList = lists:keydelete(RoleID, #online_role.role_id, List),
    NewList1 = lists:sublist([OnlineRole | NewList], 20),
    {noreply, State#state{num = Num + 1, near_login = NewList1}};

handle_info(delete, State = #state{num = Num}) ->
    {noreply, State#state{num = Num - 1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{near_login = List}) ->
    ?INFO("[~w] 正在关闭", [?MODULE]),
    close_dets(?role_dets_num),
    dets:delete_all_objects(near_login),
    dets:insert(near_login, List),
    dets:close(near_login),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 初始化最近登陆信息
init_near_login() ->
   dets:traverse(near_login, fun(R) -> {continue, R} end).

%% 初始化dets表
open_dets(0) -> ok;
open_dets(N) ->
    dets:open_file(list_to_atom(lists:concat(['role_', N - 1])), [{file, lists:concat(["./dets/role_",  N -1, ".dets"])}, {keypos, #role.role_id}, {type, set}]),
    open_dets(N - 1).

%% 关闭dets
close_dets(0) -> ok;
close_dets(N) ->
    dets:close(list_to_atom(lists:concat(['role_', N - 1]))),
    close_dets(N - 1).


%% 根据 人物id找到数据表名
get_role_log_name(RoleID) ->
    Num = RoleID rem 10,
    erlang:list_to_atom(lists:concat(["role_", Num])).



%% 第一次创建角色
new_to_db(Role = #role{role_id = RoleID, name = Name, icon = Icon, openid = OpenID, register_time = RegisterTime, gold = Gold, diamond = Diamond, login_time = LoginTime, off_time = OffTime}) ->
    case db:exec("insert into role (role_id, openid, icon, name, register_time, gold, diamond, login_time, off_time, info) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [RoleID, OpenID, Icon, Name, RegisterTime, Gold, Diamond,LoginTime, OffTime, util:term_to_string(Role)]) of
        ok ->
            save(Role),
            ok;
        {error, Err} ->
            ?ERR("[~ts]的角色数据存入数据库时发生异常: ~w", [Name, Err]),
            {error, Err}
    end.

%% 保存数据到数据库
save_to_db(Role = #role{role_id = RoleID, name = Name, icon = Icon,  openid = OpenID, register_time = RegisterTime, gold = Gold,  diamond = Diamond, login_time = LoginTime, off_time = OffTime}) ->
    Dets = get_role_dets_name(RoleID),
    dets:insert(Dets, Role),
    log_db:log(role, insert, [RoleID, OpenID, Icon, Name, RegisterTime, Gold, Diamond, LoginTime, OffTime, ""]).


get_role_dets_name(RoleID) ->
    Num = RoleID rem ?role_dets_num,
    erlang:list_to_atom(lists:concat(["role_", Num])).

%% 删除本地玩家信息
delete_role(RoleID) ->
    db:exec("delete from role where role_id = ?", [RoleID]),
    Dets = get_role_dets_name(RoleID),
    dets:delete(Dets, RoleID).

%% 测试使用 更新玩家字段
do_update_role(Role, Key1, Val) ->
  Key =
      case is_list(Key1) of
          true ->
              list_to_atom(Key1);
          _ ->
              Key1
      end,
  NewRole =
      case Key of
          icon ->
              Role#role{icon = Val};
          gold ->
              Role#role{gold = Val};
          name ->
              Role#role{name = Val};
          _ ->
              Role
      end,
  {ok, NewRole}.

%% 获取所有在线玩家数据, 后台调用
get_all_online() ->
    List = ets:tab2list(online_role),
    [[{role_id, RoleID}, {name, Name}, {gold, Gold}] || #online_role{role_id = RoleID, name = Name, gold = Gold}<-List].


