%%%-------------------------------------------------------------------
%%% @author weichengjun
%%% @copyright (C) 2019, <COMPANY>
%%% @doc  倒计时控制器 暂时用s级别的
%%%
%%% @end
%%% Created : 01. 三月 2019 23:29
%%%-------------------------------------------------------------------
-module(role_timer).
-author("weichengjun").

%% API
-export([
      add_timer/2
      ,login/1
      ,do_function/1
      ,cancel_timer/2
]).

-include("role.hrl").


%% 登陆处理过期的定时器，并且启动定时器
login(Role = #role{role_timer =  #role_timer{now = undefined}}) ->
  Role;
login(Role = #role{role_timer =  #role_timer{now = Now, list = List}}) ->
  NowTime = date:unixtime(),
  NewList = [Timer#timer{}|| Timer = #timer{end_time = Time}<-[Now | List], Time > NowTime],
  next_timer(Role#role{role_timer = #role_timer{list = NewList}}).







%% 当前没有定时器运行
add_timer(Role = #role{role_timer = RoleTimer = #role_timer{ref = undefined}}, Timer = #timer{}) ->
  next_timer(Role#role{role_timer = RoleTimer#role_timer{now = Timer}});
%% 增加的定时器和当前的定时器同id, 取消掉当前倒计时，然后计算最优先掉
add_timer(Role = #role{role_timer = #role_timer{ref = Ref, now = #timer{id = Id, end_time = EndTime}, list = List}}, Timer = #timer{id = Id, time = Time1}) ->
  erlang:cancel_timer(Ref),
  NewList = lists:keysort(#timer.end_time, [Timer#timer{end_time = EndTime + Time1} | List]),
  next_timer(Role#role{role_timer = #role_timer{list = NewList}});
%% 增加掉定时器比当前计时器还要早,取消掉当前倒计时
add_timer(Role = #role{role_timer = #role_timer{ref = Ref, now = Now = #timer{end_time = EndTime}, list = List}}, Timer = #timer{end_time = EndTime1}) when EndTime1 < EndTime ->
  erlang:cancel_timer(Ref),
  next_timer(Role#role{role_timer = #role_timer{now = Timer, list = [Now | List]}});
%% 其他情况往后增加计时器
add_timer(Role = #role{role_timer = #role_timer{list = List}}, Timer = #timer{}) ->
  NewList = lists:keysort(#timer.end_time, [Timer| List]),
  Role#role{role_timer = #role_timer{list = NewList}}.


%% 启动下一个定时器
next_timer(Role = #role{role_timer = #role_timer{ref = undefined, now = undefined, list = [Timer = #timer{end_time = Time} | List]}}) ->
    Now = date:unixtime(),
    NewRef = erlang:send_after(max(0, Time - Now) * 1000, self(), {apply_async, {?MODULE, do_function, []}}),
    Role#role{role_timer = #role_timer{ref = NewRef, now = Timer, list = List}};
next_timer(Role = #role{role_timer = Timer = #role_timer{ref = undefined, now = #timer{end_time = Time}}}) ->
    Now = date:unixtime(),
    NewRef = erlang:send_after(max(0, Time - Now) * 1000, self(), {apply_async, {?MODULE, do_function, []}}),
    Role#role{role_timer = Timer#role_timer{ref = NewRef}};
next_timer(Role) -> Role.

%% 取消计时器
cancel_timer(Role = #role{role_timer = RoleTimer = #role_timer{ref = Ref, now = #timer{id = Id}}}, Id) ->
  erlang:cancel_timer(Ref),
  next_timer(Role#role{role_timer = RoleTimer#role_timer{ref = undefined, now = undefined}});
cancel_timer(Role = #role{role_timer = RoleTimer = #role_timer{list = List}}, Id) ->
  NewList = lists:keydelete(Id, #timer.id, List),
  Role#role{role_timer = RoleTimer#role_timer{list = NewList}}.


%% 执行回调方法
do_function(Role = #role{role_timer = RoleTimer = #role_timer{now = #timer{mfa = {M, F, A}}}}) ->
    NewRole = erlang:apply(M, F, [Role | A]),
    NewRole1 = NewRole#role{role_timer = RoleTimer#role_timer{now = undefined, ref = undefined}},
    next_timer(NewRole1).
    






