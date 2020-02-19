%%----------------------------------------------------
%% 游戏相关设置
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(setting_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,get/1
        ,set/2
        ,web_get/1
        ,web_set/2
        ,web_set/4
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("role.hrl").

-record(setting, {
        type = 0           %% 类型
        ,value = 0         %% 值
    }
).


%% 目前的设置

get(Type) ->
    case ets:lookup(setting, Type) of
        [#setting{value = Value}] -> {ok, Value};
        _ -> false
    end.

set(Type, Value) ->
    ets:insert(setting, #setting{type = Type, value = Value}).



web_set(Type, Value) ->
    ets:insert(setting, #setting{type = Type, value = Value}),
    true.

web_set(Type, Start, End, Num) ->
    ets:insert(setting, #setting{type = Type, value = {Start, End, Num}}),
    shop:reload(),
    true.

web_get(Type) ->
    case ets:lookup(setting, Type) of
        [#setting{type = Type, value = Value}] -> [{value, Value}];
        _ -> false
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(setting, [set, named_table, public, {read_concurrency, true}, {keypos, #setting.type}]),
    dets:open_file(setting, [{file, "./dets/setting.dets"}, {keypos, #setting.type}, {type, set}]),
    ets:from_dets(setting, setting),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    ets:to_dets(setting, setting),
    util:close_dets(setting),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
