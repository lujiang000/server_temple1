%%----------------------------------------------------
%% @doc 排行榜处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank).
-export([
        handle/2
        ,handle/3
        ,get_rank_config/1
    ]).

-include("common.hrl").
-include("role.hrl").
-include("rank.hrl").


%% 所有排行信息监听
handle(Type, Role) ->
    RankRole = to_rank_info(Type, Role),
    in_rank(Type, RankRole).

handle(Type, Role, Value) ->
    RankRole = to_rank_info(Type, Role, Value),
    in_rank(Type, RankRole).

% 进行上榜  玩家本身的数据
in_rank(Type, RankRole = #rank_role{id = Id, value1 = Value}) ->
    #rank_config{len = MaxLen, min = MinValue, zone = Zone} = get_rank_config(Type),
    case Value >= MinValue of
        true ->
            #rank{len = Len, last_val = Min, list = RankL} = rank_mgr:lookup(Type),
            case lists:keyfind(Id, #rank_role.id, RankL) of
                false when Value =< Min andalso Len >= MaxLen ->        %% 不在榜上
                    ok;
                _ ->
                    rank_zone:in_rank(Zone, Type, RankRole)
            end;
        _ ->
            exit_rank(Zone, Type, Id)
    end.

%%下榜
exit_rank(Zone, Type, Id) ->
    #rank{list = RankL} = rank_mgr:lookup(Type),
    case lists:keyfind(Id, #rank_role.id, RankL) of
        false -> ok;
        _ ->
            rank_zone:exit_rank(Zone, Type, Id)
    end.

%% 转换成排行数据
to_rank_info(_, #role{}) -> 
    #rank_role{}.

%% 转换成排行数据
to_rank_info(_, #role{role_id = RoleId, name = Name, icon = Icon}, Value) -> 
    #rank_role{id = RoleId, role_id = RoleId, name = Name, icon = Icon, value1 = Value}.

%% 获取对应的排行配置信息, 可以同一个进程处理几个榜，有需要发奖励的榜只能一个进程处理一个榜
get_rank_config(_) ->
    #rank_config{min = 5, len = 500}.








