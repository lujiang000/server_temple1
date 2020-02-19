%%----------------------------------------------------
%% @doc 排行榜协议处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(rank_rpc).
-export([
        handle/3

    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("rank.hrl").
-include("error_msg.hrl").


handle(1201, #m_1201_tos{start = Start, type = Type, num = Num, up_or_down = UpDown, date = Date}, Role = #role{}) ->
    {List, N, Value} = 
    case Date =:= 0 orelse Date =:= undefined of
        true -> rank_mgr:get_rank_info(Role, Start, Type, Num, UpDown);
        _ -> rank_mgr:get_old_rank_info(Role, Start, Type, Num, UpDown, Date)
    end,
    {reply, #m_1201_toc{list = List, num = N, value = Value}};


handle(_Cmd, _Data, _Role) ->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    ok.

