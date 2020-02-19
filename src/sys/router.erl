%%----------------------------------------------------
%% @doc 路由处理
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(router).
-export([handle/4]).

-include("conn.hrl").
-include("common.hrl").

handle(Cmd, Data, Flag, State = #conn{pid_object = Pid}) ->
    case Cmd div 100 of
        10 -> 
            login_rpc:handle(Cmd, Data, State);
        _ -> 
            Mod = get_mod(Cmd),
            role:handle_rpc(Pid, Mod, Cmd, Data, Flag),
            ok
    end.



%% 获取协议号对应的模块
get_mod(Cmd) ->
    case Cmd div 100 of
        11 ->
            role_rpc;
        12 ->
            rank_rpc
        
    end.


