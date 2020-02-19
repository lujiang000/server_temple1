%%----------------------------------------------------
%% @doc 人物协议处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_rpc).
-export([
        handle/3

    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").


%% 前端报错的错误日志
handle(1197, #m_1197_tos{msg = Msg}, #role{role_id = RoleId, name = Name}) ->
    case get(client_error_log) of
        Msg -> ok;
        _ ->
            put(client_error_log, Msg),
            log_db:log(client_error_log, insert, [RoleId, Name, Msg, date:unixtime()])
    end,
    {reply, #m_1197_toc{}};

handle(_Cmd, _Data, _Role) ->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    ok.
%%---------------------------------
%% internal function
%%---------------------------------

