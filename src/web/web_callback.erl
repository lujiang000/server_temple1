%%----------------------------------------------------
%% @doc 后台处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_callback).
-export([
        do_rpc/1
        ,do_rpc/2

    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include_lib("xmerl/include/xmerl.hrl").

do_rpc(Data) ->
    {_, Mod1} = lists:keyfind("mod", 1, Data),
    {_, Function1} = lists:keyfind("function", 1, Data),
    Mod = erlang:list_to_atom(Mod1),
    Function = erlang:list_to_atom(Function1),
    NewArgs = case lists:keyfind("args", 1, Data) of
        {_, Args} ->
            L =
                case is_binary(Args) of
                    true ->
                        json:decode(Args, [{object_format, proplist}]);
                    _ ->
                        json:decode(erlang:list_to_binary(Args), [{object_format, proplist}])
                end,
            lists:map(fun({_, Val}) ->
                case catch erlang:binary_to_integer(Val) of
                    R when is_integer(R) ->
                        R;
                    _ ->
                        case is_integer(Val) of
                            true ->
                                Val;
                            _ ->
                                erlang:binary_to_list(Val)
                        end
                end end, L);
        _ ->
            []
    end,
     ?ERR("Mod:~w, Function:~w, Args:~w", [Mod, Function, NewArgs]),
    case catch erlang:apply(Mod, Function, NewArgs) of
        true ->
            true;
        ok ->
            true;
        false ->
            false;
        {ok, Value} ->
            to_json([{value, Value}]);
        {false, _Reson} ->
            false;
        <<"fail">> ->
            false;
        <<"success">> ->
            true;
        List when is_list(List) ->
            to_json(List);
        R ->
            R
    end.

to_json(List) -> json:encode(List).


%% 解析数据
do_rpc(Type, Data) ->
   List = json:decode(Data, [{object_format, proplist}]).

