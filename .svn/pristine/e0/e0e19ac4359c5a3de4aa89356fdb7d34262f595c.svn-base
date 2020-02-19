%%----------------------------------------------------
%% @doc 所有回调接口
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web).
-export([
        handle/3
    ]).

-include("common.hrl").
-define(OK, <<"ok">>).
-define(SUCCESS, <<"success">>).
-define(FAIL, <<"fail">>).
-define(Error, <<"server error">>).
-define(true, <<"true">>).
-define(false, <<"false">>).


%% 所有回调接受信息并且返回
%% 后台 parse_qs() get请求  parse_post() post请求
handle("/web_rpc", Req, _Socket) ->
    Params = Req:parse_qs(),
    case catch web_callback:do_rpc(Params) of
        true ->
            success(Req, ?true);
        false ->
            send_error(Req, ?false);
        Data ->
            success(Req, Data)
    end;

handle("/rpc", Req, _) ->
  Params = Req:parse_post(),
  Data = json:encode([{result, success}]),
  ?ERR("接收到消息~w, 返回消息~w", [Params, Data]),
  success(Req, Data);

handle("/wft", Req, _) ->
  Params = Req:parse_post(),
  Reply = web_callback:wft(Params),
  success(Req, Reply);

handle("/yao_zfb", Req, _) ->
  Params = Req:parse_post(),
  Reply = web_callback:yao_zfb(Params),
  success(Req, Reply);

handle("/yb", Req, _) ->
  Params = Req:parse_post(),
  Reply = web_callback:yb_pay(Params),
  case Reply of
      <<"fail">> ->
          ?ERR("收到数据:~w", [Params]);
      _ ->
          ok
  end,
  success(Req, Reply);

handle("/再见", Req, _) ->
  _Params = Req:parse_post(),
  fail(Req, <<"你好">>);

%%---------------------------------------------------
%% 短链接  所有开放接口
%%---------------------------------------------------
%% 微信登陆
handle("/wx_login", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_login_rpc, wx_login, Params, Socket) of
      {ok, Data} -> 
          success(Req, json:encode([{status, 0}, {result, Data}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;
%% 账号登陆
handle("/account_login", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_login_rpc, account_login, Params, Socket) of
      {ok, Data} -> 
          success(Req, json:encode([{status, 0}, {result, Data}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          ?ERR("~w", [_ERR]),
          send_error(Req, "未知错误")
  end;
%% 密匙登陆
handle("/secret_login", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_login_rpc, secret_login, Params, Socket) of
      {ok, Data} -> 
          success(Req, json:encode([{status, 0}, {result, Data}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;
%% 保存数据
handle("/storage/set", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_role_rpc, set_data, Params, Socket) of
      ok ->
          success(Req, json:encode([{status, 0}]));
      {ok, Data} -> 
          success(Req, json:encode([{status, 0}, {result, Data}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;
%% 获取数据
handle("/storage/get", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_role_rpc, get_data, Params, Socket) of
      {ok, Data} -> 
          success(Req, json:encode([{status, 0}, {result, Data}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;

%% 上传排行数据
handle("/rank/set", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_role_rpc, set_rank, Params, Socket) of
      ok -> 
          success(Req, json:encode([{status, 0}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;

%% 获取排行数据
handle("/rank/get", Req, Socket) ->
  Params = Req:parse_post(),
  case catch web_rpc:handle(web_role_rpc, get_rank, Params, Socket) of
      {ok, Data} -> 
          success(Req, json:encode([{status, 0}, {result, Data}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;

%% 同步用户数据
handle("/sync_data", Req, Socket) ->
  Params = Req:parse_post(),
  %%?ERR("~w", [Params]),
  case catch web_rpc:handle(web_role_rpc, sync_data, Params, Socket) of
      ok -> 
          success(Req, json:encode([{status, 0}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;

%% 获取版本号
handle("/version", Req, _Socket) ->
    case sys_env:get_env(version) of
        List when is_list(List)->
          success(Req, json:encode([{status, 0}, {result, List}]));
      _ ->
          success(Req, json:encode([{status, 0}, {result, []}]))
    end;


%% 错误日志
handle("/error_log", Req, Socket) ->
  Params = Req:parse_post(),
  %%?ERR("~w", [Params]),
  case catch web_rpc:handle(web_role_rpc, error_log, Params, Socket) of
      ok -> 
          success(Req, json:encode([{status, 0}]));
      {false, Reason} ->
          Data = json:encode([{status, Reason}]),
          success(Req, Data);
      _ERR ->
          send_error(Req, "未知错误")
  end;



handle(Unknown, Req, _) ->
	Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s", [Unknown])}).


fail(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "charset=utf-8"},
    {"Content-Language", "en,zh"}], Body}).

send_error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body});
send_error(Req, Body) when is_list(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}],unicode:characters_to_binary(Body)}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).
