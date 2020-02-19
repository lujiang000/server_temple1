%%----------------------------------------------------
%% 账目统计
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(account_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,charge/1
        ,pw/2
        ,help/1
        ,get_account/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        charge = 0    %% 充值
        ,p = 0        %% 投入
        ,w = 0        %% 产出
        ,help = 0     %% 助力产出
        ,help_num = 0 %% 助力次数
        ,time = 0     %% 时间
    }).

-include("common.hrl").
-include("role.hrl").


%% 充值
charge(Num) ->
    ?MODULE ! {charge, Num}.

%% pw统计
pw(P, W) ->
    ?MODULE ! {pw, P, W}.

%% 助力统计
help(Num) ->
    ?MODULE ! {help, Num}.

%% 后台调用
get_account() ->
    case catch gen_server:call(?MODULE, get_account) of
        {ok, Data} ->
            List1 = record_info(fields, state),
            [_ | List2] = erlang:tuple_to_list(Data),
            lists:zip(List1, List2);
        _ ->
            false
    end.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    State = do_init(),
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(get_account, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({charge, Value}, State = #state{charge = Charge}) ->
    {noreply, State#state{charge = Value + Charge}};

handle_info({pw, P, W}, State = #state{p = P1, w = W1}) ->
    {noreply, State#state{p = P + P1, w = W + W1}};

handle_info({help, Value}, State = #state{help = Help, help_num = Num}) ->
    {noreply, State#state{help = Help + Value, help_num = Num + 1}};

%% 0点入库
handle_info(next_day, State) ->
    save(State),
    erlang:send_after(date:next_diff(0, 0, 1) * 1000, self(), next_day),
    {noreply, #state{time = date:unixtime(zero)}};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?INFO("[~w] 正在关闭....", [?MODULE]),
    save(State),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_init() ->
    Now = date:unixtime(zero),
    case db:get_row("select * from account_log where time = ?", [Now]) of
        {ok, [_, Charge, P, W, Help, HelpNum, Time]}  ->
            #state{charge = Charge, p = P, w = W, help = Help, help_num = HelpNum, time = Time};
        _ ->
            #state{time = Now}
    end.


%% 入库
save(#state{charge = Charge, p = P, w = W, help = Help, help_num = HelpNum, time = Time}) ->
    case db:get_one("select id from account_log where time = ?", [Time]) of
        {ok, Id} when is_integer(Id) ->
            db:exec("replace into account_log(id, charge, p, w, help, help_num, time) values(?, ?, ?, ?, ?, ?, ?)", [Id, Charge, P, W, Help, HelpNum, Time]);
        _ ->
            db:exec("insert into account_log(charge, p, w, help, help_num, time) values(?, ?, ?, ?, ?, ?)", [Charge, P, W, Help, HelpNum, Time])
    end.

