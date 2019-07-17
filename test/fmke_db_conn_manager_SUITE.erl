%%%-------------------------------------------------------------------
%%% File    : fmke_db_conn_manager_SUITE.erl
%%% Author  : Gonçalo Tomás
%%% Description : Tests the behaviour of the DB connection manager
%%%               under several scenarios.
%%% Created : Fri 9 Feb 2018 17:42
%%%-------------------------------------------------------------------
-module(fmke_db_conn_manager_SUITE).
-include("fmke.hrl").

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define (NODENAME, 'fmke@127.0.0.1').
-define (COOKIE, fmke).

suite() ->
    [{timetrap, {minutes, 3}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    TestNode = 'fmke_db_conn_mgr_test@127.0.0.1',
    ok = fmke_test_setup:ensure_start_dist_node(TestNode),
    true = erlang:set_cookie(TestNode, ?COOKIE),
    fmke_test_setup:start_node_with_mock_cluster(?NODENAME),
    true = erlang:set_cookie(?NODENAME, ?COOKIE),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    fmke_test_setup:stop_node(?NODENAME),
    fmke_test_setup:stop_all(),
    net_kernel:stop(),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        get_pool_names,
        round_robin_policy,
        no_overflow_when_using_conn_pool_size_pids,
        checkin_unknown_pid_is_recognized_by_manager,
        dead_pid_is_cleaned_from_manager_state
    ].

get_pool_names(_Config) ->
    {ok, Pools} = rpc(application, get_env, [?APP, pools]),
    [pool_127_0_0_1_8087, pool_localhost_8087] = Pools.

round_robin_policy(_Config) ->
    Pids = checkout_multiple(6),
    [P1, P2, P3, P4, P5, P6] = Pids,
    {ok, Pools} = get_pools(),
    [Pool1Pids, Pool2Pids] = lists:map(fun get_pids/1, Pools),
    true = lists:member(P1, Pool1Pids),
    true = lists:member(P2, Pool2Pids),
    true = lists:member(P3, Pool1Pids),
    true = lists:member(P4, Pool2Pids),
    true = lists:member(P5, Pool1Pids),
    true = lists:member(P6, Pool2Pids),
    checkin_multiple(Pids),
    ok.

no_overflow_when_using_conn_pool_size_pids(_Config) ->
    ConnPoolSize = get_pool_size(),
    true = ConnPoolSize > 1,
    Pids = checkout_multiple(ConnPoolSize + 1),
    {ok, [Pool1, Pool2]} = get_pools(),
    TwiceConnPoolSize = 2 * ConnPoolSize,
    {ready, Pool1CurrSize, CurrOverflow1, _Monitors1} = get_pool_state(Pool1),
    {ready, Pool2CurrSize, CurrOverflow2, _Monitors2} = get_pool_state(Pool2),
    TwiceConnPoolSize = length(Pids) + Pool1CurrSize + Pool2CurrSize,
    0 = CurrOverflow1,
    0 = CurrOverflow2,
    checkin_multiple(Pids),
    ok.

checkin_unknown_pid_is_recognized_by_manager(_Config) ->
    no_such_pid = checkin(self()).

dead_pid_is_cleaned_from_manager_state(_Config) ->
    Pid = checkout(),
    rpc(erlang, send, [fmke_db_conn_manager, {'EXIT', Pid, died}]),
    true = (undefined =/= rpc(erlang, process_info, [Pid])),
    timer:sleep(500),
    no_such_pid = checkin(Pid),
    true = rpc(erlang, exit, [Pid, die]),
    undefined = rpc(erlang, process_info, [Pid]).

get_pids(Pool) ->
    Results = rpc(gen_server, call, [Pool, get_all_workers]),
    lists:map(fun({_Monitors, Pid, _Type, _Module}) -> Pid end, Results).

get_pools() ->
    rpc:call(?NODENAME, application, get_env, [?APP, pools]).

get_pool_state(Pool) ->
    rpc(gen_server, call, [Pool, status]).

get_pool_size() ->
    {ok, ConnPoolSize} = rpc(application, get_env, [?APP, connection_pool_size]),
    ConnPoolSize.

checkin(Pid) ->
    rpc:call(?NODENAME, fmke_db_conn_manager, checkin, [Pid]).

checkin_multiple(Pids) ->
    lists:map(fun(Pid) -> rpc:call(?NODENAME, fmke_db_conn_manager, checkin, [Pid]) end, Pids).

checkout() ->
    rpc(fmke_db_conn_manager, checkout, []).

checkout_multiple(N) ->
    lists:map(fun(_N) -> rpc:call(?NODENAME, fmke_db_conn_manager, checkout, []) end, lists:seq(1, N)).

rpc(Mod, Fun, Args) ->
    rpc:call(?NODENAME, Mod, Fun, Args).
