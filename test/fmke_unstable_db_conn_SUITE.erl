%%%-------------------------------------------------------------------
%%% File    : fmke_delayed_start_connection_test_SUITE.erl
%%% Author  : Gonçalo Tomás
%%% Description : Tests the behaviour of the DB connection manager
%%%               when FMKe experiments database system is ready.
%%%               Only makes sense to test against databases that use FMKe's
%%%               database connection manager (e.g. antidote, riak)
%%% Created : Wed 14 Nov 2018 02:03
%%%-------------------------------------------------------------------
-module(fmke_unstable_db_conn_SUITE).
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
    TestNode = 'fmke_db_conn_robustness_test@127.0.0.1',
    ok = fmke_test_setup:ensure_start_dist_node(TestNode),
    true = erlang:set_cookie(TestNode, ?COOKIE),
    Opts = [
      {database_addresses, ["127.0.0.1"]},
      {database_ports, [8087]},
      {target_database, riak},
      {connection_pool_size, 2},
      {http_port, 10008}
    ],
    Data = [
        {patients, [{1, "john smith", "somewhere in portugal"}]}
    ],
    Config ++ [{fmke_opts, Opts}, {data, Data}].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    fmke_test_setup:stop_node(?NODENAME),
    fmke_test_setup:stop_all(),
    net_kernel:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

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
        start_fmke_ok,
        check_status_ok,
        kill_database,
        check_that_database_is_down,
        check_fmke_down,
        restart_db_and_wait_for_it,
        recheck_status_ok
    ].

start_fmke_ok(Config) ->
    Opts = ?config(fmke_opts, Config),
    true = erlang:set_cookie(?NODENAME, ?COOKIE),
    fmke_test_setup:launch_fmke(?NODENAME, Opts),
    ok.

check_status_ok(Config) ->
    add_data(Config),
    Data = ?config(data, Config),
    [{patients, [{Id, Name, Address}]}] = Data,
    ExpectedPat = #patient{id = Id, name = Name, address = Address},
    RemotePat = rpc(fmke, get_patient_by_id, [Id]),
    true = fmke_test_utils:compare_patients(ExpectedPat, RemotePat),
    ok.

kill_database(_Config) ->
    fmke_test_setup:stop_riak(),
    ok.

check_that_database_is_down(Config) ->
    _FmkeOpts = ?config(fmke_opts, Config),
    {ok, Pools} = rpc(application, get_env, [?APP, pools]),
    lists:map(fun(Pool) ->
        {badrpc, {'EXIT', {timeout, _MoreInfo}}} = rpc(gen_server, call, [Pool, get_avail_workers])
    end, Pools),
    ok.

check_fmke_down(Config) ->
    Data = ?config(data, Config),
    [{patients, [{Id, _Name, _Address}]}] = Data,
    {badrpc, {'EXIT', {timeout, _MoreInfo}}} = rpc(fmke, get_patient_by_id, [Id]),
    ok.

restart_db_and_wait_for_it(Config) ->
    %% give some time for workers to generate more error messages in the FMKe node
    timer:sleep(3000),
    %% restart riak
    fmke_test_setup:start_riak(),
    add_data(Config),
    ok.

recheck_status_ok(Config) ->
    Data = ?config(data, Config),
    [{patients, [{Id, Name, Address}]}] = Data,
    ExpectedPat = #patient{id = Id, name = Name, address = Address},
    %% remember to sleep for the maximum amount between connection attempts (10s)
    timer:sleep(10000),
    RemotePat = rpc(fmke, get_patient_by_id, [Id]),
    true = fmke_test_utils:compare_patients(ExpectedPat, RemotePat),
    ok.

%% abstracts what data is supposed to be in the database. Since we essencially kill it and bring it back up,
%% no data persistence is provided, thus we need to add data again in the restart_db_and_wait_for_it test.
add_data(Config) ->
    Data = ?config(data, Config),
    [{patients, [{Id, Name, Address}]}] = Data,
    ok = rpc(fmke, create_patient, [Id, Name, Address]).

rpc(Mod, Fun, Args) ->
    rpc:call(?NODENAME, Mod, Fun, Args).
