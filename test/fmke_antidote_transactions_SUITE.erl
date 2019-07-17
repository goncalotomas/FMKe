%%%-------------------------------------------------------------------
%%% File    : fmke_antidote_transactions_SUITE.erl
%%% Author  : Gonçalo Tomás
%%% Description : Tests the behaviour of AntidoteDB transactions.
%%% Created : Mon 1 Oct 2018 17:42
%%%-------------------------------------------------------------------
-module(fmke_antidote_transactions_SUITE).
-include("fmke.hrl").

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (NODENAME, 'fmke@127.0.0.1').
-define (COOKIE, fmke).

suite() ->
    [{timetrap, {minutes, 5}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    TestNode = 'fmke_antidote_ct@127.0.0.1',
    ok = fmke_test_setup:ensure_start_dist_node(TestNode),
    true = erlang:set_cookie(TestNode, ?COOKIE),
    fmke_test_setup:start_node_with_antidote_backend(?NODENAME),
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
all() -> [read_read_succeds, read_write_succeeds, write_write_aborts].

read_read_succeds(_Config) ->
    Key = list_to_binary(rand_str:get(64)),
    %% add key to antidote
    Pid = checkout_remote_pid(),
    {ok, Txn} = antidotec_pb:start_transaction(Pid, ignore),
    BoundObject = {Key, antidote_crdt_counter_pn, <<"bucket">>},
    Obj = antidotec_counter:increment(1, antidotec_counter:new()),
    ok = antidotec_pb:update_objects(Pid, antidotec_counter:to_ops(BoundObject, Obj), Txn),
    {ok, _} = antidotec_pb:commit_transaction(Pid, Txn),
    Pid1 = checkout_remote_pid(),
    Pid2 = checkout_remote_pid(),
    {ok, Txn1} = antidotec_pb:start_transaction(Pid1, ignore),
    {ok, Txn2} = antidotec_pb:start_transaction(Pid2, ignore),
    {ok, [Val1]} = antidotec_pb:read_objects(Pid1, [BoundObject], Txn1),
    {ok, [Val2]} = antidotec_pb:read_objects(Pid2, [BoundObject], Txn2),
    {ok, _} = antidotec_pb:commit_transaction(Pid1, Txn1),
    {ok, _} = antidotec_pb:commit_transaction(Pid2, Txn2),
    Value1 = antidotec_counter:value(Val1),
    Value2 = antidotec_counter:value(Val2),
    checkin_remote_pid(Pid),
    checkin_remote_pid(Pid1),
    checkin_remote_pid(Pid2),
    ?assertEqual(Value1, Value2).

read_write_succeeds(_Config) ->
    Key = list_to_binary(rand_str:get(64)),
    %% add key to antidote
    Pid = checkout_remote_pid(),
    {ok, Txn} = antidotec_pb:start_transaction(Pid, ignore),
    BoundObject = {Key, antidote_crdt_counter_pn, <<"bucket">>},
    Obj = antidotec_counter:increment(1, antidotec_counter:new()),
    ok = antidotec_pb:update_objects(Pid, antidotec_counter:to_ops(BoundObject, Obj), Txn),
    {ok, _} = antidotec_pb:commit_transaction(Pid, Txn),
    Pid1 = checkout_remote_pid(),
    Pid2 = checkout_remote_pid(),
    {ok, Txn1} = antidotec_pb:start_transaction(Pid1, ignore),
    {ok, Txn2} = antidotec_pb:start_transaction(Pid2, ignore),
    {ok, [_Val1]} = antidotec_pb:read_objects(Pid1, [BoundObject], Txn1),
    ObjUpdate = antidotec_counter:increment(1, Obj),
    ok = antidotec_pb:update_objects(Pid2, antidotec_counter:to_ops(BoundObject, ObjUpdate), Txn2),
    {ok, _} = antidotec_pb:commit_transaction(Pid1, Txn1),
    {ok, _} = antidotec_pb:commit_transaction(Pid2, Txn2),
    checkin_remote_pid(Pid),
    checkin_remote_pid(Pid1),
    checkin_remote_pid(Pid2),
    ok.

write_write_aborts(_Config) ->
    Key = list_to_binary(rand_str:get(64)),
    %% add key to antidote
    Pid = checkout_remote_pid(),
    {ok, Txn} = antidotec_pb:start_transaction(Pid, ignore),
    BoundObject = {Key, antidote_crdt_counter_pn, <<"bucket">>},
    Obj = antidotec_counter:increment(1, antidotec_counter:new()),
    ok = antidotec_pb:update_objects(Pid, antidotec_counter:to_ops(BoundObject, Obj), Txn),
    {ok, _} = antidotec_pb:commit_transaction(Pid, Txn),
    Pid1 = checkout_remote_pid(),
    Pid2 = checkout_remote_pid(),
    {ok, Txn1} = antidotec_pb:start_transaction(Pid1, ignore),
    {ok, Txn2} = antidotec_pb:start_transaction(Pid2, ignore),
    ObjUpdate1 = antidotec_counter:increment(1, Obj),
    ObjUpdate2 = antidotec_counter:increment(2, Obj),
    ok = antidotec_pb:update_objects(Pid2, antidotec_counter:to_ops(BoundObject, ObjUpdate2), Txn2),
    ok = antidotec_pb:update_objects(Pid1, antidotec_counter:to_ops(BoundObject, ObjUpdate1), Txn1),
    %% check if both transactions committed successfully
    case {antidotec_pb:commit_transaction(Pid1, Txn1), antidotec_pb:commit_transaction(Pid2, Txn2)} of
        {{error, _}, {error, _}} ->
            %% both failed
            ok;
        {{ok, _}, {error, _}} ->
            %% one of them failed, the other succeeded
            ok;
        {{error, _}, {ok, _}} ->
            %% one of them failed, the other succeeded
            ok;
        {{ok, _}, {ok, _}} ->
            %% both transactions succeeded in an update to the same key at the same time
            throw("write_write_transaction_succeded")
    end,
    checkin_remote_pid(Pid),
    checkin_remote_pid(Pid1),
    checkin_remote_pid(Pid2),
    ok.

checkin_remote_pid(Pid) ->
    rpc(fmke_db_conn_manager, checkout, [Pid]).

checkout_remote_pid() ->
    rpc(fmke_db_conn_manager, checkout, []).

rpc(Mod, Fun, Args) ->
    rpc:call(?NODENAME, Mod, Fun, Args).
