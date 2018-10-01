-module(fmke_test_setup).
-include_lib("common_test/include/ct.hrl").

-include ("fmke.hrl").

-export ([
    start_antidote/0,
    start_riak/0,
    start_redis/0,
    start_node_with_antidote_backend/3,
    start_node_with_ets_backend/2,
    start_node_with_redis_backend/3,
    start_node_with_riak_backend/3,
    start_node_with_mock_cluster/3,
    stop_all/0,
    stop_antidote/0,
    stop_riak/0,
    stop_redis/0,
    stop_node/1
]).

-define(ANTIDOTE_PORT, 8087).
-define(RIAK_PORT, 8087).
-define(REDIS_PORTS, [7000, 7001, 7002, 7003, 7004, 7005]).

start_antidote() ->
    Result = os:cmd("docker run -d --name antidote -e NODE_NAME=antidote@127.0.0.1 "
                    "-p \"4368:4368\" -p \"8085:8085\" -p \"8087:8087\" -p \"8099:8099\" -p \"9100:9100\" "
                    "mweber/antidotedb"),
    io:format("Starting antidote...~n~p~n", [Result]),
    timer:sleep(5000).

stop_antidote() ->
    os:cmd("docker stop antidote && docker rm antidote").

start_riak() ->
    Result = os:cmd("docker run -d --name riak -p \"8087:8087\" -p \"8098:8098\" "
                    "-e NODE_NAME=riak@127.0.0.1 goncalotomas/riak"),
    io:format("Starting riak...~n~p~n", [Result]),
    timer:sleep(30000).

stop_riak() ->
    os:cmd("docker stop riak && docker rm riak").

start_redis() ->
    Result = os:cmd("docker run -d --name redis -e CLUSTER_ONLY=true -e IP=0.0.0.0 -p \"7000:7000\" -p \"7001:7001\""
    " -p \"7002:7002\" -p \"7003:7003\" -p \"7004:7004\" -p \"7005:7005\" grokzen/redis-cluster:latest"),
    io:format("Starting redis...~n~p~n", [Result]),
    timer:sleep(10000).

stop_redis() ->
    os:cmd("docker stop redis && docker rm redis").

stop_all() ->
    Result = os:cmd("docker stop $(docker ps -aq) && docker rm $(docker ps -aq)"),
    io:format("Stopping all containers...~n~p~n", [Result]).

start_node_with_antidote_backend(Name, Optimized, DataModel) ->
    fmke_test_setup:start_antidote(),
    start_node(Name, [{optimized_driver, Optimized}, {data_model, DataModel}, {target_database, antidote},
                      {database_ports, [?ANTIDOTE_PORT]}]).

start_node_with_ets_backend(Name, DataModel) ->
    start_node(Name, [{data_model, DataModel}, {target_database, ets}]).

start_node_with_riak_backend(Name, Optimized, DataModel) ->
    fmke_test_setup:start_riak(),
    start_node(Name, [{optimized_driver, Optimized}, {data_model, DataModel}, {target_database, riak},
                      {database_ports, [?RIAK_PORT]}]).

start_node_with_redis_backend(Name, Optimized, DataModel) ->
    fmke_test_setup:start_redis(),
    start_node(Name, [{optimized_driver, Optimized}, {data_model, DataModel}, {target_database, redis},
                      {database_ports, ?REDIS_PORTS}]).

start_node_with_mock_cluster(Name, Optimized, DataModel) ->
    fmke_test_setup:start_antidote(),
    %% Uses two different loopback addresses to create pools (one IPv4, one IPv6)
    start_node(Name, [{optimized_driver, Optimized}, {data_model, DataModel}, {target_database, antidote},
                      {database_addresses, ["127.0.0.1", "localhost"]}, {database_ports, [?ANTIDOTE_PORT]}]).

start_node(Name, Opts) ->
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    %% have the slave nodes monitor the runner node, so they can't outlive it
    NodeConfig = [{monitor_master, true}, {kill_if_fail, true}, {boot_timeout, 5}, {init_timeout, 3},
        {startup_timeout, 3}, {startup_functions, [{code, set_path, [CodePath]}]}],
    %% start ct_slave node
    case ct_slave:start(Name, NodeConfig) of
        {ok, Node} ->
            StartupOpts = lists:ukeymerge(1, lists:sort(Opts), lists:sort(maps:to_list(?DEFAULTS))),
            lists:map(
                fun({Opt, Val}) ->
                    rpc:call(Node, application, set_env, [?APP, Opt, Val])
                end, StartupOpts),
            {target_database, Database} = lists:keyfind(target_database, 1, StartupOpts),
            ok = load_client_lib(Node, Database),
            %% start the application remotely
            {ok, _} = rpc:call(Node, application, ensure_all_started, [?APP]),
            io:format("Node ~p started", [Node]),
            Node;
        {error, Reason, Node} ->
            io:format("Error starting node ~p (~p), retrying...", [Node, Reason]),
            ct_slave:stop(Name),
            wait_until_offline(Node),
            start_node(Name, Opts)
    end.

wait_until_offline(Node) ->
    wait_until(fun() -> pang == net_adm:ping(Node) end, 60*2, 500).

wait_until(Fun, Retry, Delay) when Retry > 0 ->
    wait_until_result(Fun, true, Retry, Delay).

wait_until_result(Fun, Result, Retry, Delay) when Retry > 0 ->
    Res = Fun(),
    case Res of
        Result ->
            ok;
        _ when Retry == 1 ->
            {fail, Res};
        _ ->
            timer:sleep(Delay),
            wait_until_result(Fun, Result, Retry-1, Delay)
end.

stop_node(Node) ->
    rpc:call(Node, application, stop, [?APP]),
    ct_slave:stop(Node),
    wait_until_offline(Node).


load_client_lib(Node, antidote) ->
    rpc:call(Node, application, load, [antidotec_pb]);
load_client_lib(_Node, ets) ->
    ok;
load_client_lib(Node, redis) ->
    rpc:call(Node, application, load, [eredis]),
    rpc:call(Node, application, load, [eredis_cluster]);
load_client_lib(Node, riak) ->
    rpc:call(Node, application, load, [riak_pb]);
load_client_lib(Node, riak_kv) ->
    rpc:call(Node, application, load, [riak_pb]).
