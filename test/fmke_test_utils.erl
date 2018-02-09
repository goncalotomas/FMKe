-module(fmke_test_utils).
-include_lib("common_test/include/ct.hrl").

-include ("fmke.hrl").

-export ([
    start_antidote/0,
    start_riak/0,
    start_redis/0,
    start_node_with_antidote_backend/1,
    start_node_with_riak_backend/1,
    start_node_with_redis_backend/1,
    start_norm_node_with_antidote_backend/1,
    start_norm_node_with_riak_backend/1,
    start_node_with_mock_redis_cluster/1,
    stop_all/0,
    stop_antidote/0,
    stop_riak/0,
    stop_redis/0,
    stop_node/1
]).

-define (ANTIDOTE_PORT, 8087).
-define (RIAK_PORT, 8087).
-define (REDIS_PORT, 6379).

start_antidote() ->
    Result = os:cmd("docker run -d --name antidote -e NODE_NAME=antidote@127.0.0.1 "
           "-p \"4368:4368\" -p \"8085:8085\" -p \"8087:8087\" -p \"8099:8099\" -p \"9100:9100\" mweber/antidotedb"),
    io:format("Starting antidote...~n~p~n", [Result]),
    timer:sleep(5000).

stop_antidote() ->
    os:cmd("docker stop antidote && docker rm antidote").

start_riak() ->
    Result = os:cmd("docker run -d --name riak -p \"8087:8087\" -p \"8098:8098\" -e NODE_NAME=riak@127.0.0.1"
            " goncalotomas/riak"),
    io:format("Starting riak...~n~p~n", [Result]),
    timer:sleep(17500).

stop_riak() ->
    os:cmd("docker stop riak && docker rm riak").

start_redis() ->
    Result = os:cmd("docker run -d --name redis -p \"6379:6379\" redis"),
    io:format("Starting redis...~n~p~n", [Result]),
    timer:sleep(4000).

stop_redis() ->
    os:cmd("docker stop redis && docker rm redis").

stop_all() ->
    Result = os:cmd("docker stop $(docker ps -aq) && docker rm $(docker ps -aq)"),
    io:format("Stopping all containers...~n~p~n", [Result]).

start_norm_node_with_antidote_backend(Name) ->
    fmke_test_utils:start_antidote(),
    start_local_node(Name, antidote_norm, ?ANTIDOTE_PORT).

start_norm_node_with_riak_backend(Name) ->
    fmke_test_utils:start_riak(),
    start_local_node(Name, riak_norm, ?RIAK_PORT).

start_node_with_antidote_backend(Name) ->
    fmke_test_utils:start_antidote(),
    start_local_node(Name, antidote, ?ANTIDOTE_PORT).

start_node_with_riak_backend(Name) ->
    fmke_test_utils:start_riak(),
    start_local_node(Name, riak, ?RIAK_PORT).

start_node_with_redis_backend(Name) ->
    fmke_test_utils:start_redis(),
    start_local_node(Name, redis, ?REDIS_PORT).

start_node_with_mock_redis_cluster(Name) ->
    fmke_test_utils:start_redis(),
    %% Uses two different loopback addresses to create pools (one IPv4, one IPv6)
    start_local_node(Name, redis, ["127.0.0.1", "::1"], [?REDIS_PORT, ?REDIS_PORT]).

start_local_node(Name, Database, Port) ->
    start_local_node(Name, Database, ["127.0.0.1"], [Port]).

start_local_node(Name, Database, Hostnames, Ports) ->
    io:format(
        "Trying to spawn node ~p and connect it to ~p...~n"
        "Using list of hostnames: ~p...~n"
        "Using list of ports: ~p~n",
        [Name, Database, Hostnames, Ports]),
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    %% have the slave nodes monitor the runner node, so they can't outlive it
    NodeConfig = [{monitor_master, true}, {kill_if_fail, true}, {boot_timeout, 5}, {init_timeout, 3},
        {startup_timeout, 1}, {startup_functions, [{code, set_path, [CodePath]}]}],
    case ct_slave:start(Name, NodeConfig) of
        {ok, Node} ->
            ok = rpc:call(Node, application, set_env, [?APP, target_database, Database]),
            ok = rpc:call(Node, application, set_env, [?APP, connection_pool_size, 64]),
            ok = rpc:call(Node, application, set_env, [?APP, database_addresses, Hostnames]),
            ok = rpc:call(Node, application, set_env, [?APP, database_ports, Ports]),
            ok = rpc:call(Node, application, set_env, [?APP, http_port, 9090]),

            ClientLib = get_client_lib(Database),
            ok = rpc:call(Node, application, load, [ClientLib]),
            %% start the application remotely
            {ok, _} = rpc:call(Node, application, ensure_all_started, [?APP]),
            io:format("Node ~p started", [Node]),
            Node;
        {error, Reason, Node} ->
            io:format("Error starting node ~p (~p), retrying...", [Node, Reason]),
            ct_slave:stop(Name),
            wait_until_offline(Node),
            start_local_node(Name, Database, Hostnames, Ports)
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
    ct_slave:stop(Node),
    wait_until_offline(Node).

-spec get_client_lib(Database :: antidote | antidote_norm | riak | riak_norm | redis) -> atom().
get_client_lib(antidote) -> antidote_pb;
get_client_lib(antidote_norm) -> antidote_pb;
get_client_lib(riak) -> riak_pb;
get_client_lib(riak_norm) -> riak_pb;
get_client_lib(redis) -> eredis.
