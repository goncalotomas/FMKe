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
    launch_app/2,
    stop_all/0,
    stop_antidote/0,
    stop_riak/0,
    stop_redis/0,
    stop_node/1
]).

-define(ANTIDOTE_PORT, 8087).
-define(RIAK_PORT, 8087).
-define(REDIS_PORTS, [7000, 7001, 7002, 7003, 7004, 7005]).
-define(DOCKER_CMD_STOP_ANTIDOTE, "docker stop antidote && docker rm antidote").
-define(DOCKER_CMD_STOP_RIAK, "docker stop riak && docker rm riak").
-define(DOCKER_CMD_STOP_REDIS, "docker stop redis && docker rm redis").
-define(DOCKER_CMD_STOP_ALL, "docker stop $(docker ps -aq) && docker rm $(docker ps -aq)").

-define(DOCKER_CMD_START_ANTIDOTE(Port), "docker run -d --name antidote -e NODE_NAME=antidote@127.0.0.1 -p "
                                   "\"4368:4368\" -p \"" ++ integer_to_list(Port) ++ ":8087\" mweber/antidotedb").

-define(DOCKER_CMD_START_RIAK(Port), "docker run -d --name riak -p \"" ++ integer_to_list(Port) ++ ":8087\" "
                                "-e NODE_NAME=riak@127.0.0.1 goncalotomas/riak").

-define(DOCKER_CMD_START_REDIS(Port), "docker run -d --name redis -e CLUSTER_ONLY=true -e IP=0.0.0.0 "
                                "-p \"" ++ integer_to_list(Port) ++ ":7000\" "
                                "-p \"" ++ integer_to_list(Port+1) ++ ":7001\" "
                                "-p \"" ++ integer_to_list(Port+2) ++ ":7002\" "
                                "-p \"" ++ integer_to_list(Port+3) ++ ":7003\" "
                                "-p \"" ++ integer_to_list(Port+4) ++ ":7004\" "
                                "-p \"" ++ integer_to_list(Port+5) ++ ":7005\" grokzen/redis-cluster:latest").

start_antidote() ->
    start_antidote(8087).

start_antidote(Port) ->
    0 = cmd:run(?DOCKER_CMD_START_ANTIDOTE(Port), return_code),
    io:format("Started antidote.~n"),
    timer:sleep(5000),
    ok.

stop_antidote() ->
    0 = cmd:run(?DOCKER_CMD_STOP_ANTIDOTE, return_code),
    ok.

start_riak() ->
    start_riak(8087).

start_riak(Port) ->
    0 = cmd:run(?DOCKER_CMD_START_RIAK(Port), return_code),
    io:format("Started riak.~n"),
    timer:sleep(30000),
    ok.

stop_riak() ->
    0 = cmd:run(?DOCKER_CMD_STOP_RIAK, return_code),
    ok.

start_redis() ->
    start_redis(7000).

start_redis(Port) ->
    0 = cmd:run(?DOCKER_CMD_START_REDIS(Port), return_code),
    io:format("Started redis.~n"),
    timer:sleep(10000),
    ok.

stop_redis() ->
    0 = cmd:run(?DOCKER_CMD_STOP_REDIS, return_code),
    ok.

stop_all() ->
    %% when we use the stop all command, we could be making sure that all instances are down
    %% (say, before a test suite is run). This means that we are not sure that it will always
    %% be successful.
    Result = cmd:run(?DOCKER_CMD_STOP_ALL, return_code),
    io:format("Stopped all running Docker containers (return code ~p).~n", [Result]).

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
            AdapterOps = case proplists:get_value(optimized_driver, Opts, undefined) of
                undefined ->
                    Opts;
                true ->
                    %% optimized drivers imply that the driver implements the full FMKe interface
                    %% in such a case, the adapter to use is the passthrough adapter.
                    lists:keyreplace(adapter, 1, Opts, {adapter, fmke_pt_adapter});
                false ->
                    Opts
            end,
            StartupOpts = lists:ukeymerge(1, lists:sort(AdapterOps), lists:sort(maps:to_list(?DEFAULTS))),
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

stop_node(Node) ->
    rpc:call(Node, application, stop, [?APP]),
    ct_slave:stop(Node),
    wait_until_offline(Node).

launch_app(Nodename, Config) ->
    io:format("Got config = ~p~n", [Config]),
    Database = proplists:get_value(target_database, Config),
    [Port] = proplists:get_value(database_ports, Config),
    ok = start_db(Database, Port),
    start_node(Nodename, Config).

start_db(antidote, Port) ->
    start_antidote(Port);
start_db(ets, _Port) ->
    ok; %% ets doesn't need to be started
start_db(redis, Port) ->
    start_redis(Port);
start_db(riak, Port) ->
    start_riak(Port).

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
