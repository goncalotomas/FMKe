%%%-------------------------------------------------------------------
%% @doc fmk top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fmke_sup).

-behaviour(supervisor).

-include ("fmke.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
    config_env(),

    ConnPoolSpecs = gen_pool_specs(),
    ConnManagerSpec = gen_conn_pool_mgr_spec(),
    WebServerSpec = gen_web_server_spec(),
    ApplicationSpec = gen_fmke_spec(),

    RestartStrategy = #{strategy => one_for_one, intensity => 10, period => 10},
    {ok, {RestartStrategy, [WebServerSpec, ApplicationSpec, ConnManagerSpec | ConnPoolSpecs]}}.

%%====================================================================
%% Internal functions
%%====================================================================

config_env() ->
    try
        {ok, CurrentDirectory} = file:get_cwd(),
        ConfigFile = CurrentDirectory ++ ?CONFIG_FILE_PATH,
        {ok, AppProps} = file:consult(ConfigFile),
        config(AppProps)
    catch
        _:Reason ->
            lager:info("Error reading from config file: ~p", [Reason]),
            lager:info("Could not read from config file, reverting to environment and default values..."),
            config([])
    end.

%% Sets all options needed to start FMKe, from the 4 following sources, ordered by priority:
%% OS Environment, Application Environment, config file, default value
config(ConfigProps) ->
    lists:foreach(
        fun(Param) ->
            {Source, Value} = get_value(os:getenv(atom_to_list(Param)), application:get_env(?APP, Param),
                                        proplists:get_value(Param, ConfigProps), maps:get(Param, ?DEFAULTS)),
            lager:info("~p option '~p' read from ~p, set to ~p", [?APP, Param, Source, Value]),
            ok = application:set_env(?APP, Param, Value)
        end,
    ?OPTIONS).

-spec gen_fmke_spec() -> supervisor:child_spec().
gen_fmke_spec() ->
    #{
        id => fmke,
        start => {fmke, start_link, [[]]},
        restart => permanent,
        type => worker
    }.

-spec gen_web_server_spec() -> supervisor:child_spec().
gen_web_server_spec() ->
    {ok, HttpPort} = application:get_env(?APP, http_port),

    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", fmke_http_handler_app, []},
        {"/prescriptions/[:id]", fmke_http_handler_prescriptions, []},
        {"/patients/[:id]", fmke_http_handler_patients, []},
        {"/pharmacies/[:id]", fmke_http_handler_pharmacies, []},
        {"/pharmacies/[:id]/prescriptions", fmke_http_handler_pharmacies, prescriptions},
        {"/pharmacies/[:id]/processed_prescriptions", fmke_http_handler_pharmacies, processed_prescriptions},
        {"/facilities/[:id]", fmke_http_handler_facilities, []},
        % {"/treatments/[:id]", fmke_http_handler_treatments, []},
        % {"/events/[:id]", fmke_http_handler_events, []},
        {"/staff/[:id]", fmke_http_handler_staff, []},
        {"/staff/[:id]/prescriptions", fmke_http_handler_staff, prescriptions}
      ]}
    ]),

    #{
        id => cowboy,
        start => {cowboy, start_clear, [fmke_http_listener, [{port, HttpPort}], #{env => #{dispatch => Dispatch}}]},
        restart => permanent,
        type => worker
    }.

-spec gen_pool_specs() -> list(supervisor:child_spec()).
gen_pool_specs() ->
    {ok, TargetDatabase} = application:get_env(?APP, target_database),
    {ok, ConnPoolSize} = application:get_env(?APP, connection_pool_size),
    {ok, Hostnames} = application:get_env(?APP, database_addresses),
    {ok, PortNums} = application:get_env(?APP, database_ports),

    ClientLib = get_client_lib(TargetDatabase),
    {Hosts, Ports} = make_same_len(Hostnames, PortNums),
    Connections = lists:zip(Hosts, Ports),
    NumPools = length(Connections),
    %% Save pool names to application env
    ok = application:set_env(?APP, pools, lists:map(
                                                fun(N) ->
                                                    list_to_atom("pool_" ++ get_atom_compatible_addr(
                                                                                lists:nth(N, Hosts)))
                                                end, lists:seq(1, NumPools))),
    %% Create a connection pool to each database node
    Pools = lists:map(
                fun(N) ->
                    Name = list_to_atom("pool_" ++ get_atom_compatible_addr(
                                                lists:nth(N, Hosts))),
                    lager:info("Generating spec for conn pool with name ~p~n", [Name]),
                    SizeArgs = [{size, ConnPoolSize}, {max_overflow, 2 * ConnPoolSize}],
                    {Host, Port} = lists:nth(N, Connections),
                    WorkerArgs = [{client_lib, ClientLib}, {host, Host}, {port, Port}],
                    {Name, SizeArgs, WorkerArgs}
                end, lists:seq(1, NumPools)),
    %% return specs for each pools
    lists:map(
        fun({Name, SizeArgs, WorkerArgs}) ->
            PoolArgs = [{name, {local, Name}}, {worker_module, fmke_db_connection}] ++ SizeArgs,
            poolboy:child_spec(Name, PoolArgs, WorkerArgs)
        end, Pools).

-spec gen_conn_pool_mgr_spec() -> supervisor:child_spec().
gen_conn_pool_mgr_spec() ->
    #{
        id => fmke_db_conn_manager,
        start => {fmke_db_conn_manager, start_link, []},
        restart => permanent,
        type => worker
    }.

get_value(false, undefined, undefined, Val) -> {defaults, Val};
get_value(false, undefined, Val, _) -> {config_file, Val};
get_value(false, {ok, Val}, _, _) -> {app_env, Val};
get_value(Val, _, _, _) -> {os_env, Val}.

-spec make_same_len(L1 :: list(), L2 :: list()) -> {list(), list()}.
make_same_len(L1, L2) when length(L1) == length(L2) -> {L1, L2};
make_same_len([H1|_T1] = L1, L2) when length(L1) < length(L2) -> make_same_len([H1 | L1], L2);
make_same_len(L1, [H2|_T2] = L2) when length(L1) > length(L2) -> make_same_len(L1, [H2 | L2]).

-spec get_client_lib(Database :: antidote | antidote_norm | riak | riak_norm | redis) -> atom().
get_client_lib(antidote) -> antidotec_pb_socket;
get_client_lib(antidote_norm) -> antidotec_pb_socket;
get_client_lib(riak) -> riakc_pb_socket;
get_client_lib(riak_norm) -> riakc_pb_socket;
get_client_lib(redis) -> eredis.

-spec get_atom_compatible_addr(Addr :: list()) -> list().
get_atom_compatible_addr(Addr) ->
    lists:flatten(string:replace(lists:flatten(string:replace(Addr, ".", ":", all)), ":", "_", all)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                     Eunit Tests                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

web_server_spec_test() ->
    HttpPort = 8888,
    ok = application:set_env(?APP, http_port, HttpPort),
    #{
        id := cowboy,
        start := {cowboy, start_clear, [fmke_http_listener, [{port, HttpPort}], #{env := #{dispatch := _Dispatch}}]},
        restart := permanent,
        type := worker
    } = gen_web_server_spec().

single_node_pool_spec_test() ->
    ok = application:set_env(?APP, target_database, riak),
    ok = application:set_env(?APP, connection_pool_size, 1),
    ok = application:set_env(?APP, database_addresses, ["8.8.8.8"]),
    ok = application:set_env(?APP, database_ports, [4321]),
    ExpectedSpec = {pool_8_8_8_8,
         {poolboy,start_link,
             [[{name,{local,pool_8_8_8_8}},
               {worker_module,fmke_db_connection},
               {size,1},
               {max_overflow,2}],
              [{client_lib,riakc_pb_socket},
               {host,"8.8.8.8"},
               {port,4321}]]},
         permanent,5000,worker,
         [poolboy]},
    ?assertEqual([ExpectedSpec], gen_pool_specs()).

multiple_node_pool_spec_test() ->
    ok = application:set_env(?APP, target_database, redis),
    ok = application:set_env(?APP, connection_pool_size, 2),
    ok = application:set_env(?APP, database_addresses, ["8.8.8.8", "127.0.0.1"]),
    ok = application:set_env(?APP, database_ports, [4321, 8765]),
    ExpectedSpecs = [{pool_8_8_8_8,
         {poolboy,start_link,
             [[{name,{local,pool_8_8_8_8}},
               {worker_module,fmke_db_connection},
               {size,2},
               {max_overflow,4}],
              [{client_lib,eredis},{host,"8.8.8.8"},{port,4321}]]},
         permanent,5000,worker,
         [poolboy]},
     {pool_127_0_0_1,
         {poolboy,start_link,
             [[{name,{local,pool_127_0_0_1}},
               {worker_module,fmke_db_connection},
               {size,2},
               {max_overflow,4}],
              [{client_lib,eredis},{host,"127.0.0.1"},{port,8765}]]},
         permanent,5000,worker,
         [poolboy]}],
    ?assertEqual(ExpectedSpecs, gen_pool_specs()).

single_host_single_port_setup_test() ->
    {Hosts, Ports} = make_same_len(["127.0.0.1"], [8087]),
    ?assertEqual(Hosts, ["127.0.0.1"]),
    ?assertEqual(Ports, [8087]).

single_host_multiple_ports_setup_test() ->
    {Hosts, Ports} = make_same_len(["127.0.0.1"], [8087, 8187, 8287, 8387]),
    ?assertEqual(Hosts, ["127.0.0.1", "127.0.0.1", "127.0.0.1", "127.0.0.1"]),
    ?assertEqual(Ports, [8087, 8187, 8287, 8387]).

multiple_hosts_single_port_setup_test() ->
    {Hosts, Ports} = make_same_len(["127.0.0.1", "8.8.8.8", "196.162.1.1", "0.0.0.0"], [8087]),
    ?assertEqual(Hosts, ["127.0.0.1", "8.8.8.8", "196.162.1.1", "0.0.0.0"]),
    ?assertEqual(Ports, [8087, 8087, 8087, 8087]).

multiple_hosts_multiple_ports_same_length_setup_test() ->
    Hosts = ["8.8.8.8", "196.162.1.1"],
    Ports = [8087, 8187],
    ?assertEqual({Hosts, Ports}, make_same_len(Hosts, Ports)).

multiple_hosts_multiple_ports_more_hosts_setup_test() ->
    Hosts = ["127.0.0.1", "8.8.8.8", "196.162.1.1", "0.0.0.0"],
    Ports = [8087, 8187],
    ?assertEqual({Hosts, [8087, 8087, 8087, 8187]}, make_same_len(Hosts, Ports)).

multiple_hosts_multiple_ports_more_ports_setup_test() ->
    Hosts = ["8.8.8.8", "196.162.1.1"],
    Ports = [8087, 8187, 8287, 8387],
    ?assertEqual({["8.8.8.8", "8.8.8.8", "8.8.8.8", "196.162.1.1"], Ports}, make_same_len(Hosts, Ports)).

get_atom_from_ipv4_addr_test() ->
    ?assertEqual('127_0_0_1', list_to_atom(get_atom_compatible_addr("127.0.0.1"))).

get_atom_from_ipv6_addr_test() ->
    ?assertEqual('2001_0db8_85a3_0000_0000_8a2e_0370_7334',
        list_to_atom(get_atom_compatible_addr("2001:0db8:85a3:0000:0000:8a2e:0370:7334"))).

-endif.
