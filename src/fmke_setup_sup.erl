%% ----------------------------------------------------------------------------
%% fmke_setup_sup: supervises the adapter, driver and connection manager
%% Stars 2-3 children:
%% - adapter: the proper adapter for handling the database according to the config
%% - driver: a module that is able to perform operations on the selected database
%% - conn_manager_sup: only started if the driver needs it, manages connections to the database
%% ----------------------------------------------------------------------------
-module(fmke_setup_sup).

-behaviour(supervisor).

-include("fmke.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE_OPTS, [set, public, named_table, {keypos,1}, {write_concurrency,false}, {read_concurrency,false}]).

-type database() :: antidote | ets | redis | riak.

start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init([Adapter, Database, DataModel, Optimized]) ->
    case requires_ets_table(Database, DataModel, Optimized) of
        true -> ets:new(?ETS_TABLE_NAME, ?ETS_TABLE_OPTS);
        false -> ok
    end,

    Driver = case Optimized of
        true -> opt_driver(Database);
        false -> driver(Database)
    end,

    RestartStrategy = #{
        strategy => one_for_all,
        intensity => 10,
        period => 10
    },

    BaseChildren = [adapter_spec(Adapter, Driver, DataModel), driver_spec(Driver, DataModel)],

    Children = case requires_conn_manager(Database, DataModel, Optimized) of
        false ->
            ok = application:set_env(?APP, pools, []),
            BaseChildren;
        true ->
            {ok, ConnPoolSize} = application:get_env(connection_pool_size),
            {ok, Hostnames} = application:get_env(database_addresses),
            {ok, PortNums} = application:get_env(database_ports),

            Mod = get_client_lib(Database, DataModel, Optimized),
            {Hosts, Ports} = make_same_len(Hostnames, PortNums),
            Pools = gen_pool_names(Hosts, Ports),
            Connections = lists:zip(Hosts, Ports),
            Args = [Pools, Connections, Mod, ConnPoolSize],
            BaseChildren ++ [conn_mgr_sup_spec(Args)]
    end,

    {ok, {RestartStrategy, Children}}.

-spec adapter_spec(module(), module(), atom()) -> supervisor:child_spec().
adapter_spec(Adapter, Driver, DataModel) ->
    #{
        id => fmke,
        start => {Adapter, start, [[Driver, DataModel]]},
        restart => permanent,
        type => worker
    }.

-spec driver_spec(Driver::module(), DataModel::atom()) -> supervisor:child_spec().
driver_spec(Driver, DataModel) ->
  #{
      id => driver,
      start => {Driver, start, [DataModel]},
      restart => permanent,
      type => worker
  }.

-spec conn_mgr_sup_spec(Args::list(term())) -> supervisor:child_spec().
conn_mgr_sup_spec(Args) ->
  #{
      id => conn_manager_sup,
      start => {fmke_db_conn_sup, start_link, [Args]},
      restart => permanent,
      type => supervisor
  }.

-spec gen_pool_names(list(list()), list(non_neg_integer())) -> list(atom()).
gen_pool_names(Addrs, Ports) ->
    gen_pool_names(Addrs, Ports, []).

gen_pool_names([], [], Accum) ->
    lists:reverse(Accum);
gen_pool_names([A|T], [P|T2], Accum) ->
    gen_pool_names(T, T2, [gen_pool_name(A, P) | Accum]).

-spec get_client_lib(Database::atom(), DataModel::atom(), Optimized::atom()) -> atom().
get_client_lib(antidote, _, _) ->       antidotec_pb_socket;
get_client_lib(riak, _, _) ->           riakc_pb_socket;
get_client_lib(redis, _, _) ->          eredis.

-spec requires_ets_table(Database::atom(), DataModel::atom(), Optimized::atom()) -> true | false.
requires_ets_table(ets, _, _) ->        true;
requires_ets_table(_, _, _) ->          false.

-spec requires_conn_manager(Database::atom(), DataModel::atom(), Optimized::atom()) -> true | false.
requires_conn_manager(antidote, _, _) ->    true;
requires_conn_manager(riak, _, _) ->        true;
requires_conn_manager(redis, _, _) ->       true;
requires_conn_manager(_, _, _) ->           false.

-spec driver(Database::database()) -> module().
driver(antidote) ->     fmke_driver_antidote;
driver(ets) ->          fmke_driver_ets;
driver(redis) ->        fmke_driver_redis;
driver(riak) ->         fmke_driver_riak_kv.

-spec opt_driver(Database::atom()) -> module().
opt_driver(antidote) -> fmke_driver_opt_antidote;
opt_driver(redis) ->    fmke_driver_opt_redis;
opt_driver(riak) ->     fmke_driver_opt_riak_kv.

-spec make_same_len(L1 :: list(), L2 :: list()) -> {list(), list()}.
make_same_len(L1, L2) when length(L1) == length(L2) -> {L1, L2};
make_same_len([H1|_T1] = L1, L2) when length(L1) < length(L2) -> make_same_len([H1 | L1], L2);
make_same_len(L1, [H2|_T2] = L2) when length(L1) > length(L2) -> make_same_len(L1, [H2 | L2]).

gen_pool_name(Addr, Port) ->
    AtomCompatAddr = get_atom_compatible_list(Addr),
    list_to_atom(unicode:characters_to_list(["pool_", AtomCompatAddr, "_", integer_to_list(Port)])).

-spec get_atom_compatible_list(Str :: list()) -> list().
get_atom_compatible_list(Str) ->
    lists:flatten(string:replace(lists:flatten(string:replace(Str, ".", ":", all)), ":", "_", all)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                     Eunit Tests                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

single_host_single_port_list_setup_test() ->
    {Hosts, Ports} = make_same_len(["127.0.0.1"], [8087]),
    ?assertEqual(Hosts, ["127.0.0.1"]),
    ?assertEqual(Ports, [8087]).

single_host_multiple_ports_list_setup_test() ->
    {Hosts, Ports} = make_same_len(["127.0.0.1"], [8087, 8187, 8287, 8387]),
    ?assertEqual(Hosts, ["127.0.0.1", "127.0.0.1", "127.0.0.1", "127.0.0.1"]),
    ?assertEqual(Ports, [8087, 8187, 8287, 8387]).

multiple_hosts_single_port_list_setup_test() ->
    {Hosts, Ports} = make_same_len(["127.0.0.1", "8.8.8.8", "196.162.1.1", "0.0.0.0"], [8087]),
    ?assertEqual(Hosts, ["127.0.0.1", "8.8.8.8", "196.162.1.1", "0.0.0.0"]),
    ?assertEqual(Ports, [8087, 8087, 8087, 8087]).

multiple_hosts_multiple_ports_same_length_list_setup_test() ->
    Hosts = ["8.8.8.8", "196.162.1.1"],
    Ports = [8087, 8187],
    ?assertEqual({Hosts, Ports}, make_same_len(Hosts, Ports)).

multiple_hosts_multiple_ports_more_hosts_list_setup_test() ->
    Hosts = ["127.0.0.1", "8.8.8.8", "196.162.1.1", "0.0.0.0"],
    Ports = [8087, 8187],
    ?assertEqual({Hosts, [8087, 8087, 8087, 8187]}, make_same_len(Hosts, Ports)).

multiple_hosts_multiple_ports_more_ports_list_setup_test() ->
    Hosts = ["8.8.8.8", "196.162.1.1"],
    Ports = [8087, 8187, 8287, 8387],
    ?assertEqual({["8.8.8.8", "8.8.8.8", "8.8.8.8", "196.162.1.1"], Ports}, make_same_len(Hosts, Ports)).

get_atom_from_ipv4_addr_test() ->
    ?assertEqual('127_0_0_1', list_to_atom(get_atom_compatible_list("127.0.0.1"))).

get_atom_from_ipv6_addr_test() ->
    ?assertEqual('2001_0db8_85a3_0000_0000_8a2e_0370_7334',
        list_to_atom(get_atom_compatible_list("2001:0db8:85a3:0000:0000:8a2e:0370:7334"))).

-endif.
