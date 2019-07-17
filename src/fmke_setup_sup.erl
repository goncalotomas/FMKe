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

start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init([]) ->
    %% Driver, adapter, data_model and connection_pool_size are assumed to be defined at this point by fmke_sup.
    Driver = fmke_driver_config:selected_driver(),
    {ok, DataModel} = application:get_env(?APP, data_model),
    {ok, ConnPoolSize} = application:get_env(?APP, connection_pool_size),
    %% these remaining 2 options may be undefined if working with Mnesia, ETS, or other types of connections that don't
    %% use {hostname, port} combinations to connect to the data store.
    Hostnames = application:get_env(?APP, database_addresses),
    PortNums = application:get_env(?APP, database_ports),

    case fmke_driver_config:requires_ets_table(Driver) of
        true -> ets:new(?ETS_TABLE_NAME, ?ETS_TABLE_OPTS);
        false -> ok
    end,

    RestartStrategy = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 10
    },

    DataModel = case application:get_env(?APP, data_model) of
        undefined ->
            ?DEFAULT(data_model);
        {ok, RequestedDataModel} ->
            RequestedDataModel
    end,

    BaseChildren = [handler_pool_spec()],

    Children = case {Hostnames, PortNums} of
        {undefined, undefined} ->
            lager:info("list of hosts and ports are undefined, cannot create connection pools."),
            ok = application:set_env(?APP, pools, []),
            BaseChildren;
        {_Hosts, undefined} ->
            lager:info("list of ports is undefined, cannot create connection pools."),
            ok = application:set_env(?APP, pools, []),
            BaseChildren;
        {undefined, _Ports} ->
            lager:info("list of hosts is undefined, cannot create connection pools."),
            ok = application:set_env(?APP, pools, []),
            BaseChildren;
        {{ok, Hs}, {ok, Ps}} ->
            {Hosts, Ports} = make_same_len(Hs, Ps),
            case fmke_driver_config:requires_conn_manager(Driver) of
                false ->
                    ok = application:set_env(?APP, pools, []),
                    ok = application:set_env(?APP, hosts, Hosts),
                    ok = application:set_env(?APP, ports, Ports),
                    BaseChildren;
                true ->
                    Mod = fmke_driver_config:get_client_lib(Driver),
                    Pools = gen_pool_names(Hosts, Ports),
                    Connections = lists:zip(Hosts, Ports),
                    Args = [Pools, Connections, Mod, ConnPoolSize],
                    BaseChildren ++ [conn_mgr_sup_spec(Args)]
            end
    end,

    {ok, {RestartStrategy, Children}}.

-spec conn_mgr_sup_spec(Args::list(term())) -> supervisor:child_spec().
conn_mgr_sup_spec(Args) ->
  #{
      id => conn_manager_sup,
      start => {fmke_db_conn_sup, start_link, [Args]},
      restart => permanent,
      type => supervisor
  }.

-spec handler_pool_spec() -> supervisor:child_spec().
handler_pool_spec() ->
    Driver = fmke_driver_config:selected_driver(),
    {Module, WorkerArgs} = case fmke_driver_config:is_simple_kv_driver(Driver) of
        false ->
            {Driver, []};
        true ->
            {ok, DataModel} = application:get_env(?APP, data_model),
            {fmke_kv_adapter, [Driver, DataModel]}
    end,
    Name = handlers,
    NumHandlers = get_handler_pool_size(),
    lager:info("Handler pool will have ~p procs.", [NumHandlers]),
    SizeArgs = [{size, NumHandlers}, {max_overflow, 0}],
    PoolArgs = [{name, {local, Name}}, {worker_module, Module}] ++ SizeArgs,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs).

get_handler_pool_size() ->
    Driver = fmke_driver_config:selected_driver(),
    {ok, ConnPoolSize} = application:get_env(?APP, connection_pool_size),
    case fmke_driver_config:requires_conn_manager(Driver) of
        false ->
            ConnPoolSize;
        true ->
            ConnPoolSize * get_num_db_pools()
    end.

get_num_db_pools() ->
    {ok, Hostnames} = application:get_env(?APP, database_addresses),
    {ok, PortNums} = application:get_env(?APP, database_ports),
    {Hosts, _} = make_same_len(Hostnames, PortNums),
    length(Hosts).

-spec gen_pool_names(list(list()), list(non_neg_integer())) -> list(atom()).
gen_pool_names(Addrs, Ports) ->
    gen_pool_names(Addrs, Ports, []).

gen_pool_names([], [], Accum) ->
    lists:reverse(Accum);
gen_pool_names([A|T], [P|T2], Accum) ->
    gen_pool_names(T, T2, [gen_pool_name(A, P) | Accum]).

-spec make_same_len(L1 :: list(), L2 :: list()) -> {list(), list()}.
make_same_len(L1, L2) when length(L1) == length(L2) -> {L1, L2};
make_same_len([H1|_T1] = L1, L2) when length(L1) < length(L2) -> make_same_len([H1 | L1], L2);
make_same_len(L1, [H2|_T2] = L2) when length(L1) > length(L2) -> make_same_len(L1, [H2 | L2]).

-spec gen_pool_name(Addr :: list(), Port :: non_neg_integer()) -> atom().
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
