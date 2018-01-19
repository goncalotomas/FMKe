%% ---------------------------------------------------------------------------------------------------------------------
%% This module is responsible for maintaining application state using the fmke_mochiglobal module, as well as
%% reading the configuration file and determining a series of parameters to connect to the target database.
%%
%% The following parameters are read from the config file:
%% - http_port (determines the HTTP port where FMKe will listen for HTTP requests)
%% - connection_pool_size (determines the number of connections that FMKe will keep open to the target database)
%% - database_addresses (a list of IP addresses to which FMKe will connect)
%% - database_ports (a list of port numbers that matches the addresses provided above)
%% - target_database (the name of the target database, provided to load the correct database driver)
%% ---------------------------------------------------------------------------------------------------------------------
-module (fmke_config).
-include ("fmke.hrl").
-export ([
    read_config_file/0,
    get/1,
    get/2,
    set/2
]).

get(Key) ->
    fmke_mochiglobal:get(Key, undefined).

get(Key, Default) ->
    fmke_mochiglobal:get(Key, Default).

set(Key, Value) ->
    fmke_mochiglobal:put(Key, Value).

read_config_file() ->
    {ok, CurrentDirectory} = file:get_cwd(),
    ConfigFile = CurrentDirectory ++ ?CONFIG_FILE_PATH,
    {ok, AppProps} = file:consult(ConfigFile),

    %% read properties from the config file
    HttpPort = proplists:get_value(http_port, AppProps),
    ConnPoolSize = proplists:get_value(connection_pool_size, AppProps),
    DbAddressList = proplists:get_value(database_addresses, AppProps),
    DbPortList = proplists:get_value(database_ports, AppProps),
    TargetDatabase = proplists:get_value(target_database, AppProps),

    %% check for correct input from the config file
    true = is_integer(HttpPort) andalso HttpPort > 0 andalso HttpPort =< 65535,
    true = is_integer(ConnPoolSize) andalso ConnPoolSize > 0,
    {ok, ParsedAddressList} = parse_db_address_list(DbAddressList),
    {ok, ParsedPortList} = parse_db_port_list(DbPortList),
    true = supported_db(TargetDatabase),

    {Driver, DriverImpl} = get_driver_setup(TargetDatabase),

    %% set application variables from config file
    set(http_port, HttpPort),
    set(connection_pool_size, ConnPoolSize),
    set(driver, Driver),
    set(simplified_driver, DriverImpl),
    set(db_conn_hostnames, ParsedAddressList),
    set(db_conn_ports, ParsedPortList),

    [
        {driver, Driver},
        {simplified_driver, DriverImpl},
        {db_conn_hostnames, ParsedAddressList},
        {db_conn_ports, ParsedPortList},
        {db_conn_pool_size, ConnPoolSize},
        {http_port, HttpPort}
    ].

%%====================================================================
%% Auxiliary functions
%%====================================================================

get_driver_setup(Database) when is_list(Database) ->
    get_driver_setup(list_to_atom(Database));

get_driver_setup(Database) when is_atom(Database) ->
    %% TODO maybe I should find a better way to do this... later.
    DriverSetups = #{
      antidote => {fmke_kv_driver, fmke_db_driver_antidote},
      antidote_norm => {fmke_db_driver_antidote_norm, undefined},
      redis => {fmke_kv_driver, fmke_db_driver_redis},
      riak => {fmke_kv_driver, fmke_db_driver_riak_kv},
      riak_kv => {fmke_kv_driver, fmke_db_driver_riak_kv},
      riak_norm => {fmke_db_driver_riak_kv_norm, undefined},
      riak_kv_norm => {fmke_db_driver_riak_kv_norm, undefined}
    },
    case maps:find(Database, DriverSetups) of
        {ok, Value} -> Value;
        error -> {error, not_supported, Database}
    end.

is_alias_of_database(riak_norm) ->
    {true, riak_kv_norm};
is_alias_of_database(riak) ->
    {true, riak_kv};
is_alias_of_database(_DatabaseName) ->
    false.

parse_db_address_list(DbAddressList) when is_tuple(DbAddressList) ->
    parse_db_address_list([DbAddressList]);
parse_db_address_list(DbAddressList) ->
    case io_lib:printable_unicode_list(DbAddressList) of
        false ->
            parse_db_address_list_rec(DbAddressList, []);
        true ->
            Split = string:split(DbAddressList, " "),
            case Split of
                [[]] -> {error, no_addresses};
                _ -> {ok, Split}
            end
    end.

parse_db_address_list_rec([], []) -> {error, no_addresses};
parse_db_address_list_rec([], Accum) -> {ok, lists:reverse(Accum)};
parse_db_address_list_rec([H|T], Accum) ->
    case is_tuple(H) of
        true -> parse_db_address_list_rec(T, [read_tuple_address(H) | Accum]);
        false ->
            case io_lib:printable_unicode_list(H) of
                false -> {error, invalid_format};
                true -> parse_db_address_list_rec(T, [H | Accum])
            end
    end.

read_tuple_address({Fst, Snd, Thd, Fth}) when is_integer(Fst), is_integer(Snd), is_integer(Thd), is_integer(Fth) ->
    integer_to_list(Fst) ++ "." ++ integer_to_list(Snd) ++ "." ++ integer_to_list(Thd) ++ "." ++ integer_to_list(Fth);
read_tuple_address({A, B, C, D, E, F, G, H}) ->
    A ++ ":" ++ B ++ ":" ++ C ++ ":" ++ D ++ ":" ++ E ++ ":" ++ F ++ ":" ++ G ++ ":" ++ H.

parse_db_port_list(DbPortList) when is_integer(DbPortList) ->
    parse_db_port_list([DbPortList]);
parse_db_port_list(DbPortList) ->
    case io_lib:printable_list(DbPortList) of
        false ->
            parse_db_port_list_rec(DbPortList, []);
        true ->
            Split = string:split(DbPortList, " "),
            case Split of
                [[]] -> {error, no_ports};
                _ -> parse_db_port_list_rec(Split, [])
            end
    end.

parse_db_port_list_rec([], []) -> {error, no_ports};
parse_db_port_list_rec([], Accum) -> {ok, lists:reverse(Accum)};
parse_db_port_list_rec([H|T], Accum) ->
    case is_integer(H) of
        true -> parse_db_port_list_rec(T, [H | Accum]);
        false ->
            case io_lib:printable_list(H) of
                false -> {error, invalid_format};
                true -> parse_db_port_list_rec(T, [list_to_integer(H) | Accum])
            end
    end.

supported_db(Database) when is_atom(Database) ->
    case lists:member(Database, ?SUPPORTED_DBS) of
        true -> true;
        false ->
            case is_alias_of_database(Database) of
                {true, _Handle} -> true;
                false -> false
            end
    end;

supported_db(Database) when is_list(Database) ->
    case io_lib:printable_unicode_list(Database) of
        true -> supported_db(list_to_atom(Database));
        false -> false
    end;

supported_db(_Database) ->
    false.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

mochiglobal_application_state_test() ->
    undefined = ?MODULE:get(something),
    everything = ?MODULE:get(something, everything),
    ok = set(something, nothing),
    nothing = ?MODULE:get(something).

supports_antidote_test() ->
    ?assert(supported_db("antidote")),
    ?assert(supported_db(antidote)).

supports_antidote_norm_test() ->
    ?assert(supported_db("antidote_norm")),
    ?assert(supported_db(antidote_norm)).

supports_riak_test() ->
    ?assert(supported_db("riak")),
    ?assert(supported_db(riak)).

supports_riak_norm_test() ->
    ?assert(supported_db("riak_norm")),
    ?assert(supported_db(riak_norm)).

supports_redis_test() ->
    ?assert(supported_db("redis")),
    ?assert(supported_db(redis)).

correct_antidote_driver_setup_test() ->
    {fmke_kv_driver, fmke_db_driver_antidote} = get_driver_setup("antidote"),
    {fmke_kv_driver, fmke_db_driver_antidote} = get_driver_setup(antidote).

correct_antidote_norm_driver_setup_test() ->
    {fmke_db_driver_antidote_norm, undefined} = get_driver_setup("antidote_norm"),
    {fmke_db_driver_antidote_norm, undefined} = get_driver_setup(antidote_norm).

correct_riak_driver_setup_test() ->
    {fmke_kv_driver, fmke_db_driver_riak_kv} = get_driver_setup("riak"),
    {fmke_kv_driver, fmke_db_driver_riak_kv} = get_driver_setup(riak).

correct_riak_norm_driver_setup_test() ->
    {fmke_db_driver_riak_kv_norm, undefined} = get_driver_setup("riak_norm"),
    {fmke_db_driver_riak_kv_norm, undefined} = get_driver_setup(riak_norm).

correct_redis_driver_setup_test() ->
    {fmke_kv_driver, fmke_db_driver_redis} = get_driver_setup("redis"),
    {fmke_kv_driver, fmke_db_driver_redis} = get_driver_setup(redis).

parse_ipv4_tuple_address_test() ->
    "1.2.3.4" = read_tuple_address({1, 2, 3, 4}),
    "123.245.167.98" = read_tuple_address({123, 245, 167, 098}).

parse_ipv6_tuple_address_test() ->
    "ab:cd:ef:bb:cc:dd:ee:ff" = read_tuple_address({"ab", "cd", "ef", "bb", "cc", "dd", "ee", "ff"}).

parse_empty_address_list_test() ->
    {error, no_addresses} = parse_db_address_list([]).

parse_address_list_with_one_element_test() ->
    {ok, ["1.2.3.4"]} = parse_db_address_list(["1.2.3.4"]).

parse_address_list_from_single_address_test() ->
    {ok, ["1.2.3.4"]} = parse_db_address_list({1, 2, 3, 4}),
    {ok, ["1.2.3.4"]} = parse_db_address_list("1.2.3.4").

parse_address_list_from_multiple_addresses_test() ->
    {ok, ["1.2.3.4", "4.3.2.1"]} = parse_db_address_list("1.2.3.4 4.3.2.1"),
    {ok, ["1.2.3.4", "4.3.2.1"]} = parse_db_address_list(["1.2.3.4", {4, 3, 2, 1}]),
    {ok, ["1.2.3.4", "4.3.2.1"]} = parse_db_address_list([{1, 2, 3, 4}, {4, 3, 2, 1}]),
    {ok, ["1.2.3.4", "Aa:fe:1001:cd:FF:12:23:34"]} = parse_db_address_list([
        {1, 2, 3, 4}, {"Aa", "fe", "1001", "cd", "FF", "12", "23", "34"}
    ]).

parse_empty_port_list_test() ->
    {error, no_ports} = parse_db_port_list([]).

parse_port_list_with_one_element_test() ->
    {ok, [8087]} = parse_db_port_list([8087]).

parse_port_list_from_single_port_test() ->
    {ok, [6173]} = parse_db_port_list("6173"),
    {ok, [8087]} = parse_db_port_list(8087).

parse_port_list_from_multiple_ports_test() ->
    {ok, [8087, 8187]} = parse_db_port_list("8087 8187"),
    {ok, [8087, 8187]} = parse_db_port_list([8087, 8187]),
    {ok, [8087, 8187]} = parse_db_port_list([8087, "8187"]).

-endif.
