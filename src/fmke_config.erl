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

parse_db_address_list(DbAddressList) ->
    case io_lib:printable_unicode_list(DbAddressList) of
        false ->
            parse_db_address_list_rec(DbAddressList, []);
        true ->
            Split = string:split(DbAddressList, " "),
            case Split of
                [[]] -> {error, no_addresses};
                _Other -> {ok, Split}
            end
    end.

parse_db_address_list_rec([], Accum) ->
    case length(Accum) > 0 of
        true -> {ok, Accum};
        false -> {error, no_addresses}
    end;
parse_db_address_list_rec([H|T], Accum) ->
    case is_tuple(H) of
        true -> parse_db_address_list_rec(T, lists:append(Accum, [read_tuple_address(H)]));
        false ->
            case io_lib:printable_unicode_list(H) of
                false -> {error, invalid_format};
                true -> parse_db_address_list_rec( T, lists:append(Accum, [H]))
            end
    end.

read_tuple_address({A, B, C, D}) when is_integer(A) andalso is_integer(B) andalso is_integer(C) andalso is_integer(D) ->
    integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D);

read_tuple_address({A, B, C, D, E, F, G, H}) when is_list(A) andalso is_list(B) andalso is_list(C) andalso is_list(D)
    andalso is_list(E) andalso is_list(F) andalso is_list(G) andalso is_list(H) ->
        integer_to_list(A) ++ ":" ++ integer_to_list(B) ++ ":" ++ integer_to_list(C) ++ ":" ++ integer_to_list(D) ++ ":"
        ++ integer_to_list(E) ++ ":" ++ integer_to_list(F) ++ integer_to_list(G) ++ ":" ++ integer_to_list(H).

parse_db_port_list(DbPortList) ->
    parse_db_port_list_rec(DbPortList, []).

parse_db_port_list_rec([], Accum) ->
    case length(Accum) > 0 of
        true -> {ok, Accum};
        false -> {error, no_ports}
    end;
parse_db_port_list_rec([H|T], Accum) ->
    case is_integer(H) of
        true -> parse_db_port_list_rec(T, lists:append(Accum, [H]));
        false ->
            case io_lib:printable_unicode_list(H) of
                false -> {error, invalid_format};
                true -> parse_db_port_list_rec(T, lists:append(Accum, [list_to_integer(H)]))
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
