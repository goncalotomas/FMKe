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

    {DriverType, DriverImpl} = get_driver_setup(TargetDatabase),

    %% set application variables from config file
    set(http_port,HttpPort),
    set(connection_pool_size,ConnPoolSize),
    set(driver_type,DriverType),
    set(driver_implementation,DriverImpl),
    set(db_conn_hostnames,ParsedAddressList),
    set(db_conn_ports,ParsedPortList),

    [
        {driver_type, DriverType},
        {driver_implementation, DriverImpl},
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
    case lists:member(Database, ?SUPPORTED_KVS) of
        true -> {fmke_kv_driver, list_to_atom("fmke_db_driver_" ++ atom_to_list(Database))};
        false ->
            case is_alias_of_database(Database) of
                {true,ValidDbHandle} ->
                    {fmke_kv_driver, list_to_atom("fmke_db_driver_" ++ atom_to_list(ValidDbHandle))};
                false ->
                    {error, not_supported, Database}
            end
    end.

is_alias_of_database(riak) ->
    {true, riak_kv};
is_alias_of_database(_DatabaseName) ->
    false.

parse_db_address_list(DbAddressList) ->
    case io_lib:printable_unicode_list(DbAddressList) of
        false ->
            parse_db_address_list_rec(DbAddressList,[]);
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
        true -> parse_db_address_list_rec(T,lists:append(Accum,[read_tuple_address(H)]));
        false ->
            case io_lib:printable_unicode_list(H) of
                false -> {error, invalid_format};
                true -> parse_db_address_list_rec(T,lists:append(Accum,[H]))
            end
    end.

read_tuple_address({_1,_2,_3,_4}) when is_integer(_1) andalso is_integer(_2)
    andalso is_integer(_3) andalso is_integer(_4) ->
        integer_to_list(_1) ++ "." ++ integer_to_list(_2) ++ "." ++
        integer_to_list(_3) ++ "." ++ integer_to_list(_4);

read_tuple_address({_1,_2,_3,_4,_5,_6}) when is_integer(_1) andalso is_integer(_2)
    andalso is_integer(_3) andalso is_integer(_4) andalso is_integer(_5) andalso is_integer(_6) ->
        integer_to_list(_1) ++ "." ++ integer_to_list(_2) ++ "." ++
        integer_to_list(_3) ++ "." ++ integer_to_list(_4) ++ "." ++
        integer_to_list(_5) ++ "." ++ integer_to_list(_6).

parse_db_port_list(DbPortList) ->
    parse_db_port_list_rec(DbPortList,[]).

parse_db_port_list_rec([], Accum) ->
    case length(Accum) > 0 of
        true -> {ok, Accum};
        false -> {error, no_ports}
    end;
parse_db_port_list_rec([H|T], Accum) ->
    case is_integer(H) of
        true -> parse_db_port_list_rec(T,lists:append(Accum,[H]));
        false ->
            case io_lib:printable_unicode_list(H) of
                false -> {error, invalid_format};
                true -> parse_db_port_list_rec(T,lists:append(Accum,[list_to_integer(H)]))
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
