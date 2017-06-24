-module (fmk_config).
-include ("fmk.hrl").
-export ([
    get/2,
    get_env/2,
    set/2,
    set_from_env/3
]).

get(Key, Default) ->
    fmke_mochiglobal:get(Key, Default).

get_env(Key, Default) ->
    application:get_env(?APP, Key, Default).

set(Key, Value) ->
    application:set_env(?APP, Key, Value),
    fmke_mochiglobal:put(Key, Value).

set_from_env(VarName, EnvVarName, DefaultVal) ->
    %% try to load value from environment variable
    EnvOrDefault = os:getenv(EnvVarName, DefaultVal),
    ParsedValue = parse_app_var(VarName,EnvOrDefault),
    set(VarName,ParsedValue),
    io:format("FMKe application variable '~p' set to value ~p~n",[VarName,ParsedValue]).

parse_app_var(db_conn_hostnames,ListAddresses) when is_list(ListAddresses), (length(ListAddresses) > 0) ->
    [list_to_atom(X) || X <- ListAddresses];

parse_app_var(db_conn_hostnames,ListAddresses) ->
    [list_to_atom(X) || X <- parse_list_from_env_var(ListAddresses)];

parse_app_var(db_conn_ports,ListPorts) when is_list(ListPorts), length(ListPorts) > 0 ->
    case is_integer(lists:nth(1,ListPorts)) of
        true ->
            [X || X <- ListPorts];
        false -> [list_to_integer(X) || X <- ListPorts]
    end;

parse_app_var(db_conn_ports,ListPorts) ->
    parse_list_from_env_var(ListPorts);

parse_app_var(http_port,Port) when is_list(Port) ->
    parse_app_var(http_port,list_to_integer(Port));

parse_app_var(http_port,Port) when is_integer(Port) ->
    Port.

parse_list_from_env_var(String) ->
    io:format("RECEIVED: ~p\n",[String]),
    try
      string:tokens(String,",") %% CSV style
    catch
      _:_  ->
        bad_input_format
    end.
