%% ----------------------------------------------------------------------------
%% fmke_sup: supervise the FMKe application
%% Stars 3 children:
%% - cowboy: web server
%% - fmke: application
%% - fmke_setup_sup: supervisor for the driver setup
%% ----------------------------------------------------------------------------

-module(fmke_sup).

-behaviour(supervisor).

-include ("fmke.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% useful config funs
-import(fmke_driver_config, [
    driver_adapter/1,
    default_driver/1,
    db_from_driver/1
]).

-define(SERVER, ?MODULE).

-define(KV_ADAPTER, fmke_kv_adapter).
-define(SQL_ADAPTER, fmke_sql_adapter).
-define(PASS_THROUGH_ADAPTER, fmke_pt_adapter).

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
    %% read required parameters from app environment
    {ok, HttpPort} = application:get_env(?APP, http_port),
    Adapter = fmke_driver_config:selected_adapter(),

    RestartStrategy = #{strategy => one_for_one, intensity => 10, period => 10},
    {ok, {RestartStrategy, [
        gen_web_server_spec(HttpPort),
        fmke_spec(Adapter),
        setup_sup_spec()
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec gen_web_server_spec(HttpPort::non_neg_integer()) -> supervisor:child_spec().
gen_web_server_spec(HttpPort) ->
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", fmke_http_handler_app, []},
        {"/prescriptions/[:id]", fmke_http_handler_prescriptions, []},
        {"/patients/[:id]", fmke_http_handler_patients, []},
        {"/pharmacies/[:id]", fmke_http_handler_pharmacies, []},
        {"/pharmacies/[:id]/prescriptions", fmke_http_handler_pharmacies, prescriptions},
        {"/pharmacies/[:id]/processed_prescriptions", fmke_http_handler_pharmacies, processed_prescriptions},
        {"/facilities/[:id]", fmke_http_handler_facilities, []},
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

-spec fmke_spec(module()) -> supervisor:child_spec().
fmke_spec(Adapter) ->
    #{
        id => fmke,
        start => {fmke, start_link, [[Adapter]]},
        restart => permanent,
        type => worker
    }.

-spec setup_sup_spec() -> supervisor:child_spec().
setup_sup_spec() ->
    #{
        id => setup_sup,
        start => {fmke_setup_sup, start_link, [[]]},
        restart => permanent,
        type => supervisor
    }.

config_env() ->
    try
        {ok, CurrentDirectory} = file:get_cwd(),
        ConfigFile = CurrentDirectory ++ ?CONFIG_FILE_PATH,
        lager:info("Trying to fetch config from ~p...~n", [ConfigFile]),
        {ok, AppProps} = file:consult(ConfigFile),
        config(AppProps)
    catch
        Error:Reason:Stack ->
            lager:error("Error reading from config file: ~p:~p~nStacktrace: ~p~n", [Error, Reason, Stack]),
            lager:info("Could not read from config file, reverting to environment and default values..."),
            config([])
    end.

%% Sets all options needed to start FMKe, from the 4 following sources, ordered by priority:
%% OS Environment, Application Environment, config file, default value
config(Config) ->
    Driver = get_option(driver, Config),
    Database = get_option(target_database, Config),
    PoolSize = get_option(connection_pool_size, Config),
    Addresses = get_option(database_addresses, Config),
    Ports = get_option(database_ports, Config),
    HttpPort = get_option(http_port, Config),
    Model = get_option(data_model, Config),
    set_opts([
        {driver, Driver},
        {target_database, Database},
        {connection_pool_size, PoolSize},
        {database_addresses, Addresses},
        {database_ports, Ports},
        {http_port, HttpPort},
        {data_model, Model}
    ]).

set_opts([]) ->
    ok;

set_opts([{_Opt, undefined} | Rest]) ->
    set_opts(Rest);

set_opts([{Opt, Val} | Rest]) ->
    config(Opt, Val),
    set_opts(Rest).

config(data_model, Model) ->
    maybe_config(data_model, Model);
config(http_port, HttpPort) ->
    maybe_config(http_port, HttpPort);
config(target_database, Database) ->
    maybe_config(target_database, Database),
    Driver = default_driver(Database),
    maybe_config(driver, Driver),
    case driver_adapter(Driver) of
        none ->
            ok;
        Adapter ->
            maybe_config(adapter, Adapter)
    end;
config(database_ports, Ports) ->
    maybe_config(database_ports, Ports);
config(database_addresses, Addresses) ->
    maybe_config(database_addresses, Addresses);
config(connection_pool_size, Size) ->
    maybe_config(connection_pool_size, Size);
config(driver, Driver) ->
    maybe_config(driver, Driver),
    case driver_adapter(Driver) of
        none ->
            ok;
        Adapter ->
            maybe_config(adapter, Adapter)
    end,
    maybe_config(target_database, db_from_driver(Driver)).

maybe_config(Key, Val) ->
    case application:get_env(?APP, Key) of
        undefined ->
            lager:info("Setting FMKe option ~p = ~p~n", [Key, Val]),
            ok = application:set_env(?APP, Key, Val);
        {ok, Predefined} ->
            lager:info("Setting FMKe option ~p failed (already defined as ~p)~n", [Key, Predefined]),
            already_defined
    end.

get_option(Opt, Config) ->
    {_Source, Val} = get_value(os:getenv(atom_to_list(Opt)),
                               application:get_env(?APP, Opt),
                               proplists:get_value(Opt, Config),
                               maps:get(Opt, ?DEFAULTS, undefined)),
    Val.

get_value(false, undefined, undefined, Val) ->      {defaults, Val};
get_value(false, undefined, Val, _) ->              {config_file, Val};
get_value(false, {ok, Val}, _, _) ->                {app_env, Val};
get_value(Val, _, _, _) ->                          {os_env, Val}.
