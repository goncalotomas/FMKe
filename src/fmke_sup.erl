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
config(Config) ->
    Driver = get_option(driver, Config),
    Database = get_option(target_database, Config),
    PoolSize = get_option(connection_pool_size, Config),
    Addresses = get_option(database_addresses, Config),
    Ports = get_option(database_ports, Config),
    HttpPort = get_option(http_port, Config),
    Model = get_option(data_model, Config),
    config(driver, {Driver, Database}),
    config(adapter, {driver, Driver}),
    config(target_database, fmke_driver_config:db_from_driver(Driver)),
    config(pool_size, PoolSize),
    config(database_addresses, Addresses),
    config(database_ports, Ports),
    config(http_port, HttpPort),
    config(data_model, Model).

config(data_model, Model) ->
    maybe_config(data_model, Model);
config(http_port, HttpPort) ->
    maybe_config(http_port, HttpPort);
config(target_database, Database) ->
    maybe_config(target_database, Database);
config(database_ports, Ports) ->
    maybe_config(database_ports, Ports);
config(database_addresses, Addresses) ->
    maybe_config(database_addresses, Addresses);
config(pool_size, Size) ->
    maybe_config(connection_pool_size, Size);
config(adapter, {driver, Driver}) ->
    maybe_config(adapter, fmke_driver_config:driver_adapter(Driver));
config(driver, {undefined, Database}) ->
    maybe_config(driver, fmke_driver_config:default_driver(Database));
config(driver, {Driver, _Database}) ->
    maybe_config(driver, Driver).

maybe_config(Key, undefined) ->
    lager:info("Unable to set ~p (value undefined)~n", [Key]),
    ok;
maybe_config(Key, Val) ->
    lager:info("Setting FMKe option ~p = ~p~n", [Key, Val]),
    ok = application:set_env(?APP, Key, Val).

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
