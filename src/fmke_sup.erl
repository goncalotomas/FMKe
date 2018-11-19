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
config(ConfigProps) ->
    lists:foreach(
        fun(Param) ->
            {Source, Value} = get_value(os:getenv(atom_to_list(Param)), application:get_env(?APP, Param),
                                        proplists:get_value(Param, ConfigProps), maps:get(Param, ?DEFAULTS)),
            lager:info("~p option '~p' read from ~p, set to ~p", [?APP, Param, Source, Value]),
            ok = application:set_env(?APP, Param, Value)
        end,
    ?OPTIONS).

get_value(false, undefined, undefined, Val) ->      {defaults, Val};
get_value(false, undefined, Val, _) ->              {config_file, Val};
get_value(false, {ok, Val}, _, _) ->                {app_env, Val};
get_value(Val, _, _, _) ->                          {os_env, Val}.
