%%%-------------------------------------------------------------------
%% @doc fmk public API
%% @end
%%%-------------------------------------------------------------------

-module(fmk_app).
-include("fmk.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    %HACK: Short-circuit over DB_DRIVER.
    %TODO: Pass configurable parameters.
    Args = case ?KV_IMPLEMENTATION of
               fmke_db_driver_antidote -> [];
               fmke_db_driver_riak_kv -> {["127.0.0.1"],[8087], 'riak@127.0.0.1', riak}
           end,

    Result = ?DB_DRIVER:init(Args),
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", fmk_handler, []},
        {"/prescriptions/[:id]", prescription_handler, []},
        {"/patients/[:id]", patient_handler, []},
        {"/pharmacies/[:id]", pharmacy_handler, []},
        {"/pharmacies/[:id]/prescriptions", pharmacy_handler, prescriptions},
        {"/pharmacies/[:id]/processed_prescriptions", pharmacy_handler, processed_prescriptions},
        {"/facilities/[:id]", facility_handler, []},
        {"/treatments/[:id]", treatment_handler, []},
        {"/events/[:id]", event_handler, []},
        {"/staff/[:id]", staff_handler, []},
        {"/staff/[:id]/prescriptions", staff_handler, prescriptions}
      ]}
    ]),
    set_application_variable(http_port,"HTTP_PORT",?DEFAULT_FMKE_HTTP_PORT),
    HttpPort = list_to_integer(fmk_config:get(http_port,9090)),
    {ok, _} = cowboy:start_clear(http, 100, [{port, HttpPort}], #{
      env => #{dispatch => Dispatch}
    }),
    Result.

%%--------------------------------------------------------------------
stop(_State) ->
    ?DB_DRIVER:stop([]).

%%====================================================================
%% Internal functions
%%====================================================================
set_application_variable(ApplicationVariable, EnvironmentVariable, EnvironmentDefault) ->
  %% try to load value from environment variable
  Default = os:getenv(EnvironmentVariable, EnvironmentDefault),
  Value = application:get_env(?APP,ApplicationVariable,Default),
  fmk_config:set(ApplicationVariable,Value),
  Value.
