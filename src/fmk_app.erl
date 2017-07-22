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
    Result = ?DB_DRIVER:init({}),
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
    fmk_config:set_from_env(http_port,"HTTP_PORT",?DEFAULT_FMKE_HTTP_PORT),
    HttpPort = fmk_config:get(http_port,9090),
    {ok, _} = cowboy:start_http(fmke_http_listener, 100, [{port, HttpPort}],
      [{env, [{dispatch, Dispatch}]}]
    ),
    Result.

%%--------------------------------------------------------------------
stop(_State) ->
    ?DB_DRIVER:stop([]),
    cowboy:stop_listener(fmke_http_listener).
