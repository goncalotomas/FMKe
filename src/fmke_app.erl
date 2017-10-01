%%%-------------------------------------------------------------------
%% @doc fmk public API
%% @end
%%%-------------------------------------------------------------------

-module(fmke_app).
-include("fmke.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    InitParams = fmke_config:read_config_file(),

    HttpPort = proplists:get_value(http_port, InitParams),
    DriverType = proplists:get_value(driver_type, InitParams),

    Result = DriverType:init(InitParams),
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", fmke_http_handler_app, []},
        {"/prescriptions/[:id]", fmke_http_handler_prescriptions, []},
        {"/patients/[:id]", fmke_http_handler_patients, []},
        {"/pharmacies/[:id]", fmke_http_handler_pharmacies, []},
        {"/pharmacies/[:id]/prescriptions", fmke_http_handler_pharmacies, prescriptions},
        {"/pharmacies/[:id]/processed_prescriptions", fmke_http_handler_pharmacies, processed_prescriptions},
        {"/facilities/[:id]", fmke_http_handler_facilities, []},
        {"/treatments/[:id]", fmke_http_handler_treatments, []},
        {"/events/[:id]", fmke_http_handler_events, []},
        {"/staff/[:id]", fmke_http_handler_staff, []},
        {"/staff/[:id]/prescriptions", fmke_http_handler_staff, prescriptions}
      ]}
    ]),

    {ok, _} = cowboy:start_clear(fmke_http_listener, [{port, HttpPort}],
      #{env => #{dispatch => Dispatch}}
    ),
    Result.

%%--------------------------------------------------------------------
stop(_State) ->
    (fmke_config:get(driver_type)):stop([]),
    cowboy:stop_listener(fmke_http_listener).
