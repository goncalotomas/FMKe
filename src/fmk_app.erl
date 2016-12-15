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
    Result = case fmk_sup:start_link() of
        {ok, Pid} ->
              case open_antidote_socket() of
                {ok,_} -> {ok, Pid};
                Err -> {error, {cannot_open_protobuff_socket, Err}}
              end;
        {error, Reason} ->
              {error, Reason}
    end,
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
    HttpPort = list_to_integer(fmk_config:get(http_port,9090)),
    {ok, _} = cowboy:start_clear(http, 100, [{port, HttpPort}], #{
      env => #{dispatch => Dispatch}
    }),
    Result.

%%--------------------------------------------------------------------
stop(_State) ->
    close_antidote_socket().

%%====================================================================
%% Internal functions
%%====================================================================

set_application_variable(ApplicationVariable, EnvironmentVariable, EnvironmentDefault) ->
  %% try to load value from environment variable
  Default = os:getenv(EnvironmentVariable, EnvironmentDefault),
  Value = application:get_env(?APP,ApplicationVariable,Default),
  fmk_config:set(ApplicationVariable,Value),
  Value.

open_antidote_socket() ->
    set_application_variable(http_port,"HTTP_PORT",?DEFAULT_FMKE_HTTP_PORT),
    set_application_variable(antidote_address,"ANTIDOTE_ADDRESS",?DEFAULT_ANTIDOTE_ADDRESS),
    set_application_variable(antidote_port,"ANTIDOTE_PB_PORT",?DEFAULT_ANTIDOTE_PORT),
    AntidoteNodeAddress = fmk_config:get_env(?VAR_ANTIDOTE_PB_ADDRESS,?DEFAULT_ANTIDOTE_ADDRESS),
    AntidoteNodePort = fmk_config:get_env(?VAR_ANTIDOTE_PB_PORT,?DEFAULT_ANTIDOTE_PORT),
    antidote_pool:start([{hostname, AntidoteNodeAddress}, {port, AntidoteNodePort}]).

close_antidote_socket() ->
    AntidotePbPid = fmk_config:get(?VAR_ANTIDOTE_PB_PID,undefined),
    case AntidotePbPid of
        undefined ->
            {error, error_closing_pb_socket};
        _SomethingElse ->
            ok
    end.
