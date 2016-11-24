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
    Dispatch = cowboy_router:compile([
  		{'_', [
  			{"/prescriptions/[:id]", prescription_handler, []},
        {"/patients/[:id]", patient_handler, []},
        {"/pharmacies/[:id]", pharmacy_handler, []},
        {"/facilities/[:id]", facility_handler, []},
        {"/treatments/[:id]", treatment_handler, []},
        {"/events/[:id]", event_handler, []},
        {"/staff/[:id]", staff_handler, []}
  		]}
  	]),
  	{ok, _} = cowboy:start_clear(http, 100, [{port, 9090}], #{
  		env => #{dispatch => Dispatch}
  	}),
    case fmk_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
%%              case open_antidote_socket() of
%%                ok -> {ok, Pid};
%%                _ -> {error, cannot_open_protobuff_socket}
%%              end;
        {error, Reason} ->
              {error, Reason}
    end.

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
    {ok, AntidoteNodeAddress} = application:get_env(fmk, antidote_ip),
    {ok, AntidoteNodePort} = application:get_env(fmk, antidote_port),
    set_application_variable(antidote_address,"ANTIDOTE_ADDRESS",AntidoteNodeAddress),
    set_application_variable(antidote_port,"ANTIDOTE_PORT", AntidoteNodePort),
    {ok, _} =antidote_pool:start([{hostname, AntidoteNodeAddress}, {port, AntidoteNodePort}]),
    ok.

close_antidote_socket() ->
    AntidotePbPid = fmk_config:get(?VAR_ANTIDOTE_PB_PID,undefined),
    case AntidotePbPid of
        undefined ->
            {error, error_closing_pb_socket};
        _SomethingElse ->
            ok
    end.
