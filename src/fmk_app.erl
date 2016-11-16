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
    case fmk_sup:start_link() of
        {ok, Pid} ->
              case open_antidote_socket() of
                ok -> {ok, Pid};
                _ -> {error, cannot_open_protobuff_socket}
              end;
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
    set_application_variable(antidote_address,"ANTIDOTE_ADDRESS",?DEFAULT_ANTIDOTE_ADDRESS),
    set_application_variable(antidote_port,"ANTIDOTE_PORT",?DEFAULT_ANTIDOTE_PORT),
    AntidoteNodeAddress = fmk_config:get_env(?VAR_ANTIDOTE_PB_ADDRESS,?DEFAULT_ANTIDOTE_ADDRESS),
    AntidoteNodePort = fmk_config:get_env(?VAR_ANTIDOTE_PB_PORT,?DEFAULT_ANTIDOTE_PORT),
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
