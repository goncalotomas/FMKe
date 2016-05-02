%%%-------------------------------------------------------------------
%% @doc fmk public API
%% @end
%%%-------------------------------------------------------------------

-module(fmk_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Response = fmk_sup:start_link(),
    io:format("FMK server starting, testing if antidote is up...~n"),
    fmk_core:test_antidote(),
    io:format("Tests completed, server started.~n"),
    Response.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
