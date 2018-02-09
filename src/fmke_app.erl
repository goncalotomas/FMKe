%%%-------------------------------------------------------------------
%% @doc fmk public API
%% @end
%%%-------------------------------------------------------------------

-module(fmke_app).
-include("fmke.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    fmke_sup:start_link(StartArgs).

stop(_State) ->
    ok.
