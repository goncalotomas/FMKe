%% Heavily inspired in Peter Zeller's previous module antidote_pool.
%% This module manages connections between databases and FMKe.
-module(fmke_db_connection).
-author("Gonçalo Tomás <goncalo@goncalotomas.com>").
-include("fmke.hrl").

-behaviour(poolboy_worker).

%% poolboy_worker callback
-export([start_link/1]).

start_link([{client_lib, Module}, {host, Host}, {port, Port}]) ->
    try_connect(Module, Host, Port, 100).

try_connect(Module, Hostname, Port, Timeout) ->
    case Module:start_link(Hostname, Port) of
      {ok, Pid} ->
          lager:debug("Connected to ~p:~p --> ~p ~n", [Hostname, Port, Pid]),
          {ok, Pid};
      {error, Reason} ->
          lager:error("Could not connect to ~p:~p, Reason: ~p~n", [Hostname, Port, Reason]),
          timer:sleep(Timeout),
          try_connect(Module, Hostname, Port, min(10000, Timeout*2))
    end.
