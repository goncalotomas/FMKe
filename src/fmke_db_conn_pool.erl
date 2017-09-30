%% Heavily inspired in Peter Zeller's previous module antidote_pool.
%% This module manages connections between databases and FMKe.
-module(fmke_db_conn_pool).
-author("Gonçalo Tomás <goncalo@goncalotomas.com>").

-behaviour(poolboy_worker).
-behaviour(supervisor).

%% API
-export([start/1, with_connection/1]).

%% Supervisor callbacks
-export([init/1]).

%% Poolboy callbacks
-export([start_link/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Options]).

with_connection(Fun) ->
    poolboy:transaction(fmke_db_connection_pool, Fun).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Params]) ->
    ListConnHostnames = proplists:get_value(db_conn_hostnames,Params),
    ListConnPorts = proplists:get_value(db_conn_ports,Params),
    ConnModule = proplists:get_value(db_conn_module,Params),
    ConnPoolSize = proplists:get_value(db_conn_pool_size,Params),
    PoolArgs = [
      {name, {local, fmke_db_connection_pool}},
      {worker_module, ?MODULE},
      {size, ConnPoolSize},
      {max_overflow, 0}
    ],
    WorkerArgs = [ConnModule,ListConnHostnames,ListConnPorts],
    PoolSpec = poolboy:child_spec(fmke_db_connection_pool, PoolArgs, WorkerArgs),
    {ok, {{one_for_one, 10, 10}, [PoolSpec]}}.

start_link([Module,ListHostnames,ListPorts]) ->
    true = (Len = length(ListHostnames)) =:= length(ListPorts),
    Index = rand:uniform(Len),
    Hostname = lists:nth(Index,ListHostnames),
    Element = lists:nth(Index,ListPorts),
    Port =
      case is_integer(Element) of
          true -> Element;
          false -> list_to_integer(Element)
      end,
    try_connect(Module,Hostname, Port, 100).

try_connect(Module,Hostname,Port,Timeout) ->
    case Module:start_link(Hostname, Port) of
        {ok, Pid} ->
            % io:format("Connected to ~p:~p --> ~p ~n", [Hostname, Port, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Could not connect to ~p:~p, Reason: ~p~n", [Hostname, Port, Reason]),
            timer:sleep(Timeout),
            try_connect(Module, Hostname, Port, min(10000, Timeout*2))
    end.
