-module(fmke_db_conn_sup).

-behaviour(supervisor).

-include("fmke.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init([Pools, Connections, Module, PoolSize]) ->
    ok = application:set_env(?APP, pools, Pools),
    RestartStrategy = #{strategy => one_for_one, intensity => 10, period => 10},
    Children = [gen_conn_pool_mgr_spec()] ++ gen_pool_specs(Pools, Connections, Module, PoolSize),
    {ok, {RestartStrategy, Children}}.


-spec gen_conn_pool_mgr_spec() -> supervisor:child_spec().
gen_conn_pool_mgr_spec() ->
    #{
        id => fmke_db_conn_manager,
        start => {fmke_db_conn_manager, start_link, []},
        restart => permanent,
        type => worker
    }.

-spec gen_pool_specs(Pools::list(atom()), Connections::list({list(), non_neg_integer()}), Module::module(),
                     PoolSize::non_neg_integer()) -> list(supervisor:child_spec()).
gen_pool_specs(Pools, Connections, Module, PoolSize) ->
    lists:map(
        fun({Pool, Connection}) ->
            SizeArgs = [{size, PoolSize}, {max_overflow, 2 * PoolSize}],
            {Host, Port} = Connection,
            WorkerArgs = [{client_lib, Module}, {host, Host}, {port, Port}],
            PoolArgs = [{name, {local, Pool}}, {worker_module, fmke_db_connection}] ++ SizeArgs,
            poolboy:child_spec(Pool, PoolArgs, WorkerArgs)
        end,
        lists:zip(Pools, Connections)).
