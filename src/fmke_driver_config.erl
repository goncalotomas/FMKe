-module(fmke_driver_config).

-include("fmke.hrl").

-type database() :: atom().

-define(KV_ADAPTER, fmke_kv_adapter).


-export([
    db_from_driver/1,
    default_driver/1,
    driver_adapter/1,
    get_client_lib/1,
    is_opt_driver/1,
    is_simple_kv_driver/1,
    requires_conn_manager/1,
    requires_ets_table/1,
    selected_driver/0,
    selected_adapter/0
]).

%% Stores the default drivers for each database.
%% These typically only get updated once a new database is supported.
-define(DEFAULT_DRIVER, #{
    aql =>              fmke_driver_opt_aql,
    antidote =>         fmke_driver_opt_antidote,
    cassandra =>        fmke_driver_opt_cassandra,
    ets =>              fmke_driver_ets,
    redis =>            fmke_driver_opt_redis_crdb,
    redis_crdb =>       fmke_driver_opt_redis_crdb,
    redis_cluster =>    fmke_driver_opt_redis_cluster,
    riak =>             fmke_driver_opt_riak_kv
}).

%% Add your driver to this list if you wish to use FMKe's connection manager
-define(REQUIRE_CONN_MANAGER, [
    fmke_driver_opt_aql,
    fmke_driver_opt_antidote,
    fmke_driver_opt_redis_crdb,
    fmke_driver_opt_riak_kv
]).

%% Add your driver to this list if you wish to have an ETS table created at boot
-define(REQUIRE_ETS, [
    fmke_driver_ets
]).

%% Add your driver to this list if your driver implements only the simple KV
%% interface and not the entire FMKe API. Drivers that are not on this list
%% or on the SIMPLE_SQL_DRIVERS are assumed to implement the entire FMKe API.
-define(SIMPLE_KV_DRIVERS, [
    fmke_driver_ets
]).

-spec selected_driver() -> module().
selected_driver() ->
    {ok, Driver} = application:get_env(?APP, driver),
    Driver.

-spec selected_adapter() -> module().
selected_adapter() ->
    driver_adapter(selected_driver()).

-spec requires_conn_manager(Driver::module()) -> true | false.
requires_conn_manager(Driver) ->
    lists:member(Driver, ?REQUIRE_CONN_MANAGER) .

-spec default_driver(Database::database()) -> module().
default_driver(Database) ->
    maps:get(Database, ?DEFAULT_DRIVER).

-spec is_opt_driver(Driver::module()) -> boolean().
is_opt_driver(Driver) ->
    not lists:member(Driver, ?SIMPLE_KV_DRIVERS).

-spec is_simple_kv_driver(Driver::module()) -> boolean().
is_simple_kv_driver(Driver) ->
    lists:member(Driver, ?SIMPLE_KV_DRIVERS).

-spec requires_ets_table(Driver::module()) -> true | false.
requires_ets_table(Driver) ->
    lists:member(Driver, ?REQUIRE_ETS).

-spec get_client_lib(Driver::module()) -> atom().
get_client_lib(fmke_driver_opt_aql) ->              aqlc_tcp;
get_client_lib(fmke_driver_opt_antidote) ->         antidotec_pb_socket;
get_client_lib(fmke_driver_opt_redis_crdb) ->       eredis;
get_client_lib(fmke_driver_opt_riak_kv) ->          riakc_pb_socket.

-spec driver_adapter(Driver::module()) -> module().
driver_adapter(Driver) ->
    case is_opt_driver(Driver) of
        false ->
            ?KV_ADAPTER;
        true ->
            none
    end.

db_from_driver(fmke_driver_ets) -> ets;
db_from_driver(fmke_driver_opt_aql) -> aql;
db_from_driver(fmke_driver_opt_antidote) -> antidote;
db_from_driver(fmke_driver_opt_riak_kv) -> riak;
db_from_driver(fmke_driver_opt_cassandra) -> cassandra;
db_from_driver(fmke_driver_opt_redis_crdb) -> redis_crdb;
db_from_driver(fmke_driver_opt_redis_cluster) -> redis_cluster.
