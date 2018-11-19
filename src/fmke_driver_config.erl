-module(fmke_driver_config).

-include("fmke.hrl").

-define(KV_ADAPTER, fmke_kv_adapter).
-define(SQL_ADAPTER, fmke_sql_adapter).
-define(PT_ADAPTER, fmke_pt_adapter).

-export([
    default_driver/1,
    driver_adapter/1,
    get_client_lib/1,
    is_opt_driver/1,
    requires_conn_manager/1,
    requires_ets_table/1,
    selected_driver/0,
    selected_adapter/0
]).

-type database() :: atom().

-spec selected_driver() -> module().
selected_driver() ->
    case application:get_env(?APP, driver) of
        undefined ->
            {ok, Database} = application:get_env(?APP, target_database),
            default_driver(Database);
        {ok, RequestedDriver} ->
            RequestedDriver
    end.

-spec selected_adapter() -> module().
selected_adapter() ->
    driver_adapter(selected_driver()).

-spec requires_conn_manager(Driver::module()) -> true | false.
requires_conn_manager(fmke_driver_antidote) ->              true;
requires_conn_manager(fmke_driver_opt_antidote) ->          true;
requires_conn_manager(fmke_driver_opt_redis_crdb) ->        true;
requires_conn_manager(fmke_driver_opt_riak_kv) ->           true;
requires_conn_manager(fmke_driver_riak_kv) ->               true;
requires_conn_manager(_Driver) ->                           false.

-spec default_driver(Database::database()) -> module().
default_driver(antidote) ->         fmke_driver_opt_antidote;
default_driver(cassandra) ->        fmke_driver_opt_cassandra;
default_driver(ets) ->              fmke_driver_ets;
default_driver(redis) ->            fmke_driver_opt_redis_crdb;
default_driver(redis_crdb) ->       fmke_driver_opt_redis_crdb;
default_driver(redis_cluster) ->    fmke_driver_opt_redis_cluster;
default_driver(riak) ->             fmke_driver_opt_riak_kv.

-spec is_opt_driver(Driver::module()) -> boolean().
is_opt_driver(fmke_driver_antidote) ->      false;
is_opt_driver(fmke_driver_ets) ->           false;
is_opt_driver(fmke_driver_riak_kv) ->       false;
is_opt_driver(_Driver) ->                   true.

-spec is_sql_driver(Driver::module()) -> boolean().
is_sql_driver(fmke_driver_postgres) -> true;
is_sql_driver(_Driver) -> false.

-spec requires_ets_table(Driver::module()) -> true | false.
requires_ets_table(fmke_driver_ets) ->          true;
requires_ets_table(_Driver) ->                  false.

-spec get_client_lib(Driver::module()) -> atom().
get_client_lib(fmke_driver_antidote) ->             antidotec_pb_socket;
get_client_lib(fmke_driver_opt_antidote) ->         antidotec_pb_socket;
get_client_lib(fmke_driver_opt_redis_crdb) ->       eredis;
get_client_lib(fmke_driver_opt_riak_kv) ->          riakc_pb_socket;
get_client_lib(fmke_driver_riak) ->                 riakc_pb_socket.

-spec driver_adapter(Driver::module()) -> module().
driver_adapter(Driver) ->
    case is_opt_driver(Driver) of
        false ->
            sql_or_kv_adapter(Driver);
        true ->
            ?PT_ADAPTER
    end.

sql_or_kv_adapter(Driver) ->
    case is_sql_driver(Driver) of
        false ->
            ?KV_ADAPTER;
        true ->
            ?SQL_ADAPTER
    end.
