%%-define(ANTIDOTE, 'antidote@127.0.0.1').
-define(DB_DRIVER, fmke_kv_driver).
-define(KV_IMPLEMENTATION, antidote_kv_driver).
-define (DEFAULT_FMKE_HTTP_PORT, 9090).
-define (APP, fmke).

%% Type specification borrowed from antidote
-type txid() :: {pid(), antidote:txid()}.
-type reason() :: antidote:reason().
-type snapshot_time() :: antidote:snapshot_time().
-type bound_object() :: antidote:bound_object().
-type op_name() :: antidote:op_name().
-type op_param() :: antidote:op_param().
-type crdt() :: term().
-type crdt_op() :: any().
-type field() :: binary().
-type map_field_op() ::  {remove, field()}.
-type map_field_update() :: {update, field(), crdt_op()}.
-type map_op() :: {update, {[map_field_update() | map_field_op()], actorordot()}}.
-type actorordot() :: antidote_crdt:actor() | antidote_crdt:dot().
-type object_bucket() :: {field(), crdt(), term()}.
-type id() :: non_neg_integer().
-define (MAP_UPDATE_OP,update).

%% Test macros
-define(TEST_COUNTER_TYPE, antidote_crdt_pncounter).
-define(TEST_COUNTER_KEY, fmk_counter_test).
-define(TEST_MAP_KEY, 'fmk_map_test').
-define(TEST_NESTED_MAP_KEY, 'fmk_nested_map_test').
