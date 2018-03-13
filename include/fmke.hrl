-define (APP, fmke).
-define (OPTIONS, [
    adapter, connection_pool_size, database_addresses, database_ports, http_port,
    target_database, data_model, optimized_driver
]).
-define (DEFAULTS, #{
    adapter => fmke_kv_adapter,
    connection_pool_size => 64,
    database_addresses => ["127.0.0.1"],
    database_ports => [8087],
    http_port => 9090,
    target_database => riak,
    data_model => nested,
    optimized_driver => false
}).

-define(TIMEOUT, 60000).

-define(CONFIG_FILE_PATH, "/config/fmke.config").
-define(ETS_TABLE_NAME, fmke_ets).

%% TODO move this to an ETS table
-define(SUPPORTED_DBS, [antidote, antidote_norm, riak_kv, riak_kv_norm, redis]).
-define(SUPPORTED_KVS, [antidote, antidote_norm, riak_kv, riak_kv_norm, redis]).

-record(prescription, {
    id :: id()
    ,patient_id :: id()
    ,pharmacy_id :: id()
    ,prescriber_id :: id()
    ,date_prescribed :: field()
    ,date_processed = <<"undefined">> :: field()
    ,drugs :: list(field())
    ,is_processed = <<"prescription_not_processed">> :: field()
}).

-record(patient, {
    id :: id()
    ,name :: string()
    ,address :: string()
    ,prescriptions = [] :: list(#prescription{})
    % ,treatments=[] :: list(#treatment{})
    % ,events=[] :: list(#event{})
}).

-record(pharmacy, {
    id :: id()
    ,name :: string()
    ,address :: string()
    ,prescriptions = [] :: list(#prescription{})
}).

-record(facility, {
    id :: id()
    ,name :: string()
    ,address :: string()
    ,type :: string()
    % ,treatments=[] :: list(#treatment{})
    % ,events=[] :: list(#event{})
}).

-record(staff, {
    id :: id()
    ,name :: string()
    ,address :: string()
    ,speciality :: string()
    ,prescriptions = [] :: list(#prescription{})
}).

-type id() :: non_neg_integer().
-type field() :: binary().
-type reason() :: term().
-type crdt() :: term().
-type app_record() :: #facility{} |
                      #patient{} |
                      #pharmacy{} |
                      #prescription{} |
                      #staff{}.
-type entity() :: facility | patient | pharmacy | prescription | staff.
