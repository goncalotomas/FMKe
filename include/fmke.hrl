-define(APP, fmke).
-define(OPTIONS, [
    target_database, connection_pool_size, database_addresses, database_ports, http_port, driver
]).
-define(DEFAULTS, #{
    connection_pool_size => 64,
    database_addresses => ["127.0.0.1"],
    http_port => 9090,
    data_model => non_nested
}).
-define(DEFAULT(Opt), maps:get(Opt, ?DEFAULTS, undefined)).

-define(TIMEOUT, 60000).

-define(CONFIG_FILE_PATH, "/config/fmke.config").
-define(ETS_TABLE_NAME, fmke_ets).

%% TODO move this to an ETS table
-define(SUPPORTED_DBS, [antidote, antidote_norm, riak_kv, riak_kv_norm, redis]).
-define(SUPPORTED_KVS, [antidote, antidote_norm, riak_kv, riak_kv_norm, redis]).

-record(prescription, {
    id :: id() | binary()
    ,patient_id :: id() | binary()
    ,pharmacy_id :: id() | binary()
    ,prescriber_id :: id() | binary()
    ,date_prescribed :: field()
    ,date_processed = <<"undefined">> :: field()
    ,drugs :: list(string() | binary())
    ,is_processed = <<"prescription_not_processed">> :: field()
}).

-record(patient, {
    id :: id()
    ,name :: string()
    ,address :: string()
    ,prescriptions = [] :: list(#prescription{} | key())
    % ,treatments=[] :: list(#treatment{})
    % ,events=[] :: list(#event{})
}).

-record(pharmacy, {
    id :: id()
    ,name :: string()
    ,address :: string()
    ,prescriptions = [] :: list(#prescription{} | key())
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
    ,prescriptions = [] :: list(#prescription{} | key())
}).

-type id() :: non_neg_integer().
-type field() :: binary().
-type reason() :: term().
-type crdt() :: term().
-type key() :: binary().
-type app_record() :: #facility{} |
                      #patient{} |
                      #pharmacy{} |
                      #prescription{} |
                      #staff{} |
                      list(key()).

-type entity() :: facility | patient | pharmacy | prescription | staff.

-type facility() :: #facility{}.
-type patient() :: #patient{}.
-type pharmacy() :: #pharmacy{}.
-type prescription() :: #prescription{}.
-type staff() :: #staff{}.
