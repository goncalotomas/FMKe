-define (APP, fmke).

-define (CONFIG_FILE_PATH, "/config/fmke.config").
-define (DEFAULT_HTTP_PORT, 9090).
-define (DEFAULT_CONN_SIZE, 32).

%% TODO move this to an ETS table
-define(SUPPORTED_DBS, [antidote, antidote_norm, riak_kv, riak_kv_norm, redis]).
-define(SUPPORTED_KVS, [antidote, antidote_norm, riak_kv, riak_kv_norm, redis]).

-type id() :: non_neg_integer().
-type field() :: binary().
-type reason() :: term().
-type crdt() :: term().

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
