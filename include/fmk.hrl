%%-define(ANTIDOTE, 'antidote@127.0.0.1').
-define(DB_DRIVER, fmke_kv_driver).
-define(KV_IMPLEMENTATION, fmke_db_driver_riak_kv).
-define (DEFAULT_FMKE_HTTP_PORT, 9090).
-define (APP, fmke).

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
    ,is_processed = <<"not_processed">> :: field()
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
