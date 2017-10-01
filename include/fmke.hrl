-define (APP, fmke).

-define (CONFIG_FILE_PATH, "/../../../../config/fmke.config").

-define(SUPPORTED_DBS, [antidote, riak_kv, redis]).
-define(SUPPORTED_KVS, [antidote, riak_kv, redis]).

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
