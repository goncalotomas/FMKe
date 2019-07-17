%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for AntidoteDB, featuring a normalized data model (no CRDT nesting).
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_antidote).

% -behaviour(fmke_gen_driver).
-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server exports
-export ([
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

-define(ANTIDOTE_TRANSACTION_RETRIES, 3).
-define(MAP, antidote_crdt_map_go).
-define(LWWREG, antidote_crdt_register_lww).
-define(ORSET, antidote_crdt_set_aw).

%% Type definitions
-type update() :: {{binary(), atom(), binary()}, atom(), any()}.
-type txid() :: {pid(), antidote:txid()}.
% -type snapshot_time() :: antidote:snapshot_time().
-type bound_object() :: antidote:bound_object().
-type op_name() :: antidote:op_name().
-type op_param() :: antidote:op_param().
-type crdt_op() :: any().
% -type map_field_update() :: {update, field(), crdt_op()}.
% -type actorordot() :: antidote_crdt:actor() | antidote_crdt:dot().
-type object_bucket() :: {field(), crdt(), term()}.
% -type map_field_op() ::  {remove, field()}.
% -type map_op() :: {update, {[map_field_update() | map_field_op()], actorordot()}}.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    {ok, _Started} = application:ensure_all_started(antidotec_pb),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, State) ->
    Reply = call(Op),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, State}.

%% handle_call callbacks

call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}) ->
    %% gather required antidote keys
    PrescriptionKey = gen_key(prescription, Id),
    PatientKey = gen_key(patient, PatientId),
    PharmacyKey = gen_key(pharmacy, PharmacyId),
    PrescriberKey = gen_key(staff, PrescriberId),
    Txn = txn_start(),
    %% check required pre-conditions
    case missing_keys(check_keys(Txn, [{patient, PatientId}, {pharmacy, PharmacyId}, {staff, PrescriberId}])) of
        {true, Entity} ->
            txn_commit(Txn),
            {error, no_such_entity(Entity)};
        false ->
            PrescriptionFields = [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
            {HandleCreateOpResult, Txn2} = create_if_not_exists(prescription, PrescriptionFields, Txn),
            case HandleCreateOpResult of
                ok ->
                    %% build updates for patients, pharmacies, facilities and the prescriber
                    %% these are already generated as buckets
                    PatientUpdate = gen_entity_update(add_entity_prescription, [PatientKey, PrescriptionKey]),
                    PharmacyUpdate = gen_entity_update(add_entity_prescription, [PharmacyKey, PrescriptionKey]),
                    PrescriberUpdate = gen_entity_update(add_entity_prescription, [PrescriberKey, PrescriptionKey]),
                    txn_update_objects([PharmacyUpdate, PrescriberUpdate, PatientUpdate], Txn2),
                    txn_commit(Txn2);
                Error ->
                    txn_commit(Txn2),
                    Error
            end
    end;

call({create, Entity, Fields}) ->
    create_if_not_exists(Entity, Fields);

call({update, prescription, Id, {date_processed, DateProcessed}}) ->
    %% process prescription
    Txn = txn_start(),
    PrescriptionKey = gen_key(prescription, Id),
    Result = case build_app_record(prescription, process_get_request(PrescriptionKey, ?MAP, Txn)) of
          {error, not_found} ->
              {error, no_such_prescription};
          #prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE} ->
              {error, prescription_already_processed};
          #prescription{is_processed=?PRESCRIPTION_NOT_PROCESSED_VALUE} ->
              IsProcessedOp = build_lwwreg_op(?PRESCRIPTION_IS_PROCESSED_KEY, ?LWWREG, ?PRESCRIPTION_PROCESSED_VALUE),
              ProcessedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PROCESSED_KEY, ?LWWREG, DateProcessed),
              PrescriptionUpdate = {create_bucket(PrescriptionKey, ?MAP), update, [IsProcessedOp, ProcessedOp]},
              txn_update_objects([PrescriptionUpdate], Txn),
              ok
    end,
    case txn_commit(Txn) of
        ok ->
            Result;
        Other ->
            Other
    end;

call({update, prescription, Id, {drugs, add, Drugs}}) ->
    %% process prescription
    Txn = txn_start(),
    Result = case build_app_record(prescription, process_get_request(gen_key(prescription, Id), ?MAP, Txn)) of
          {error, not_found} ->
              {error, no_such_prescription};
          #prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE} ->
              {error, prescription_already_processed};
          #prescription{is_processed=?PRESCRIPTION_NOT_PROCESSED_VALUE} ->
              PrescriptionSetOp = {add_all, lists:map(fun(Drug) -> list_to_binary(Drug) end, Drugs)},
              UpdateOperation = [build_map_op(?PRESCRIPTION_DRUGS_KEY, ?ORSET, PrescriptionSetOp)],
              put(gen_key(prescription, Id), ?MAP, update, UpdateOperation, Txn),
              ok
    end,
    case txn_commit(Txn) of
        ok ->
            Result;
        Other ->
            Other
    end;

call({update, Entity, Fields}) ->
    update_if_already_exists(Entity, Fields);

call({read, Entity, Id}) when Entity =:= patient; Entity =:= pharmacy; Entity =:= staff ->
    Txn = txn_start(),
    EntityKey = gen_key(Entity, Id),
    BinaryEntity = list_to_binary(atom_to_list(Entity)),
    [EntityFields, Prescriptions] = multi_read([
        create_bucket(EntityKey, ?MAP),
        create_bucket(<<EntityKey/binary, "_prescriptions">>, ?ORSET)
    ], Txn),
    Result = case Prescriptions of
        {error, not_found} ->
            build_app_record(Entity, EntityFields);
        Keys ->
            build_app_record(Entity, [
                {{<<BinaryEntity/binary, "_prescriptions">>, ?ORSET}, Keys} | EntityFields
            ])
    end,
    txn_commit(Txn),
    Result;

call({read, Entity, Id}) ->
    build_app_record(Entity, process_get_request(gen_key(Entity, Id), ?MAP));

call({read, Entity, Id, prescriptions}) ->
    Key = gen_key(Entity, Id),
    Txn = txn_start(),
    Result = case build_app_record(Entity, process_get_request(Key, ?MAP, Txn)) of
        {error, not_found} ->
            {error, no_such_entity(Entity)};
        _ ->
            [Prescriptions] = multi_read([
              create_bucket(<<Key/binary, "_prescriptions">>, ?ORSET)
            ], Txn),
            case Prescriptions of
                {error, not_found} -> [];
                Keys -> Keys
            end
    end,
    txn_commit(Txn),
    Result;

call({read, Entity, Id, processed_prescriptions}) ->
    Key = gen_key(Entity, Id),
    Txn = txn_start(),
    Result = case build_app_record(Entity, process_get_request(Key, ?MAP, Txn)) of
        {error, not_found} ->
            {error, no_such_entity(Entity)};
        _ ->
            [Prescriptions] = multi_read([
              create_bucket(<<Key/binary, "_prescriptions">>, ?ORSET)
            ], Txn),
            case Prescriptions of
                {error, not_found} -> [];
                Keys ->
                    Buckets = lists:map(fun(K) -> create_bucket(K, ?MAP) end, Keys),
                    ReadResults = multi_read(Buckets, Txn),
                    PrescObjs = lists:map(fun(Obj) -> build_app_record(prescription, Obj) end, ReadResults),
                    lists:filter(fun(Presc) ->
                                        Presc#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE
                                    end, PrescObjs)
            end
    end,
    txn_commit(Txn),
    Result;

call({read, prescription, Id, drugs}) ->
    Prescription = build_app_record(prescription, process_get_request(gen_key(prescription, Id), ?MAP)),
    case Prescription of
        {error, _} -> {error, no_such_prescription};
        _ -> Prescription#prescription.drugs
    end.

missing_keys([]) ->
    false;
missing_keys([{taken, _Key} | T]) ->
    missing_keys(T);
missing_keys([{free, {Entity, _Key}} | _T]) ->
    {true, Entity}.

create_if_not_exists(Entity, Fields) ->
    Txn = txn_start(),
    {Result, Txn2} = create_if_not_exists(Entity, Fields, Txn),
    ok = txn_commit(Txn2),
    Result.

create_if_not_exists(Entity, Fields, Txn) ->
    Id = hd(Fields),
    Result =
      case check_id(Entity, Id, Txn) of
        taken ->
            {error, id_taken(Entity)};
        free ->
            EntityKey = gen_key(Entity, Id),
            EntityUpdate = gen_entity_update(Entity, Fields),
            execute_create_op(EntityKey, EntityUpdate, Txn)
      end,
    {Result, Txn}.

%% Does kind of the opposite of create_if_not_exists/2
update_if_already_exists(Entity, Fields) ->
    Txn = txn_start(),
    Id = hd(Fields),
    Result =
      case check_id(Entity, Id, Txn) of
        free ->
            {error, no_such_entity(Entity)};
        taken ->
            EntityKey = gen_key(Entity, Id),
            EntityUpdate = gen_entity_update(Entity, Fields),
            execute_create_op(EntityKey, EntityUpdate, Txn)
      end,
    ok = txn_commit(Txn),
    Result.

%% Checks if an entity exists
check_id(Entity, Id, Txn) ->
    case build_app_record(Entity, process_get_request(gen_key(Entity, Id), ?MAP, Txn)) of
        {error, not_found} -> free;
        _Map -> taken
    end.


gen_entity_update(patient, [Id, Name, Address]) ->
    IdOp = build_id_op(?PATIENT_ID_KEY, ?LWWREG, Id),
    NameOp = build_lwwreg_op(?PATIENT_NAME_KEY, ?LWWREG, Name),
    AddressOp = build_lwwreg_op(?PATIENT_ADDRESS_KEY, ?LWWREG, Address),
    [IdOp, NameOp, AddressOp];
gen_entity_update(pharmacy, [Id, Name, Address]) ->
    IdOp = build_id_op(?PHARMACY_ID_KEY, ?LWWREG, Id),
    NameOp = build_lwwreg_op(?PHARMACY_NAME_KEY, ?LWWREG, Name),
    AddressOp = build_lwwreg_op(?PHARMACY_ADDRESS_KEY, ?LWWREG, Address),
    [IdOp, NameOp, AddressOp];
gen_entity_update(facility, [Id, Name, Address, Type]) ->
    IdOp = build_id_op(?FACILITY_ID_KEY, ?LWWREG, Id),
    NameOp = build_lwwreg_op(?FACILITY_NAME_KEY, ?LWWREG, Name),
    AddressOp = build_lwwreg_op(?FACILITY_ADDRESS_KEY, ?LWWREG, Address),
    TypeOp = build_lwwreg_op(?FACILITY_TYPE_KEY, ?LWWREG, Type),
    [IdOp, NameOp, AddressOp, TypeOp];
gen_entity_update(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]) ->
    IdOp = build_id_op(?PRESCRIPTION_ID_KEY, ?LWWREG, Id),
    PatientOp = build_id_op(?PRESCRIPTION_PATIENT_ID_KEY, ?LWWREG, PatientId),
    PharmacyOp = build_id_op(?PRESCRIPTION_PHARMACY_ID_KEY, ?LWWREG, PharmacyId),
    PrescriberOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID_KEY, ?LWWREG, PrescriberId),
    DatePrescribedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED_KEY, ?LWWREG, DatePrescribed),
    IsProcessedOp = build_lwwreg_op(?PRESCRIPTION_IS_PROCESSED_KEY, ?LWWREG, ?PRESCRIPTION_NOT_PROCESSED_VALUE),
    DrugsOp = build_map_op(?PRESCRIPTION_DRUGS_KEY, ?ORSET,
    {add_all, lists:map(fun list_to_binary/1, Drugs)}),
    [IdOp, PatientOp, PharmacyOp, PrescriberOp, DatePrescribedOp, IsProcessedOp, DrugsOp];
gen_entity_update(staff, [Id, Name, Address, Speciality]) ->
    IdOp = build_id_op(?STAFF_ID_KEY, ?LWWREG, Id),
    NameOp = build_lwwreg_op(?STAFF_NAME_KEY, ?LWWREG, Name),
    AddressOp = build_lwwreg_op(?STAFF_ADDRESS_KEY, ?LWWREG, Address),
    SpecialityOp = build_lwwreg_op(?STAFF_SPECIALITY_KEY, ?LWWREG, Speciality),
    [IdOp, NameOp, AddressOp, SpecialityOp];
gen_entity_update(add_entity_prescription, [EntityKey, PrescriptionKey]) ->
    {create_bucket(<<EntityKey/binary, "_prescriptions">>, ?ORSET), add, PrescriptionKey}.

execute_create_op(Key, Op, Txn) ->
    put(Key, ?MAP, update, Op, Txn).

%% Searches for a Value within a map that is associated with a specific key.
%% All antidote_crdt_map entries are of type {{key_name,key_type},Value}. Having this function avoids
%% repeating the following code numerous times when searching for an element within a map.
find_key(Map, Key, KeyType, FallbackValue) ->
  try lists:keyfind({Key, KeyType}, 1, Map) of
    false -> FallbackValue;
    {{Key, KeyType}, Value} -> Value
  catch
    _:_ -> FallbackValue
  end.

build_app_record(_, {error, not_found}) ->
  {error, not_found};
build_app_record(patient, Object) ->
  Id = find_key(Object, ?PATIENT_ID_KEY, ?LWWREG, -1),
  Name = find_key(Object, ?PATIENT_NAME_KEY, ?LWWREG, <<"undefined">>),
  Address = find_key(Object, ?PATIENT_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
  Prescriptions = find_key(Object, ?PATIENT_PRESCRIPTIONS_KEY, ?ORSET, []),
  #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
build_app_record(pharmacy, Object) ->
  Id = find_key(Object, ?PHARMACY_ID_KEY, ?LWWREG, -1),
  Name = find_key(Object, ?PHARMACY_NAME_KEY, ?LWWREG, <<"undefined">>),
  Address = find_key(Object, ?PHARMACY_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
  Prescriptions = find_key(Object, ?PHARMACY_PRESCRIPTIONS_KEY, ?ORSET, []),
  #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
build_app_record(staff, Object) ->
  Id = find_key(Object, ?STAFF_ID_KEY, ?LWWREG, -1),
  Name = find_key(Object, ?STAFF_NAME_KEY, ?LWWREG, <<"undefined">>),
  Address = find_key(Object, ?STAFF_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
  Speciality = find_key(Object, ?STAFF_SPECIALITY_KEY, ?LWWREG, <<"undefined">>),
  Prescriptions = find_key(Object, ?STAFF_PRESCRIPTIONS_KEY, ?ORSET, []),
  #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions};
build_app_record(facility, Object) ->
  Id = find_key(Object, ?FACILITY_ID_KEY, ?LWWREG, -1),
  Name = find_key(Object, ?FACILITY_NAME_KEY, ?LWWREG, <<"undefined">>),
  Address = find_key(Object, ?FACILITY_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
  Type = find_key(Object, ?FACILITY_TYPE_KEY, ?LWWREG, <<"undefined">>),
  #facility{id = Id, name = Name, address = Address, type = Type};
build_app_record(prescription, Object) ->
  Id = find_key(Object, ?PRESCRIPTION_ID_KEY, ?LWWREG, -1),
  PatientId = find_key(Object, ?PRESCRIPTION_PATIENT_ID_KEY, ?LWWREG, <<"undefined">>),
  PrescriberId = find_key(Object, ?PRESCRIPTION_PRESCRIBER_ID_KEY, ?LWWREG, <<"undefined">>),
  PharmacyId = find_key(Object, ?PRESCRIPTION_PHARMACY_ID_KEY, ?LWWREG, <<"undefined">>),
  DatePrescribed = find_key(Object, ?PRESCRIPTION_DATE_PRESCRIBED_KEY, ?LWWREG, <<"undefined">>),
  IsProcessed = find_key(Object, ?PRESCRIPTION_IS_PROCESSED_KEY, ?LWWREG, ?PRESCRIPTION_NOT_PROCESSED_VALUE),
  DateProcessed = find_key(Object, ?PRESCRIPTION_DATE_PROCESSED_KEY, ?LWWREG, <<"undefined">>),
  Drugs = find_key(Object, ?PRESCRIPTION_DRUGS_KEY, ?ORSET, []),
  #prescription{
      id = Id,
      patient_id = PatientId,
      pharmacy_id = PharmacyId,
      prescriber_id = PrescriberId,
      date_prescribed = DatePrescribed,
      date_processed = DateProcessed,
      drugs = Drugs,
      is_processed = IsProcessed
  }.

multi_read(Objects, Txn) ->
  Results = txn_read_objects(Objects, Txn),
  lists:map(fun parse_read_result/1, Results).

check_keys(_Context, []) ->
    [];
check_keys(Context, [H|T]) ->
    {Entity, Id} = H,
    case process_get_request(gen_key(Entity, Id), ?MAP, Context) of
        {error, not_found} -> [{free, H}] ++ check_keys(Context, T);
        _Object -> [{taken, H}] ++ check_keys(Context, T)
    end.

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------

process_get_request(Key, Type) ->
    parse_read_result(get(Key, Type)).

process_get_request(Key, Type, Txn) ->
    parse_read_result(get(Key, Type, Txn)).

parse_read_result({_Crdt, []}) -> {error, not_found};
parse_read_result({timeout, _}) -> {error, timeout};
parse_read_result({_Crdt, Object}) -> Object;
parse_read_result(_) -> erlang:error(unknown_object_type).

%% ------------------------------------------------------------------------------------------------
%% Antidote's transaction API wrapper - Use when you need fine grain control over transactions
%% Please refer to the official Antidote transaction documentation for reference on these functions
%% ------------------------------------------------------------------------------------------------

%% A wrapper for Antidote's start_transaction function
-spec txn_start() -> txid().
txn_start() ->
    Pid = fmke_db_conn_manager:checkout(),
    {ok, TxnDetails} = antidotec_pb:start_transaction(Pid, ignore),
    {Pid, TxnDetails}.

%% A wrapper for Antidote's read_objects function, with a single object being read.
-spec txn_read_object(Object :: bound_object(), TxnDetails :: txid()) -> term().
txn_read_object(Object, {Pid, TxnDetails}) ->
    {ok, [Value]} = antidotec_pb:read_values(Pid, [Object], TxnDetails),
    Value.

%% A wrapper for Antidote's read_objects function
-spec txn_read_objects(Objects :: list(bound_object()), TxnDetails :: txid()) -> [term()].
txn_read_objects(Objects, {Pid, TxnDetails}) ->
    {ok, Values} = antidotec_pb:read_values(Pid, Objects, TxnDetails),
    Values.

%% A wrapper for Antidote's update_objects function, with a single object being written.
-spec txn_update_object({bound_object(), op_name(), op_param()}, txid()) -> ok.
txn_update_object(ObjectUpdate, {Pid, TxnDetails}) ->
    ok = antidotec_pb:update_objects(Pid, [ObjectUpdate], TxnDetails).

%% A wrapper for Antidote's update_objects function
-spec txn_update_objects([update()], txid()) -> ok.
txn_update_objects(ObjectUpdates, {Pid, TxnDetails}) ->
    ok = antidotec_pb:update_objects(Pid, ObjectUpdates, TxnDetails).

%% A wrapper for Antidote's commit_transaction function
-spec txn_commit(TxnDetails :: txid()) -> ok | {error, term()}.
txn_commit({Pid, TxnDetails}) ->
    Result = case antidotec_pb:commit_transaction(Pid, TxnDetails) of
        {ok, _} ->
            ok;
        {error, Error} ->
            lager:warning("Antidote transaction failed: ~p~n", [Error]),
            {error, aborted}
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result.

%% ------------------------------------------------------------------------------------------------
%% Simple API - Recommended way to interact with Antidote
%% ------------------------------------------------------------------------------------------------

%% Creates an Antidote bucket of a certain type.
-spec create_bucket(field(), crdt()) -> object_bucket().
create_bucket(Key, Type) ->
    {Key, Type, <<"bucket">>}.

%% A simple way of getting information from antidote, just requiring a key and key-type.
-spec get(field(), crdt()) -> term().
get(Key, Type) ->
    Bucket = create_bucket(Key, Type),
    TxnDetails = txn_start(),
    ReadResult = txn_read_object(Bucket, TxnDetails),
    ok = txn_commit(TxnDetails),
    ReadResult.

%% Alternative to get/2, using an already existing transaction ID.
%% NOTE: This does not commit the ongoing transaction!
-spec get(field(), crdt(), txid()) -> term().
get(Key, Type, Txn) ->
    Bucket = create_bucket(Key, Type),
    txn_read_object(Bucket, Txn).

%% Similar to put/4, but with transactional context.
-spec put(field(), crdt(), crdt_op(), op_param(), txid()) -> ok | {error, reason()}.
put(Key, Type, Op, Param, Txn) ->
    Bucket = create_bucket(Key, Type),
    ok = txn_update_object({Bucket, Op, Param}, Txn).

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying constructing CRDT operations
%%-----------------------------------------------------------------------------
%% Builds an Antidote acceptable map operation, taking a key, key-type, and the actual operation.
-spec build_map_op(field(), crdt(), crdt_op()) -> term().
build_map_op(Key, Type, Op) ->
    {{Key, Type}, Op}.

build_id_op(Key, KeyType, Id) ->
    build_lwwreg_op(Key, KeyType, integer_to_list(Id)).

build_lwwreg_op(Key, KeyType, Value) ->
    build_map_op(Key, KeyType, {assign, Value}).

no_such_entity(facility) ->     no_such_facility;
no_such_entity(patient) ->      no_such_patient;
no_such_entity(pharmacy) ->     no_such_pharmacy;
no_such_entity(prescription) -> no_such_prescription;
no_such_entity(staff) ->        no_such_staff.

id_taken(facility) ->       facility_id_taken;
id_taken(patient) ->        patient_id_taken;
id_taken(pharmacy) ->       pharmacy_id_taken;
id_taken(prescription) ->   prescription_id_taken;
id_taken(staff) ->          staff_id_taken.

gen_key(facility, Id) ->        binary_str_w_int("facility_", Id);
gen_key(patient, Id) ->         binary_str_w_int("patient_", Id);
gen_key(pharmacy, Id) ->        binary_str_w_int("pharmacy_", Id);
gen_key(prescription, Id) ->    binary_str_w_int("prescription_", Id);
gen_key(staff, Id) ->           binary_str_w_int("staff_", Id).

binary_str_w_int(Str, Int) when is_binary(Int) ->
    list_to_binary(unicode:characters_to_list([Str, binary_to_list(Int)]));
binary_str_w_int(Str, Int) when is_integer(Int) ->
    list_to_binary(unicode:characters_to_list([Str, integer_to_list(Int)])).
