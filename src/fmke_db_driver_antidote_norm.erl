%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for AntidoteDB, featuring a normalized data model (no CRDT nesting).
%% ---------------------------------------------------------------------------------------------------------------------
-module (fmke_db_driver_antidote_norm).

-behaviour (fmke_gen_driver).

-include ("fmke.hrl").
-include ("fmk_kv.hrl").

-export([
  start/1,
  stop/1,
  create_patient/3,
  create_pharmacy/3,
  create_facility/4,
  create_staff/4,
  create_prescription/6,
  create_event/5,
  create_treatment/5,
  create_treatment/6,
  get_event_by_id/1,
  get_facility_by_id/1,
  get_facility_by_id/2,
  get_patient_by_id/1,
  get_patient_by_id/2,
  get_pharmacy_by_id/1,
  get_pharmacy_by_id/2,
  get_processed_pharmacy_prescriptions/1,
  get_pharmacy_prescriptions/1,
  get_prescription_by_id/1,
  get_prescription_by_id/2,
  get_prescription_medication/1,
  get_staff_by_id/1,
  get_staff_by_id/2,
  get_staff_prescriptions/1,
  get_staff_treatments/1,
  get_treatment_by_id/1,
  process_prescription/2,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  update_staff_details/4,
  update_prescription_medication/3
  , error_to_binary/1
]).

-define (MAP, antidote_crdt_gmap).
-define (ORSET, antidote_crdt_orset).

%% Patient macros
-define (PATIENT_ID_CRDT, antidote_crdt_lwwreg).
-define (PATIENT_NAME_CRDT, antidote_crdt_lwwreg).
-define (PATIENT_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (PATIENT_PRESCRIPTIONS_CRDT, antidote_crdt_orset).

%% Pharmacy macros
-define (PHARMACY_ID_CRDT, antidote_crdt_lwwreg).
-define (PHARMACY_NAME_CRDT, antidote_crdt_lwwreg).
-define (PHARMACY_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (PHARMACY_PRESCRIPTIONS_CRDT, antidote_crdt_orset).

%% Prescription macros
-define (PRESCRIPTION_ID_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_PATIENT_ID_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_PRESCRIBER_ID_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_PHARMACY_ID_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_DATE_PRESCRIBED_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_IS_PROCESSED_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_DATE_PROCESSED_CRDT, antidote_crdt_lwwreg).
-define (PRESCRIPTION_DRUGS_CRDT, antidote_crdt_orset).

%% Medical Staff macros
-define (STAFF_ID_CRDT, antidote_crdt_lwwreg).
-define (STAFF_NAME_CRDT, antidote_crdt_lwwreg).
-define (STAFF_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (STAFF_SPECIALITY_CRDT, antidote_crdt_lwwreg).
-define (STAFF_PRESCRIPTIONS_CRDT, antidote_crdt_orset).

%% Facility macros
-define (FACILITY_ID_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_NAME_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_TYPE_CRDT, antidote_crdt_lwwreg).

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

start(_) ->
    ok.

stop(_) ->
    ok.

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
            {error, list_to_atom(lists:flatten(io_lib:format("~p_id_taken", [Entity])))};
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
            {error, list_to_atom(lists:flatten(io_lib:format("no_such_~p", [Entity])))};
        taken ->
            EntityKey = gen_key(Entity, Id),
            EntityUpdate = gen_entity_update(Entity, Fields),
            execute_create_op(EntityKey, EntityUpdate, Txn)
      end,
    ok = txn_commit(Txn),
    Result.

%% Checks if an entity exists
check_id(Entity, Id, Txn) ->
  OpName = list_to_atom(lists:flatten(io_lib:format("get_~p_by_id", [Entity]))),
  case erlang:apply(?MODULE, OpName, [Id, Txn]) of
    {error, not_found} -> free;
    _Map -> taken
  end.

gen_key(Entity, Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p", [Entity, Id]))).

gen_entity_update(patient, [Id, Name, Address]) ->
    IdOp = build_id_op(?PATIENT_ID_KEY, ?PATIENT_ID_CRDT, Id),
    NameOp = build_lwwreg_op(?PATIENT_NAME_KEY, ?PATIENT_NAME_CRDT, Name),
    AddressOp = build_lwwreg_op(?PATIENT_ADDRESS_KEY, ?PATIENT_ADDRESS_CRDT, Address),
    [IdOp, NameOp, AddressOp];
gen_entity_update(pharmacy, [Id, Name, Address]) ->
    IdOp = build_id_op(?PHARMACY_ID_KEY, ?PHARMACY_ID_CRDT, Id),
    NameOp = build_lwwreg_op(?PHARMACY_NAME_KEY, ?PHARMACY_NAME_CRDT, Name),
    AddressOp = build_lwwreg_op(?PHARMACY_ADDRESS_KEY, ?PHARMACY_ADDRESS_CRDT, Address),
    [IdOp, NameOp, AddressOp];
gen_entity_update(facility, [Id, Name, Address, Type]) ->
    IdOp = build_id_op(?FACILITY_ID_KEY, ?FACILITY_ID_CRDT, Id),
    NameOp = build_lwwreg_op(?FACILITY_NAME_KEY, ?FACILITY_NAME_CRDT, Name),
    AddressOp = build_lwwreg_op(?FACILITY_ADDRESS_KEY, ?FACILITY_ADDRESS_CRDT, Address),
    TypeOp = build_lwwreg_op(?FACILITY_TYPE_KEY, ?FACILITY_TYPE_CRDT, Type),
    [IdOp, NameOp, AddressOp, TypeOp];
gen_entity_update(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]) ->
    IdOp = build_id_op(?PRESCRIPTION_ID_KEY, ?PRESCRIPTION_ID_CRDT, Id),
    PatientOp = build_id_op(?PRESCRIPTION_PATIENT_ID_KEY, ?PRESCRIPTION_PATIENT_ID_CRDT, PatientId),
    PharmacyOp = build_id_op(?PRESCRIPTION_PHARMACY_ID_KEY, ?PRESCRIPTION_PHARMACY_ID_CRDT, PharmacyId),
    PrescriberOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID_KEY, ?PRESCRIPTION_PRESCRIBER_ID_CRDT, PrescriberId),
    DatePrescribedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED_KEY, ?PRESCRIPTION_DATE_PRESCRIBED_CRDT,
    DatePrescribed),
    IsProcessedOp = build_lwwreg_op(?PRESCRIPTION_IS_PROCESSED_KEY, ?PRESCRIPTION_IS_PROCESSED_CRDT,
    ?PRESCRIPTION_NOT_PROCESSED_VALUE),
    DrugsOp = build_map_op(?PRESCRIPTION_DRUGS_KEY, ?PRESCRIPTION_DRUGS_CRDT,
    {add_all, lists:map(fun list_to_binary/1, Drugs)}),
    [IdOp, PatientOp, PharmacyOp, PrescriberOp, DatePrescribedOp, IsProcessedOp, DrugsOp];
gen_entity_update(staff, [Id, Name, Address, Speciality]) ->
    IdOp = build_id_op(?STAFF_ID_KEY, ?STAFF_ID_CRDT, Id),
    NameOp = build_lwwreg_op(?STAFF_NAME_KEY, ?STAFF_NAME_CRDT, Name),
    AddressOp = build_lwwreg_op(?STAFF_ADDRESS_KEY, ?STAFF_ADDRESS_CRDT, Address),
    SpecialityOp = build_lwwreg_op(?STAFF_SPECIALITY_KEY, ?STAFF_SPECIALITY_CRDT, Speciality),
    [IdOp, NameOp, AddressOp, SpecialityOp];
gen_entity_update(remove_entity_prescription, [EntityKey, PrescriptionKey]) ->
    {create_bucket(<<EntityKey/binary, "_prescriptions">>, ?ORSET), remove, PrescriptionKey};
gen_entity_update(add_entity_processed_prescription, [EntityKey, PrescriptionKey]) ->
    {create_bucket(<<EntityKey/binary, "_prescriptions_processed">>, ?ORSET), add, PrescriptionKey};
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
  Id = find_key(Object, ?PATIENT_ID_KEY, ?PATIENT_ID_CRDT, -1),
  Name = find_key(Object, ?PATIENT_NAME_KEY, ?PATIENT_NAME_CRDT, <<"undefined">>),
  Address = find_key(Object, ?PATIENT_ADDRESS_KEY, ?PATIENT_ADDRESS_CRDT, <<"undefined">>),
  Prescriptions = find_key(Object, ?PATIENT_PRESCRIPTIONS_KEY, ?PATIENT_PRESCRIPTIONS_CRDT, []),
  #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
build_app_record(pharmacy, Object) ->
  Id = find_key(Object, ?PHARMACY_ID_KEY, ?PHARMACY_ID_CRDT, -1),
  Name = find_key(Object, ?PHARMACY_NAME_KEY, ?PHARMACY_NAME_CRDT, <<"undefined">>),
  Address = find_key(Object, ?PHARMACY_ADDRESS_KEY, ?PHARMACY_ADDRESS_CRDT, <<"undefined">>),
  Prescriptions = find_key(Object, ?PHARMACY_PRESCRIPTIONS_KEY, ?PHARMACY_PRESCRIPTIONS_CRDT, []),
  #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
build_app_record(staff, Object) ->
  Id = find_key(Object, ?STAFF_ID_KEY, ?STAFF_ID_CRDT, -1),
  Name = find_key(Object, ?STAFF_NAME_KEY, ?STAFF_NAME_CRDT, <<"undefined">>),
  Address = find_key(Object, ?STAFF_ADDRESS_KEY, ?STAFF_ADDRESS_CRDT, <<"undefined">>),
  Speciality = find_key(Object, ?STAFF_SPECIALITY_KEY, ?STAFF_SPECIALITY_CRDT, <<"undefined">>),
  Prescriptions = find_key(Object, ?STAFF_PRESCRIPTIONS_KEY, ?STAFF_PRESCRIPTIONS_CRDT, []),
  #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions};
build_app_record(facility, Object) ->
  Id = find_key(Object, ?FACILITY_ID_KEY, ?FACILITY_ID_CRDT, -1),
  Name = find_key(Object, ?FACILITY_NAME_KEY, ?FACILITY_NAME_CRDT, <<"undefined">>),
  Address = find_key(Object, ?FACILITY_ADDRESS_KEY, ?FACILITY_ADDRESS_CRDT, <<"undefined">>),
  Type = find_key(Object, ?FACILITY_TYPE_KEY, ?FACILITY_TYPE_CRDT, <<"undefined">>),
  #facility{id = Id, name = Name, address = Address, type = Type};
build_app_record(prescription, Object) ->
  Id = find_key(Object, ?PRESCRIPTION_ID_KEY, ?PRESCRIPTION_ID_CRDT, -1),
  PatientId = find_key(Object, ?PRESCRIPTION_PATIENT_ID_KEY, ?PRESCRIPTION_PATIENT_ID_CRDT, <<"undefined">>),
  PrescriberId = find_key(Object, ?PRESCRIPTION_PRESCRIBER_ID_KEY, ?PRESCRIPTION_PRESCRIBER_ID_CRDT, <<"undefined">>),
  PharmacyId = find_key(Object, ?PRESCRIPTION_PHARMACY_ID_KEY, ?PRESCRIPTION_PHARMACY_ID_CRDT, <<"undefined">>),
  DatePrescribed = find_key(Object, ?PRESCRIPTION_DATE_PRESCRIBED_KEY, ?PRESCRIPTION_DATE_PRESCRIBED_CRDT,
  <<"undefined">>),
  IsProcessed = find_key(Object, ?PRESCRIPTION_IS_PROCESSED_KEY, ?PRESCRIPTION_IS_PROCESSED_CRDT,
  ?PRESCRIPTION_NOT_PROCESSED_VALUE),
  DateProcessed = find_key(Object, ?PRESCRIPTION_DATE_PROCESSED_KEY, ?PRESCRIPTION_DATE_PROCESSED_CRDT,
  <<"undefined">>),
  Drugs = find_key(Object, ?PRESCRIPTION_DRUGS_KEY, ?PRESCRIPTION_DRUGS_CRDT, []),
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

get_entity_with_prescriptions(Entity, Id) ->
    Txn = txn_start(),
    Result = get_entity_with_prescriptions(Entity, Id, Txn),
    txn_commit(Txn),
    Result.

get_entity_with_prescriptions(Entity, Id, Txn) ->
    EntityKey = gen_key(Entity, Id),
    BinaryEntity = list_to_binary(atom_to_list(Entity)),
    [EntityFields, Prescriptions, ProcessedPrescriptions] = multi_read([
      create_bucket(EntityKey, ?MAP),
      create_bucket(<<EntityKey/binary, "_prescriptions">>, ?ORSET),
      create_bucket(<<EntityKey/binary, "_prescriptions_processed">>, ?ORSET)
    ], Txn),
    case {Prescriptions, ProcessedPrescriptions} of
      {{error, not_found}, {error, not_found}} -> build_app_record(Entity, EntityFields);
      {{error, not_found}, PrescriptionKeys2} ->
          build_app_record(Entity, [
            {{<<BinaryEntity/binary, "_prescriptions">>, ?ORSET}, PrescriptionKeys2} | EntityFields
          ]);
      {PrescriptionKeys1, {error, not_found}} ->
          build_app_record(Entity, [
            {{<<BinaryEntity/binary, "_prescriptions">>, ?ORSET}, PrescriptionKeys1} | EntityFields
          ]);
      {PrescriptionKeys1, PrescriptionKeys2} ->
          build_app_record(Entity, [{{<<BinaryEntity/binary, "_prescriptions">>, ?ORSET},
              lists:append(PrescriptionKeys1, PrescriptionKeys2)} | EntityFields])
    end.

multi_read(Objects, Txn) ->
  Results = txn_read_objects(Objects, Txn),
  lists:map(fun parse_read_result/1, Results).

-spec create_patient(id(), string(), string()) -> ok | {error, reason()}.
create_patient(Id, Name, Address) -> create_if_not_exists(patient, [Id, Name, Address]).

-spec create_pharmacy(id(), string(), string()) -> ok | {error, reason()}.
create_pharmacy(Id, Name, Address) -> create_if_not_exists(pharmacy, [Id, Name, Address]).

-spec create_facility(id(), string(), string(), string()) -> ok | {error, reason()}.
create_facility(Id, Name, Address, Type) -> create_if_not_exists(facility, [Id, Name, Address, Type]).

-spec create_staff(id(), string(), string(), string()) -> ok | {error, reason()}.
create_staff(Id, Name, Address, Speciality) -> create_if_not_exists(staff, [Id, Name, Address, Speciality]).

-spec get_event_by_id(id()) -> [crdt()] | {error, reason()}.
get_event_by_id(_Id) -> erlang:error(not_implemented).

-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) -> build_app_record(facility, process_get_request(gen_key(facility, Id), ?MAP)).

-spec get_facility_by_id(id(), txid()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id, Txn) -> build_app_record(facility, process_get_request(gen_key(facility, Id), ?MAP, Txn)).

-spec get_pharmacy_by_id(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id) -> get_entity_with_prescriptions(pharmacy, Id).

-spec get_pharmacy_by_id(id(), txid()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id, Txn) -> get_entity_with_prescriptions(pharmacy, Id, Txn).

-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) -> get_entity_with_prescriptions(patient, Id).

-spec get_patient_by_id(id(), txid()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id, Txn) -> get_entity_with_prescriptions(patient, Id, Txn).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(PharmacyId) ->
    PharmacyKey = gen_key(pharmacy, PharmacyId),
    Txn = txn_start(),
    [Prescriptions, ProcessedPrescriptions] = multi_read([
      create_bucket(<<PharmacyKey/binary, "_prescriptions">>, ?ORSET),
      create_bucket(<<PharmacyKey/binary, "_prescriptions_processed">>, ?ORSET)
    ], Txn),
    Result =
      case {Prescriptions, ProcessedPrescriptions} of
          {{error, not_found}, {error, not_found}} -> [];
          {{error, not_found}, PrescriptionKeys2} ->  PrescriptionKeys2;
          {PrescriptionKeys1, {error, not_found}} ->  PrescriptionKeys1;
          {PrescriptionKeys1, PrescriptionKeys2} ->   lists:append(PrescriptionKeys1, PrescriptionKeys2)
      end,
    txn_commit(Txn),
    Result.

-spec get_processed_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_processed_pharmacy_prescriptions(PharmacyId) ->
    PharmacyKey = gen_key(pharmacy, PharmacyId),
    Txn = txn_start(),
    [ProcessedPrescriptions] = multi_read([
      create_bucket(<<PharmacyKey/binary, "_prescriptions_processed">>, ?ORSET)
    ], Txn),
    Result =
      case ProcessedPrescriptions of
          {error, not_found} -> [];
          PrescriptionKeys1 ->  PrescriptionKeys1
      end,
    txn_commit(Txn),
    Result.

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
    build_app_record(prescription, process_get_request(gen_key(prescription, Id), ?MAP)).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
get_prescription_medication(Id) ->
    Prescription = get_prescription_by_id(Id),
    case Prescription of
      {error, _} -> {error, no_such_prescription};
      _ -> Prescription#prescription.drugs
    end.

%% Alternative to get_prescription_by_id/1, which includes a transactional context.
-spec get_prescription_by_id(id(), txid()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id, Txn) ->
    build_app_record(prescription, process_get_request(gen_key(prescription, Id), ?MAP, Txn)).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) -> get_entity_with_prescriptions(staff, Id).

%% Alternative to get_staff_by_id/1, which includes a transactional context.
-spec get_staff_by_id(id(), txid()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id, Txn) -> get_entity_with_prescriptions(staff, Id, Txn).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_staff_prescriptions(StaffId) ->
    StaffKey = gen_key(staff, StaffId),
    Txn = txn_start(),
    [Prescriptions, ProcessedPrescriptions] = multi_read([
      create_bucket(<<StaffKey/binary, "_prescriptions">>, ?ORSET),
      create_bucket(<<StaffKey/binary, "_prescriptions_processed">>, ?ORSET)
    ], Txn),
    case {Prescriptions, ProcessedPrescriptions} of
        {{error, not_found}, {error, not_found}} -> [];
        {{error, not_found}, PrescriptionKeys2} ->  PrescriptionKeys2;
        {PrescriptionKeys1, {error, not_found}} ->  PrescriptionKeys1;
        {PrescriptionKeys1, PrescriptionKeys2} ->   lists:append(PrescriptionKeys1, PrescriptionKeys2)
    end.

%% Fetches a list of treatments given a certain staff member ID.
-spec get_staff_treatments(id()) -> [crdt()] | {error, reason()}.
get_staff_treatments(_StaffId) ->
  erlang:error(not_implemented).

%% Fetches a treatment by ID.
-spec get_treatment_by_id(id()) -> [crdt()] | {error, reason()}.
get_treatment_by_id(_Id) ->
  erlang:error(not_implemented).

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(), string(), string()) -> ok | {error, reason()}.
update_patient_details(Id, Name, Address) ->
  update_if_already_exists(patient, [Id, Name, Address]).

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(), string(), string()) -> ok | {error, reason()}.
update_pharmacy_details(Id, Name, Address) ->
  update_if_already_exists(pharmacy, [Id, Name, Address]).

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_facility_details(Id, Name, Address, Type) ->
  update_if_already_exists(facility, [Id, Name, Address, Type]).

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_staff_details(Id, Name, Address, Speciality) ->
  update_if_already_exists(staff, [Id, Name, Address, Speciality]).

-spec update_prescription_medication(id(), atom(), [string()]) -> ok | {error, reason()}.
update_prescription_medication(Id, add_drugs, Drugs) ->
  Txn = txn_start(),
  Result =
        case get_prescription_by_id(Id, Txn) of
            {error, not_found} ->
                {error, no_such_prescription};
            #prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE} ->
                {error, prescription_already_processed};
            #prescription{is_processed=?PRESCRIPTION_NOT_PROCESSED_VALUE} ->
                PrescriptionSetOp = {add_all, lists:map(fun(Drug) -> list_to_binary(Drug) end, Drugs)},
                UpdateOperation = [build_map_op(?PRESCRIPTION_DRUGS_KEY, ?PRESCRIPTION_DRUGS_CRDT, PrescriptionSetOp)],
                put(gen_key(prescription, Id), ?MAP, update, UpdateOperation, Txn),
                ok
       end,
  ok = txn_commit(Txn),
  Result;
update_prescription_medication(_Id, _Action, _Drugs) -> {error, invalid_update_operation}.

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy and treatment facility (hospital). The prescription also includes the prescription date
%% and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
  %% gather required antidote keys
  PrescriptionKey = gen_key(prescription, PrescriptionId),
  PatientKey = gen_key(patient, PatientId),
  PharmacyKey = gen_key(pharmacy, PharmacyId),
  PrescriberKey = gen_key(staff, PrescriberId),
  Txn = txn_start(),
  %% check required pre-conditions
  [ {taken, {patient, PatientId}}, {taken, {pharmacy, PharmacyId}}, {taken, {staff, PrescriberId}} ]
      = check_keys(Txn, [{patient, PatientId}, {pharmacy, PharmacyId}, {staff, PrescriberId}]),

  PrescriptionFields = [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
  {HandleCreateOpResult, Txn2} = create_if_not_exists(prescription, PrescriptionFields, Txn),

  {Result, Txn3} = case HandleCreateOpResult of
      ok ->
          %% build updates for patients, pharmacies, facilities and the prescriber
          %% these are already generated as buckets
          PatientUpdate = gen_entity_update(add_entity_prescription, [PatientKey, PrescriptionKey]),
          PharmacyUpdate = gen_entity_update(add_entity_prescription, [PharmacyKey, PrescriptionKey]),
          PrescriberUpdate = gen_entity_update(add_entity_prescription, [PrescriberKey, PrescriptionKey]),
          txn_update_objects([ PharmacyUpdate, PrescriberUpdate, PatientUpdate ], Txn2),
          {ok, Txn2};
      ErrorMessage -> {ErrorMessage, Txn2}
  end,
  txn_commit(Txn3),
  Result.

check_keys(_Context, []) ->
    [];
check_keys(Context, [H|T]) ->
    {Entity, Id} = H,
    case process_get_request(gen_key(Entity, Id), ?MAP, Context) of
        {error, not_found} -> [{free, H}] ++ check_keys(Context, T);
        _Object -> [{taken, H}] ++ check_keys(Context, T)
    end.

%% Creates a treatment event, with information about the staff member that registered it,
%% along with a timestamp and description.
-spec create_event(id(), id(), id(), string(), string()) -> ok | {error, reason()}.
create_event(_EventId, _TreatmentId, _StaffMemberId, _Timestamp, _Description) ->
  erlang:error(not_implemented).

%% Creates a treatment with information about the patient, the staff member that iniciated it,
%% and also the facility ID and date when the treatment started.
-spec create_treatment(id(), id(), id(), id(), string()) -> ok | {error, reason()}.
create_treatment(_TreatmentId, _PatientId, _StaffId, _FacilityId, _DateStarted) ->
  erlang:error(not_implemented).

%% Same as create_treatment/5, but includes an ending date for the treatment.
-spec create_treatment(id(), id(), id(), id(), string(), string()) -> ok | {error, reason()}.
create_treatment(_TreatmentId, _PatientId, _StaffId, _FacilityId, _DateStarted, _DateEnded) ->
  erlang:error(not_implemented).

process_prescription(Id, Date) ->
  Txn = txn_start(),
  Prescription = get_prescription_by_id(Id, Txn),
  Result =
    case can_process_prescription(Prescription) of
        {false, Reason} ->
            {error, Reason};
        true ->
            PharmacyKey =      gen_key(pharmacy, binary_to_integer(Prescription#prescription.pharmacy_id)),
            PatientKey =       gen_key(patient, binary_to_integer(Prescription#prescription.patient_id)),
            StaffKey =         gen_key(staff, binary_to_integer(Prescription#prescription.prescriber_id)),
            PrescriptionKey =  gen_key(prescription, Id),

            IsProcessedOp = build_lwwreg_op(?PRESCRIPTION_IS_PROCESSED_KEY, ?PRESCRIPTION_IS_PROCESSED_CRDT,
            ?PRESCRIPTION_PROCESSED_VALUE),
            ProcessedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PROCESSED_KEY, ?PRESCRIPTION_DATE_PROCESSED_CRDT, Date),
            PrescriptionUpdate = {create_bucket(PrescriptionKey, ?MAP), update, [IsProcessedOp, ProcessedOp]},

            PharmacyOp1 =  gen_entity_update(remove_entity_prescription, [PharmacyKey, PrescriptionKey]),
            PharmacyOp2 =  gen_entity_update(add_entity_processed_prescription, [PharmacyKey, PrescriptionKey]),
            PatientOp1 =   gen_entity_update(remove_entity_prescription, [PatientKey, PrescriptionKey]),
            PatientOp2 =   gen_entity_update(add_entity_processed_prescription, [PatientKey, PrescriptionKey]),
            StaffOp1 =     gen_entity_update(remove_entity_prescription, [StaffKey, PrescriptionKey]),
            StaffOp2 =     gen_entity_update(add_entity_processed_prescription, [StaffKey, PrescriptionKey]),

            txn_update_objects([
                PrescriptionUpdate, PharmacyOp1, PharmacyOp2, PatientOp1, PatientOp2, StaffOp1, StaffOp2
            ], Txn),
        ok
    end,
  ok = txn_commit(Txn),
  Result.

can_process_prescription({error, not_found}) ->
    {false, no_such_prescription};
can_process_prescription(#prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE}) ->
    {false, prescription_already_processed};
can_process_prescription(#prescription{is_processed=?PRESCRIPTION_NOT_PROCESSED_VALUE}) ->
    true.


%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------

process_get_request(Key, Type) ->
  parse_read_result(get(Key, Type)).

process_get_request(Key, Type, Txn) ->
  parse_read_result(get(Key, Type, Txn)).

parse_read_result({_Crdt, []}) -> {error, not_found};
parse_read_result({_Crdt, Object}) -> Object;
parse_read_result(_) -> erlang:error(unknown_object_type).

error_to_binary(Reason) -> list_to_binary(lists:flatten(io_lib:format("~p", [Reason]))).

%% ------------------------------------------------------------------------------------------------
%% Antidote's transaction API wrapper - Use when you need fine grain control over transactions
%% Please refer to the official Antidote transaction documentation for reference on these functions
%% ------------------------------------------------------------------------------------------------

%% A wrapper for Antidote's start_transaction function
-spec txn_start() -> txid().
txn_start() ->
  Pid = fmke_db_conn_manager:checkout(),
  {ok, TxnDetails} = antidotec_pb:start_transaction(Pid, ignore, {}),
  {Pid, TxnDetails}.

%% A wrapper for Antidote's read_objects function, with a single object being read.
-spec txn_read_object(Object :: bound_object(), TxnDetails :: txid()) -> term().
txn_read_object(Object, {Pid, TxnDetails}) ->
  {ok, [Value]} = antidotec_pb:read_values(Pid, [Object], TxnDetails),
  Value.

%% A wrapper for Antidote's read_objects function
-spec txn_read_objects(Objects :: [bound_object()], TxnDetails :: txid()) -> [term()].
txn_read_objects([], _) -> [];
txn_read_objects(Objects, {Pid, TxnDetails}) ->
  {ok, Values} = antidotec_pb:read_values(Pid, Objects, TxnDetails),
  Values.

%% A wrapper for Antidote's update_objects function, with a single object being written.
-spec txn_update_object({bound_object(), op_name(), op_param()}, txid()) -> ok.
txn_update_object(ObjectUpdate, {Pid, TxnDetails}) ->
  ok = antidotec_pb:update_objects(Pid, [ObjectUpdate], TxnDetails).

%% A wrapper for Antidote's update_objects function
-spec txn_update_objects([update()], txid()) -> ok.
txn_update_objects([], _) -> ok;
txn_update_objects(ObjectUpdates, {Pid, TxnDetails}) ->
  ok = antidotec_pb:update_objects(Pid, ObjectUpdates, TxnDetails).

%% A wrapper for Antidote's commit_transaction function
-spec txn_commit(TxnDetails :: txid()) -> ok.
txn_commit({Pid, TxnDetails}) ->
  {ok, _CommitTime} = antidotec_pb:commit_transaction(Pid, TxnDetails),
  fmke_db_conn_manager:checkin(Pid).


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
