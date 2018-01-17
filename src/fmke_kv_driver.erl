%%%-------------------------------------------------------------------
%%% @author goncalotomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2017 15:27
%%%-------------------------------------------------------------------
-module(fmke_kv_driver).
-include("fmke.hrl").
-include("fmk_kv.hrl").
-author("goncalotomas").

%% TODO this interface is the same, but has an extra "Context" parameter for all ops
%% TODO how do we make sure that this module has the same api as fmke.erl? (without relying on unit/coverage tests)...?
-behaviour(fmke_gen_driver).
-behaviour(gen_server).

%%-----------------------------------------------------------------------------
%% fmke_gen_driver exports
%%-----------------------------------------------------------------------------
-export([
    start/1,
    stop/1,
    create_patient/3,
    create_pharmacy/3,
    create_facility/4,
    create_staff/4,
    create_prescription/6,
    get_facility_by_id/1,
    get_patient_by_id/1,
    get_pharmacy_by_id/1,
    get_processed_pharmacy_prescriptions/1,
    get_pharmacy_prescriptions/1,
    get_prescription_by_id/1,
    get_prescription_medication/1,
    get_staff_by_id/1,
    get_staff_prescriptions/1,
    get_staff_treatments/1,
    process_prescription/2,
    update_patient_details/3,
    update_pharmacy_details/3,
    update_facility_details/4,
    update_staff_details/4,
    update_prescription_medication/3
  ]).

%% gen_server exports
-export ([init/1, handle_cast/2, handle_call/3]).

-type context() :: term().

-define (BUILD_NESTED_MAP_OP(TopLevelKey, Key, Op), [update_map_op(TopLevelKey, [update_map_op(Key, Op)])]).
%% TODO switch to stateful modules
-define (KV_IMPLEMENTATION(), fmke_config:get(simplified_driver)).

-define(MAP, map).
-define(REGISTER, register).

%% -------------------------------------------------------------------
%% Setup and teardown functions (simply pass down to db module)
%% -------------------------------------------------------------------

start(InitParams) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitParams, []).

stop(State) ->
    Driver = proplists:get_value(simplified_driver, State),
    Driver:stop([]),
    gen_server:call(?MODULE, stop).

init(InitParams) ->
    Driver = proplists:get_value(simplified_driver, InitParams),
    Driver:start(InitParams),
    {ok, Driver}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({create_patient, Id, Name, Address}, _From, Driver) ->
    {reply, create_if_not_exists(patient, [Id, Name, Address], Driver), Driver};

handle_call({create_pharmacy, Id, Name, Address}, _From, Driver) ->
    {reply, create_if_not_exists(pharmacy, [Id, Name, Address], Driver), Driver};

handle_call({create_facility, Id, Name, Address, Type}, _From, Driver) ->
    {reply, create_if_not_exists(facility, [Id, Name, Address, Type], Driver), Driver};

handle_call({create_staff, Id, Name, Address, Speciality}, _From, Driver) ->
    {reply, create_if_not_exists(staff, [Id, Name, Address, Speciality], Driver), Driver};

handle_call({create_prescription, Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs}, _From, Driver) ->
    {reply, create_prescription(Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs, Driver), Driver};

handle_call({get_facility_by_id, Id}, _From, Driver) ->
    {reply, execute_get_op(facility, Id, Driver), Driver};

handle_call({get_patient_by_id, Id}, _From, Driver) ->
    {reply, execute_get_op(patient, Id, Driver), Driver};

handle_call({get_pharmacy_by_id, Id}, _From, Driver) ->
    {reply, execute_get_op(pharmacy, Id, Driver), Driver};

handle_call({get_pharmacy_prescriptions, Id}, _From, Driver) ->
    {reply, execute_get_op(pharmacy, Id, Driver), Driver}; %% TODO not filtering pharmacy prescriptions

handle_call({get_processed_pharmacy_prescriptions, Id}, _From, Driver) ->
    {reply, execute_get_op(pharmacy, Id, Driver), Driver}; %% TODO not filtering processed pharmacy prescriptions

handle_call({get_prescription_by_id, Id}, _From, Driver) ->
    {reply, execute_get_op(prescription, Id, Driver), Driver};

handle_call({get_prescription_medication, Id}, _From, Driver) ->
    Prescription = execute_get_op(prescription, Id, Driver),
    {reply, Prescription#prescription.drugs, Driver};

handle_call({get_staff_by_id, Id}, _From, Driver) ->
    {reply, execute_get_op(staff, Id, Driver), Driver};

handle_call({get_staff_prescriptions, Id}, _From, Driver) ->
    {reply, execute_get_op(staff, Id, Driver), Driver}; %% TODO not filtering staff prescriptions

handle_call({update_patient_details, Id, Name, Address}, _From, Driver) ->
    {reply, update_if_already_exists(patient, [Id, Name, Address], Driver), Driver};

handle_call({update_pharmacy_details, Id, Name, Address}, _From, Driver) ->
    {reply, update_if_already_exists(pharmacy, [Id, Name, Address], Driver), Driver};

handle_call({update_facility_details, Id, Name, Address, Type}, _From, Driver) ->
    {reply, update_if_already_exists(facility, [Id, Name, Address, Type], Driver), Driver};

handle_call({update_staff_details, Id, Name, Address, Speciality}, _From, Driver) ->
    {reply, update_if_already_exists(staff, [Id, Name, Address, Speciality], Driver), Driver};

handle_call({update_prescription_medication, Id, Operation, Drugs}, _From, Driver) ->
    {ok, Txn} = Driver:start_transaction([]),
    {Result, Txn3} =
      case execute_get_op(prescription, Id, Driver, Txn) of
          {{error, not_found}, Txn1} -> {{error, no_such_prescription}, Txn1};
          {{ok, Prescription}, Txn2} -> update_prescription_w_obj(Txn2, Prescription, Operation, Drugs, Driver)
      end,
    Driver:commit_transaction(Txn3),
    {reply, Result, Driver};

handle_call({process_prescription, Id, Date}, _From, Driver) ->
    {ok, Txn} = Driver:start_transaction([]),
    {Result, Txn3} =
      case execute_get_op(prescription, Id, Driver, Txn) of
          {{error, not_found}, Txn1} -> {{error, no_such_prescription}, Txn1};
          {{ok, Prescription}, Txn2} -> process_prescription_w_obj(Txn2, Prescription, Date, Driver)
      end,
    Driver:commit_transaction(Txn3),
    {reply, Result, Driver}.

create_if_not_exists(Entity, Fields, Driver) ->
    {ok, Txn} = Driver:start_transaction([]),
    Id = hd(Fields),
    {Result, Txn3} =
      case check_id(Entity, Id, Txn, Driver) of
        {taken, Txn1} ->
            {{error, list_to_atom(lists:flatten(io_lib:format("~p_id_taken", [Entity])))}, Txn1};
        {free, Txn2} ->
            EntityKey = gen_key(Entity, Id),
            EntityUpdate = gen_entity_update(Entity, Fields),
            execute_create_op(Entity, EntityKey, EntityUpdate, Txn2, Driver)
      end,
    Driver:commit_transaction(Txn3),
    Result.

%% same as create_if_not_exists/2, but with transactional context
create_if_not_exists(Entity, Fields, Driver, Txn) ->
    Id = hd(Fields),
    {Result, Txn3} =
      case check_id(Entity, Id, Txn, Driver) of
        {taken, Txn1} ->
            {{error, list_to_atom(lists:flatten(io_lib:format("~p_id_taken", [Entity])))}, Txn1};
        {free, Txn2} ->
            EntityKey = gen_key(Entity, Id),
            EntityUpdate = gen_entity_update(Entity, Fields),
            execute_create_op(Entity, EntityKey, EntityUpdate, Txn2, Driver)
      end,
    {Result, Txn3}.

%% Does kind of the opposite of create_if_not_exists/2
update_if_already_exists(Entity, Fields, Driver) ->
    {ok, Txn} = Driver:start_transaction([]),
    Id = hd(Fields),
    {Result, Txn3} =
      case check_id(Entity, Id, Txn, Driver) of
        {free, Txn1} ->
            {{error, list_to_atom(lists:flatten(io_lib:format("no_such_~p", [Entity])))}, Txn1};
        {taken, Txn2} ->
            EntityKey = gen_key(Entity, Id),
            EntityUpdate = gen_entity_update(Entity, Fields),
            execute_create_op(Entity, EntityKey, EntityUpdate, Txn2, Driver)
      end,
    Driver:commit_transaction(Txn3),
    Result.

%% Checks if an entity exists
check_id(Entity, Id, Txn, Driver) ->
    Key = gen_key(Entity, Id),
    case Driver:get(Key, Entity, Txn) of
      {{error, not_found}, Txn1} -> {free, Txn1};
      {_Map, Txn2} -> {taken, Txn2}
    end.

execute_create_op(Entity, Key, Op, Context, Driver) ->
    {ok, _Context2} = Driver:put(Key, Entity, Op, Context).

execute_get_op(Entity, Id, Driver) ->
    {ok, Txn} = Driver:start_transaction([]),
    Key = gen_key(Entity, Id),
    {Result, Txn1} = Driver:get(Key, Entity, Txn),
    Driver:commit_transaction(Txn1),
    case Result of
      {ok, Object} -> Object;
      Error -> Error
    end.

execute_get_op(Entity, Id, Driver, Txn) ->
    Key = gen_key(Entity, Id),
    {_Result, _Txn1} = Driver:get(Key, Entity, Txn).

create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs, Driver) ->
    PatientKey = gen_key(patient, PatientId),
    PharmacyKey = gen_key(pharmacy, PharmacyId),
    PrescriberKey = gen_key(staff, PrescriberId),

    {ok, Txn} = Driver:start_transaction([]),
    %% Check if multiple keys are taken
    [ {taken, {patient, PatientId}}, {taken, {pharmacy, PharmacyId}}, {taken, {staff, PrescriberId}} ]
        = check_keys(Txn, [{patient, PatientId}, {pharmacy, PharmacyId}, {staff, PrescriberId}], Driver),

    %% Create top level prescription if key does not exist.
    PrescriptionFields = [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
    {HandleCreateOpResult, Txn2} = create_if_not_exists(prescription, PrescriptionFields, Driver, Txn),

    {Result, Context5} = case HandleCreateOpResult of
        ok ->
            %% creating top level prescription was successful, create nested objects
            PatientUpdate = [gen_nested_entity_update(prescription, ?PATIENT_PRESCRIPTIONS_KEY, PrescriptionFields)],
            PharmacyUpdate = [gen_nested_entity_update(prescription, ?PHARMACY_PRESCRIPTIONS_KEY, PrescriptionFields)],
            PrescriberUpdate = [gen_nested_entity_update(prescription, ?STAFF_PRESCRIPTIONS_KEY, PrescriptionFields)],
            {ok, Context2} = Driver:put(PatientKey, patient, PatientUpdate, Txn2),
            {ok, Context3} = Driver:put(PharmacyKey, pharmacy, PharmacyUpdate, Context2),
            {ok, Context4} = Driver:put(PrescriberKey, staff, PrescriberUpdate, Context3),
            {ok, Context4};
        ErrorMessage -> {ErrorMessage, Txn2}
    end,
    Driver:commit_transaction(Context5),
    Result.

process_prescription_w_obj(Context, Prescription = #prescription{}, DateProcessed, Driver) ->
    case Prescription#prescription.is_processed of
        ?PRESCRIPTION_PROCESSED_VALUE ->
            {{error, prescription_already_processed}, Context};
        _Other ->
            PrescriptionId = binary_to_integer(Prescription#prescription.id),
            PatientId = binary_to_integer(Prescription#prescription.patient_id),
            PrescriberId = binary_to_integer(Prescription#prescription.prescriber_id),
            PharmacyId = binary_to_integer(Prescription#prescription.pharmacy_id),
            PrescriptionKey = gen_key(prescription, PrescriptionId),
            PatientKey = gen_key(patient, PatientId),
            PrescriberKey = gen_key(staff, PrescriberId),
            PharmacyKey = gen_key(pharmacy, PharmacyId),

            NestedOp = [
                create_register_op(?PRESCRIPTION_IS_PROCESSED_KEY, ?PRESCRIPTION_PROCESSED_VALUE),
                create_register_op(?PRESCRIPTION_DATE_PROCESSED_KEY, DateProcessed)
            ],

            PatientUpdate = [update_map_op(?PATIENT_PRESCRIPTIONS_KEY, [update_map_op(PrescriptionKey, NestedOp)])],
            PharmacyUpdate = [update_map_op(?PHARMACY_PRESCRIPTIONS_KEY, [update_map_op(PrescriptionKey, NestedOp)])],
            PrescriberUpdate = [update_map_op(?STAFF_PRESCRIPTIONS_KEY, [update_map_op(PrescriptionKey, NestedOp)])],

            Operations = [
                {PrescriptionKey, prescription, NestedOp},
                {PatientKey, patient, PatientUpdate},
                {PharmacyKey, pharmacy, PharmacyUpdate},
                {PrescriberKey, staff, PrescriberUpdate}
            ],

            run_updates(Context, Operations, false, Driver)
      end.

-spec run_updates(Context :: context(), ListOps :: list(), Aborted :: boolean(), Driver :: atom()) ->
    {ok, context()} | {{error, term()}, context()}.
run_updates(Context, _ListOps, true, _Driver) ->
    %% TODO not calling abort on driver, might be useful to do it in some KVS
    {{error, txn_aborted}, Context};
run_updates(Context, [], false, _Driver) ->
    {ok, Context};
run_updates(Context, [H|T], false, Driver) ->
    {Key, KeyType, Update} = H,
    case execute_create_op(KeyType, Key, Update, Context, Driver) of
        {ok, Context2} ->
            run_updates(Context2, T, false, Driver);
        {_Error, Context3} ->
            run_updates(Context3, T, true, Driver)
    end.

update_prescription_w_obj(Context, Prescription = #prescription{}, Operation, Drugs, Driver) ->
    case Prescription#prescription.is_processed of
        ?PRESCRIPTION_PROCESSED_VALUE ->
            {{error, prescription_already_processed}, Context};
        _Other ->
            PrescriptionId = binary_to_integer(Prescription#prescription.id),
            PatientId = binary_to_integer(Prescription#prescription.patient_id),
            PrescriberId = binary_to_integer(Prescription#prescription.prescriber_id),
            PharmacyId = binary_to_integer(Prescription#prescription.pharmacy_id),
            PrescriptionKey = gen_key(prescription, PrescriptionId),
            PatientKey = gen_key(patient, PatientId),
            PrescriberKey = gen_key(staff, PrescriberId),
            PharmacyKey = gen_key(pharmacy, PharmacyId),

            NestedOp = [create_set_op(?PRESCRIPTION_DRUGS_KEY, Drugs)],
            PatientUpdate = ?BUILD_NESTED_MAP_OP(?PATIENT_PRESCRIPTIONS_KEY, PrescriptionKey, NestedOp),
            PharmacyUpdate = ?BUILD_NESTED_MAP_OP(?PHARMACY_PRESCRIPTIONS_KEY, PrescriptionKey, NestedOp),
            PrescriberUpdate = ?BUILD_NESTED_MAP_OP(?STAFF_PRESCRIPTIONS_KEY, PrescriptionKey, NestedOp),

            ListUpdates = [
                {PrescriptionKey, prescription, NestedOp},
                {PatientKey, patient, PatientUpdate},
                {PharmacyKey, pharmacy, PharmacyUpdate},
                {PrescriberKey, staff, PrescriberUpdate}
            ],

            run_update_prescription_ops(Context, Operation, ListUpdates, Driver)
    end.

run_update_prescription_ops(Context, add_drugs, Updates, Driver) ->
    run_updates(Context, Updates, false, Driver);
run_update_prescription_ops(Context, _OtherOp, _Updates, _Driver) ->
    {{error, invalid_update_operation}, Context}.

%%-----------------------------------------------------------------------------
%% Create functions - no transactional context
%%-----------------------------------------------------------------------------

%% Adds a patient to the FMK system, needing only an ID, Name and Address.
%% A check is done to determine if a patient with the given ID already exists,
%% and if so the operation fails.
-spec create_patient(id(), string(), string()) -> ok | {error, reason()}.
create_patient(Id, Name, Address) ->
    gen_server:call(?MODULE, {create_patient, Id, Name, Address}).

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet been seen.
-spec create_pharmacy(id(), string(), string()) -> ok | {error, reason()}.
create_pharmacy(Id, Name, Address) ->
    gen_server:call(?MODULE, {create_pharmacy, Id, Name, Address}).

%% Adds a facility to the FMK-- system if the ID for the facility has not yet been seen.
-spec create_facility(id(), string(), string(), string()) -> ok | {error, reason()}.
create_facility(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {create_facility, Id, Name, Address, Type}).

%% Adds a staff member to the FMK-- system if the ID for the member has not yet been seen.
-spec create_staff(id(), string(), string(), string()) -> ok | {error, reason()}.
create_staff(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {create_staff, Id, Name, Address, Speciality}).

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy. The prescription also includes the prescription date and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    gen_server:call(?MODULE,
        {create_prescription, PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs}
    ).

%%-----------------------------------------------------------------------------
%% Read functions - no transactional context
%%-----------------------------------------------------------------------------

%% Fetches a patient by ID.
-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) ->
    gen_server:call(?MODULE, {get_patient_by_id, Id}).

%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) ->
    gen_server:call(?MODULE, {get_facility_by_id, Id}).

%% Fetches a pharmacy by ID.
-spec get_pharmacy_by_id(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_by_id, Id}).

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {get_prescription_by_id, Id}).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_prescriptions, Id}).

-spec get_processed_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_processed_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_processed_pharmacy_prescriptions, Id}).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {get_prescription_medication, Id}).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {get_staff_by_id, Id}).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_staff_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_staff_prescriptions, Id}).

%% Fetches a list of treatments given a certain staff member ID.
-spec get_staff_treatments(id()) -> [crdt()] | {error, reason()}.
get_staff_treatments(_Id) ->
    erlang:error(not_implemented).

%%-----------------------------------------------------------------------------
%% Update functions - no transactional context
%%-----------------------------------------------------------------------------

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(), string(), string()) -> ok | {error, reason()}.
update_patient_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update_patient_details, Id, Name, Address}).

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(), string(), string()) -> ok | {error, reason()}.
update_pharmacy_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update_pharmacy_details, Id, Name, Address}).

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_facility_details(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {update_facility_details, Id, Name, Address, Type}).

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_staff_details(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {update_staff_details, Id, Name, Address, Speciality}).

-spec update_prescription_medication(id(), atom(), [string()]) -> ok | {error, reason()}.
update_prescription_medication(Id, Operation, Drugs) ->
    gen_server:call(?MODULE, {update_prescription_medication, Id, Operation, Drugs}).

process_prescription(Id, Date) ->
    gen_server:call(?MODULE, {process_prescription, Id, Date}).

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions
%%-----------------------------------------------------------------------------

gen_entity_update(pharmacy, EntityFields) ->
    [Id, Name, Address] = EntityFields,
    [
        create_register_op(?PHARMACY_ID_KEY, Id),
        create_register_op(?PHARMACY_NAME_KEY, Name),
        create_register_op(?PHARMACY_ADDRESS_KEY, Address)
    ];
gen_entity_update(staff, EntityFields) ->
    [Id, Name, Address, Speciality] = EntityFields,
    [
        create_register_op(?STAFF_ID_KEY, Id),
        create_register_op(?STAFF_NAME_KEY, Name),
        create_register_op(?STAFF_ADDRESS_KEY, Address),
        create_register_op(?STAFF_SPECIALITY_KEY, Speciality)
    ];
gen_entity_update(facility, EntityFields) ->
    [Id, Name, Address, Type] = EntityFields,
    [
        create_register_op(?FACILITY_ID_KEY, Id),
        create_register_op(?FACILITY_NAME_KEY, Name),
        create_register_op(?FACILITY_ADDRESS_KEY, Address),
        create_register_op(?FACILITY_TYPE_KEY, Type)
    ];
gen_entity_update(prescription, EntityFields) ->
    [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs] = EntityFields,
    [
        create_register_op(?PRESCRIPTION_ID_KEY, PrescriptionId),
        create_register_op(?PRESCRIPTION_PATIENT_ID_KEY, PatientId),
        create_register_op(?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId),
        create_register_op(?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId),
        create_register_op(?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed),
        create_set_op(?PRESCRIPTION_DRUGS_KEY, Drugs)
    ];
gen_entity_update(patient, EntityFields) ->
    [Id, Name, Address] = EntityFields,
    [
        create_register_op(?PATIENT_ID_KEY, Id),
        create_register_op(?PATIENT_NAME_KEY, Name),
        create_register_op(?PATIENT_ADDRESS_KEY, Address)
    ].

gen_nested_entity_update(prescription, TopLevelKey, EntityFields) ->
    [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs] = EntityFields,
    NestedOps = [
        create_register_op(?PRESCRIPTION_ID_KEY, PrescriptionId),
        create_register_op(?PRESCRIPTION_PATIENT_ID_KEY, PatientId),
        create_register_op(?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId),
        create_register_op(?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId),
        create_register_op(?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed),
        create_set_op(?PRESCRIPTION_DRUGS_KEY, Drugs)
    ],
    update_map_op(TopLevelKey, [create_map_op(gen_key(prescription, PrescriptionId), NestedOps)]).

check_keys(_Context, [], _Driver) ->
    [];
check_keys(Context, [H|T], Driver) ->
    {Entity, Id} = H,
    case execute_get_op(Entity, Id, Driver, Context) of
        {{error, not_found}, Context1} -> [{free, H}] ++ check_keys(Context1, T, Driver);
        {{ok, _Object}, Context2} -> [{taken, H}] ++ check_keys(Context2, T, Driver)
    end.

update_map_op(Key, NestedOps) ->
    {update_map, Key, NestedOps}.

create_map_op(Key, NestedOps) ->
    {create_map, Key, NestedOps}.

create_register_op(Key, Value) ->
    {create_register, Key, Value}.

create_set_op(Key, Elements) ->
    {create_set, Key, Elements}.

gen_key(Entity, Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p", [Entity, Id]))).
