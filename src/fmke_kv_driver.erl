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

-behaviour(fmke_gen_driver).

-type context() :: term().

-define (build_nested_map_op(TopLevelKey,Key,Op), [update_map_op(TopLevelKey,[update_map_op(Key,Op)])]).
-define (KV_IMPLEMENTATION(), fmke_config:get(driver_implementation)).

-export([
    init/1,
    stop/1,
    start_transaction/1,
    abort_transaction/1,
    commit_transaction/1,
    create_patient/4,
    create_pharmacy/4,
    create_facility/5,
    create_staff/5,
    create_prescription/7,
    get_event_by_id/2,
    get_facility_by_id/2,
    get_patient_by_id/2,
    get_pharmacy_by_id/2,
    get_processed_pharmacy_prescriptions/2,
    get_pharmacy_prescriptions/2,
    get_prescription_by_id/2,
    get_prescription_medication/2,
    get_staff_by_id/2,
    get_staff_prescriptions/2,
    process_prescription/3,
    update_patient_details/4,
    update_pharmacy_details/4,
    update_facility_details/5,
    update_staff_details/5,
    update_prescription_medication/4
]).

-export ([
    %% unimplemented, unused functions.
    %% need to be here for the fmke_gen_kv_driver interface.
    create_event/5,
    create_treatment/5,
    create_prescription/8,
    get_facility_treatments/2,
    get_staff_treatments/2,
    get_treatment_by_id/2
]).

-define(MAP, map).
-define(REGISTER, register).

start_transaction(Context) ->
    (?KV_IMPLEMENTATION()):start_transaction(Context).

abort_transaction(Context) ->
    (?KV_IMPLEMENTATION()):start_transaction(Context).

commit_transaction(Context) ->
    (?KV_IMPLEMENTATION()):commit_transaction(Context).

create_patient(Context, Id, Name, Address) ->
    handle_get_result_for_create_op(patient,[Id,Name,Address],get_patient_by_id(Context,Id)).

create_pharmacy(Context, Id, Name, Address) ->
    handle_get_result_for_create_op(pharmacy,[Id,Name,Address],get_pharmacy_by_id(Context,Id)).

create_facility(Context, Id, Name, Address, Type) ->
    handle_get_result_for_create_op(facility,[Id,Name,Address,Type],get_facility_by_id(Context,Id)).

create_staff(Context,Id,Name,Address,Speciality) ->
    handle_get_result_for_create_op(staff,[Id,Name,Address,Speciality],get_staff_by_id(Context,Id)).

create_prescription(Context,PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs) ->
    PatientKey = gen_patient_key(PatientId),
    PharmacyKey = gen_pharmacy_key(PharmacyId),
    PrescriberKey = gen_staff_key(PrescriberId),

    %% Check if multiple keys are taken
    [
      {taken,{PatientKey,patient}},
      {taken,{PharmacyKey,pharmacy}},
      {taken,{PrescriberKey,staff}}
    ] = check_keys(Context,[{PatientKey,patient},{PharmacyKey,pharmacy},{PrescriberKey,staff}]),

    %% Create top level prescription if key does not exist.
    PrescriptionFields = [PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs],
    HandleCreateOpResult = handle_get_result_for_create_op(prescription,PrescriptionFields,
      get_prescription_by_id(Context,PrescriptionId)),

    case HandleCreateOpResult of
        {ok, Context1} ->
            %% creating top level prescription was successful, create nested objects
            PatientUpdate = [gen_nested_entity_update(prescription,?PATIENT_PRESCRIPTIONS_KEY,PrescriptionFields)],
            PharmacyUpdate = [gen_nested_entity_update(prescription,?PHARMACY_PRESCRIPTIONS_KEY,PrescriptionFields)],
            PrescriberUpdate = [gen_nested_entity_update(prescription,?STAFF_PRESCRIPTIONS_KEY,PrescriptionFields)],
            {ok, Context2} = (?KV_IMPLEMENTATION()):put(PatientKey,patient,PatientUpdate,Context1),
            {ok, Context3} = (?KV_IMPLEMENTATION()):put(PharmacyKey,pharmacy,PharmacyUpdate,Context2),
            {ok, Context4} = (?KV_IMPLEMENTATION()):put(PrescriberKey,staff,PrescriberUpdate,Context3),
            {ok, Context4};
        ErrorMessage -> ErrorMessage
    end.

get_event_by_id(Context,Id) ->
    execute_get_op(Context,event,gen_event_key(Id)).

get_facility_by_id(Context,Id) ->
    execute_get_op(Context,facility,gen_facility_key(Id)).

get_patient_by_id(Context, Id) ->
    execute_get_op(Context,patient,gen_patient_key(Id)).

get_pharmacy_by_id(Context, Id) ->
    execute_get_op(Context,pharmacy,gen_pharmacy_key(Id)).

get_processed_pharmacy_prescriptions(Context,Id) ->
    case get_pharmacy_prescriptions(Context,Id) of
        {{ok, PharmacyPrescriptions},Context1} ->
            {{ok,[Prescription || {_PrescriptionKey,Prescription} <- PharmacyPrescriptions,
                (?KV_IMPLEMENTATION()):find_key(Prescription,?PRESCRIPTION_IS_PROCESSED_KEY,?REGISTER)
                == ?PRESCRIPTION_PROCESSED_VALUE
            ]},Context1};
        Error -> Error
    end.

get_pharmacy_prescriptions(Context,Id) ->
    case get_pharmacy_by_id(Context,Id) of
        {{ok, PharmacyObject},Context1} ->
            {{ok,(?KV_IMPLEMENTATION()):find_key(PharmacyObject,?PHARMACY_PRESCRIPTIONS_KEY,?MAP)},Context1};
        Error -> Error
    end.

get_prescription_by_id(Context,Id) ->
  execute_get_op(Context,prescription,gen_prescription_key(Id)).

get_prescription_medication(Context,Id) ->
  case get_prescription_by_id(Context,Id) of
      {{ok, PrescriptionObject = #prescription{}},Context1} ->
          {{ok,PrescriptionObject#prescription.drugs,Context1}};
      Error -> Error
  end.

get_staff_by_id(Context,Id) ->
  execute_get_op(Context,staff,gen_staff_key(Id)).

get_staff_prescriptions(Context,Id) ->
    case get_staff_by_id(Context,Id) of
        {{ok, StaffObject},Context1} ->
            {{ok,(?KV_IMPLEMENTATION()):find_key(StaffObject,?STAFF_PRESCRIPTIONS_KEY,?MAP)},Context1};
        Error -> Error
    end.

process_prescription(Context,PrescriptionId,DateProcessed) ->
    case get_prescription_by_id(Context,PrescriptionId) of
        {{error,not_found},Context1} ->
            {{error,no_such_prescription},Context1};
        {{ok,PrescriptionObject},Context2} ->
            process_prescription_w_obj(Context2,PrescriptionObject,DateProcessed)
    end.

process_prescription_w_obj(Context,Prescription = #prescription{},DateProcessed) ->
    case Prescription#prescription.is_processed of
        ?PRESCRIPTION_PROCESSED_VALUE ->
            {{error, prescription_already_processed},Context};
        _Other ->
            PrescriptionId = binary_to_integer(Prescription#prescription.id),
            PatientId = binary_to_integer(Prescription#prescription.patient_id),
            PrescriberId = binary_to_integer(Prescription#prescription.prescriber_id),
            PharmacyId = binary_to_integer(Prescription#prescription.pharmacy_id),
            PrescriptionKey = gen_key(prescription,PrescriptionId),
            PatientKey = gen_key(patient,PatientId),
            PrescriberKey = gen_key(staff,PrescriberId),
            PharmacyKey = gen_key(pharmacy,PharmacyId),

            NestedOp = [
                create_register_op(?PRESCRIPTION_IS_PROCESSED_KEY,?PRESCRIPTION_PROCESSED_VALUE),
                create_register_op(?PRESCRIPTION_DATE_PROCESSED_KEY,DateProcessed)
            ],

            PatientUpdate = [update_map_op(?PATIENT_PRESCRIPTIONS_KEY,[update_map_op(PrescriptionKey,NestedOp)])],
            PharmacyUpdate = [update_map_op(?PHARMACY_PRESCRIPTIONS_KEY,[update_map_op(PrescriptionKey,NestedOp)])],
            PrescriberUpdate = [update_map_op(?STAFF_PRESCRIPTIONS_KEY,[update_map_op(PrescriptionKey,NestedOp)])],

            Operations = [
                {PrescriptionKey,prescription,NestedOp},
                {PatientKey,patient,PatientUpdate},
                {PharmacyKey,pharmacy,PharmacyUpdate},
                {PrescriberKey,staff,PrescriberUpdate}
            ],

            run_updates(Context,Operations,false)
      end.

-spec run_updates(Context :: context(), ListOps :: list(), Aborted :: boolean()) ->
    {ok,context()} | {{error, term()},context()}.
run_updates(Context,_ListOps,true) ->
    (?KV_IMPLEMENTATION()):abort_transaction(Context),
    {{error,txn_aborted},Context};
run_updates(Context,[],false) ->
    {ok, Context};
run_updates(Context,[H|T],false) ->
    {Key,KeyType,Update} = H,
    case execute_create_op(Context,Key,KeyType,Update) of
        {ok, Context2} ->
            run_updates(Context2,T,false);
        {_Error,Context3} ->
            run_updates(Context3,T,true)
    end.

update_patient_details(Context,Id,Name,Address) ->
    PatientKey = gen_key(patient,Id),
    PatientUpdate = lists:sublist(gen_entity_update(patient,[Id,Name,Address]),2,2),
    execute_create_op(Context,PatientKey,patient,PatientUpdate).

update_pharmacy_details(Context,Id,Name,Address) ->
    PharmacyKey = gen_key(pharmacy,Id),
    PharmacyUpdate = lists:sublist(gen_entity_update(pharmacy,[Id,Name,Address]),2,2),
    execute_create_op(Context,PharmacyKey,pharmacy,PharmacyUpdate).

update_facility_details(Context,Id,Name,Address,Type) ->
    FacilityKey = gen_key(facility,Id),
    FacilityUpdate = lists:sublist(gen_entity_update(facility,[Id,Name,Address,Type]),2,3),
    execute_create_op(Context,FacilityKey,facility,FacilityUpdate).

update_staff_details(Context,Id,Name,Address,Speciality) ->
    StaffKey = gen_key(staff,Id),
    StaffUpdate = lists:sublist(gen_entity_update(staff,[Id,Name,Address,Speciality]),2,3),
    execute_create_op(Context,StaffKey,staff,StaffUpdate).

update_prescription_medication(Context,PrescriptionId,Operation,Drugs) ->
    case get_prescription_by_id(Context,PrescriptionId) of
        {{error,not_found},Context1} ->
            {{error,not_found},Context1};
        {{ok,PrescriptionObject},Context2} ->
            update_prescription_w_obj(Context2,PrescriptionObject,Operation,Drugs)
    end.

update_prescription_w_obj(Context,Prescription = #prescription{},Operation,Drugs) ->
    case Prescription#prescription.is_processed of
        ?PRESCRIPTION_PROCESSED_VALUE ->
            {{error, prescription_already_processed},Context};
        _Other ->
            PrescriptionId = binary_to_integer(Prescription#prescription.id),
            PatientId = binary_to_integer(Prescription#prescription.patient_id),
            PrescriberId = binary_to_integer(Prescription#prescription.prescriber_id),
            PharmacyId = binary_to_integer(Prescription#prescription.pharmacy_id),
            PrescriptionKey = gen_key(prescription,PrescriptionId),
            PatientKey = gen_key(patient,PatientId),
            PrescriberKey = gen_key(staff,PrescriberId),
            PharmacyKey = gen_key(pharmacy,PharmacyId),

            NestedOp = [create_set_op(?PRESCRIPTION_DRUGS_KEY,Drugs)],
            PatientUpdate = ?build_nested_map_op(?PATIENT_PRESCRIPTIONS_KEY,PrescriptionKey,NestedOp),
            PharmacyUpdate = ?build_nested_map_op(?PHARMACY_PRESCRIPTIONS_KEY,PrescriptionKey,NestedOp),
            PrescriberUpdate = ?build_nested_map_op(?STAFF_PRESCRIPTIONS_KEY,PrescriptionKey,NestedOp),

            ListUpdates = [
                {PrescriptionKey,prescription,NestedOp},
                {PatientKey,patient,PatientUpdate},
                {PharmacyKey,pharmacy,PharmacyUpdate},
                {PrescriberKey,staff,PrescriberUpdate}
            ],

            run_update_prescription_ops(Context,Operation,ListUpdates)
    end.

run_update_prescription_ops(Context, add_drugs, Updates) ->
    run_updates(Context,Updates,false);
run_update_prescription_ops(Context, _OtherOp, _Updates) ->
    {{error,invalid_update_operation},Context}.


%%-----------------------------------------------------------------------------
%% Internal auxiliary functions
%%-----------------------------------------------------------------------------
execute_create_op(Context,Key,KeyType,Operation) ->
    {ok, _Context2} = (?KV_IMPLEMENTATION()):put(Key,KeyType,Operation,Context).

execute_get_op(Context,{Key,RecordType}) ->
      execute_get_op(Context,RecordType,Key).
execute_get_op(Context,RecordType,Key) ->
    (?KV_IMPLEMENTATION()):get(Key,RecordType,Context).

gen_entity_update(pharmacy,EntityFields) ->
    [Id,Name,Address] = EntityFields,
    [
        create_register_op(?PHARMACY_ID_KEY,Id),
        create_register_op(?PHARMACY_NAME_KEY,Name),
        create_register_op(?PHARMACY_ADDRESS_KEY,Address)
    ];
gen_entity_update(staff,EntityFields) ->
    [Id,Name,Address,Speciality] = EntityFields,
    [
        create_register_op(?STAFF_ID_KEY,Id),
        create_register_op(?STAFF_NAME_KEY,Name),
        create_register_op(?STAFF_ADDRESS_KEY,Address),
        create_register_op(?STAFF_SPECIALITY_KEY,Speciality)
    ];
gen_entity_update(facility,EntityFields) ->
    [Id,Name,Address,Type] = EntityFields,
    [
        create_register_op(?FACILITY_ID_KEY,Id),
        create_register_op(?FACILITY_NAME_KEY,Name),
        create_register_op(?FACILITY_ADDRESS_KEY,Address),
        create_register_op(?FACILITY_TYPE_KEY,Type)
    ];
gen_entity_update(prescription,EntityFields) ->
    [PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs] = EntityFields,
    [
        create_register_op(?PRESCRIPTION_ID_KEY,PrescriptionId),
        create_register_op(?PRESCRIPTION_PATIENT_ID_KEY,PatientId),
        create_register_op(?PRESCRIPTION_PRESCRIBER_ID_KEY,PrescriberId),
        create_register_op(?PRESCRIPTION_PHARMACY_ID_KEY,PharmacyId),
        create_register_op(?PRESCRIPTION_DATE_PRESCRIBED_KEY,DatePrescribed),
        create_set_op(?PRESCRIPTION_DRUGS_KEY,Drugs)
    ];
gen_entity_update(patient,EntityFields) ->
    [Id,Name,Address] = EntityFields,
    [
        create_register_op(?PATIENT_ID_KEY,Id),
        create_register_op(?PATIENT_NAME_KEY,Name),
        create_register_op(?PATIENT_ADDRESS_KEY,Address)
    ].

gen_nested_entity_update(prescription, TopLevelKey, EntityFields) ->
    [PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs] = EntityFields,
    NestedOps = [
        create_register_op(?PRESCRIPTION_ID_KEY,PrescriptionId),
        create_register_op(?PRESCRIPTION_PATIENT_ID_KEY,PatientId),
        create_register_op(?PRESCRIPTION_PRESCRIBER_ID_KEY,PrescriberId),
        create_register_op(?PRESCRIPTION_PHARMACY_ID_KEY,PharmacyId),
        create_register_op(?PRESCRIPTION_DATE_PRESCRIBED_KEY,DatePrescribed),
        create_set_op(?PRESCRIPTION_DRUGS_KEY,Drugs)
    ],
    update_map_op(TopLevelKey,[create_map_op(gen_key(prescription,PrescriptionId),NestedOps)]).


handle_get_result_for_create_op(Entity,EntityFields,{{error,not_found},Context})
        when is_atom(Entity), is_list(EntityFields) ->
    Id = hd(EntityFields), %% Assumes ID is always the first field in the field list.
    EntityKey = gen_key(Entity,Id),
    EntityUpdate = gen_entity_update(Entity,EntityFields),
    execute_create_op(Context,EntityKey,Entity,EntityUpdate);

handle_get_result_for_create_op(Entity,EntityFields,{{ok, _Object}, Context})
        when is_atom(Entity), is_list(EntityFields) ->
    {{error, list_to_atom(lists:flatten(io_lib:format("~p_id_taken",[Entity])))}, Context}.

check_keys(_Context,[]) ->
    [];
check_keys(Context, [H|T]) ->
    case execute_get_op(Context,H) of
        {{error, not_found}, Context1} -> [{free, H}] ++ check_keys(Context1,T);
        {{ok, _Object}, Context2} -> [{taken, H}] ++ check_keys(Context2,T)
    end.

update_map_op(Key,NestedOps) ->
    {update_map, Key, NestedOps}.

create_map_op(Key,NestedOps) ->
    {create_map, Key, NestedOps}.

create_register_op(Key,Value) ->
    {create_register, Key, Value}.

create_set_op(Key, Elements) ->
    {create_set, Key, Elements}.

gen_key(Entity,Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p",[Entity,Id]))).

gen_patient_key(Id) ->
    gen_key(patient,Id).

gen_pharmacy_key(Id) ->
    gen_key(pharmacy,Id).

gen_event_key(Id) ->
    gen_key(event,Id).

gen_staff_key(Id) ->
    gen_key(staff,Id).

gen_facility_key(Id) ->
    gen_key(facility,Id).

gen_prescription_key(Id) ->
    gen_key(prescription,Id).

%% -------------------------------------------------------------------
%% Setup and teardown functions (simply pass down to db module)
%% -------------------------------------------------------------------

init(Params) ->
    (?KV_IMPLEMENTATION()):init(Params).

stop(State) ->
    (?KV_IMPLEMENTATION()):stop(State).

%% -------------------------------------------------------------------
%% Unimplemented functions (listed below to accept the fmke_gen_kv_driver interface):
%% -------------------------------------------------------------------
create_event(_1,_2,_3,_4,_5) ->
    erlang:error(not_implemented).

create_prescription(_1,_2,_3,_4,_5,_6,_7,_8) ->
    erlang:error(not_implemented).

create_treatment(_1,_2,_3,_4,_5) ->
    erlang:error(not_implemented).

get_facility_treatments(_1,_2) ->
    erlang:error(not_implemented).

get_staff_treatments(_1,_2) ->
    erlang:error(not_implemented).

get_treatment_by_id(_1,_2) ->
    erlang:error(not_implemented).
