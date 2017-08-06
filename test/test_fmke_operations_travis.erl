-module (test_fmke_operations_travis).
-include("fmke.hrl").
-include("fmk_kv.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define (STATIC_ID, 1).
-define (STATIC_ADDRESS,"https://github.com/goncalotomas/FMKe").
-define (STATIC_ADDRESS_UPDATED,"https://github.com/goncalotomas/FMKe.git").
-define (STATIC_PATIENT_NAME,"FMKe patient").
-define (STATIC_PATIENT_NAME_UPDATED, "FMKe patient updated").
-define (STATIC_PHARMACY_NAME,"FMKe pharmacy").
-define (STATIC_PHARMACY_NAME_UPDATED, "FMKe pharmacy updated").
-define (STATIC_FACILITY_NAME,"FMKe facility").
-define (STATIC_FACILITY_NAME_UPDATED, "FMKe facility updated").
-define (STATIC_FACILITY_TYPE, "FMKe Hospital").
-define (STATIC_FACILITY_TYPE_UPDATED, "FMKe Hospital updated").
-define (STATIC_STAFF_NAME,"FMKe doctor name").
-define (STATIC_STAFF_NAME_UPDATED, "FMKe doctor name updated").
-define (STATIC_STAFF_SPECIALITY, "FMKe doctor speciality").
-define (STATIC_STAFF_SPECIALITY_UPDATED, "FMKe doctor speciality updated").
-define (UNDEFINED_FIELD, <<"undefined">>).
-endif.

-ifdef(TEST).

connection_test_() ->
    {"Runs a quick ping test between the current test node and the local FMKe node",
    ?setup(fun test_connection/1)}.

patient_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the patient record"
    " functionality and that serve as unit tests for those operations.",
    ?setup(fun run_patient_operations/1)}.

pharmacy_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the pharmacy record"
    " functionality and that serve as unit tests for those operations.",
    ?setup(fun run_pharmacy_operations/1)}.

facility_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the facility record"
    " functionality and that serve as unit tests for those operations.",
    ?setup(fun run_facility_operations/1)}.

staff_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the staff record"
    " functionality and that serve as unit tests for those operations.",
    ?setup(fun run_staff_operations/1)}.

prescription_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the prescription record"
    " functionality and that serve as unit tests for those operations.",
    ?setup(fun run_prescription_operations/1)}.

start() ->
    Task = rand:uniform(100),
    Pname = build_generic_op("test_ops_travis_~p@127.0.0.1",[Task]),
    net_kernel:start([Pname,longnames]),
    erlang:set_cookie(node(),fmke),
    'fmke@127.0.0.1'.

stop(_FmkeNode) ->
    net_kernel:stop().

test_connection(FmkeNode) ->
    [?_assertEqual(pong, net_adm:ping(FmkeNode))].

run_patient_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_patient(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_patient(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,patient_id_taken},create_static_patient(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #patient{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PATIENT_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          prescriptions=[]
        },
        get_static_patient(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_patient(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #patient{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PATIENT_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          prescriptions=[]
        },
        get_static_patient(FmkeNode))
    ].

run_pharmacy_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_pharmacy(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_pharmacy(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,pharmacy_id_taken},create_static_pharmacy(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #pharmacy{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PHARMACY_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          prescriptions=[]
        },
        get_static_pharmacy(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_pharmacy(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #pharmacy{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PHARMACY_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          prescriptions=[]
        },
        get_static_pharmacy(FmkeNode))
    ].

run_facility_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_facility(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_facility(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,facility_id_taken},create_static_facility(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #facility{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_FACILITY_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          type=list_to_binary(?STATIC_FACILITY_TYPE)
        },
        get_static_facility(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_facility(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #facility{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_FACILITY_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          type=list_to_binary(?STATIC_FACILITY_TYPE_UPDATED)
        },
        get_static_facility(FmkeNode))
    ].

run_staff_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_staff(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_staff(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,staff_id_taken},create_static_staff(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #staff{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_STAFF_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          speciality=list_to_binary(?STATIC_STAFF_SPECIALITY)
        },
        get_static_staff(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_staff(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #staff{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_STAFF_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          speciality=list_to_binary(?STATIC_STAFF_SPECIALITY_UPDATED)
        },
        get_static_staff(FmkeNode))
    ].

run_prescription_operations(FmkeNode) ->
    %% generating required fields to pattern match on later
    DatePrescribed1 = gen_rand_date(),
    DatePrescribed2 = gen_rand_date(),
    DateProcessed1 = gen_rand_date(),
    Drugs1 = gen_drugs(),
    Drugs2 = gen_drugs(),
    NewDrugs1 = gen_drugs(),
    NewDrugList1 = lists:append(Drugs1,NewDrugs1),
    BinStaticId = integer_to_binary(?STATIC_ID),
    BinDate1 = list_to_binary(DatePrescribed1),
    %BinDate2 = list_to_binary(DatePrescribed2),
    BinDrugs1 = [list_to_binary(X) || X <- Drugs1],
    %BinDrugs2 = [list_to_binary(X) || X <- Drugs2],
    [
    %% assumes that data store already contains basic entities such as patients,
    %% hospitals, doctors and pharmacies.
    %% get unexistant first prescription
    ?_assertEqual({error,not_found},get_static_prescription(FmkeNode,?STATIC_ID))
    %% normal create for first prescription
    ,?_assertEqual(ok,create_static_prescription(FmkeNode,?STATIC_ID,DatePrescribed1,Drugs1))
    %% create when record already exists (purposely passing in details of prescription 2...)
    ,?_assertEqual({error,prescription_id_taken},create_static_prescription(FmkeNode,?STATIC_ID,DatePrescribed2,Drugs2))
    %% normal get for first prescription
    ,?_assert(verify_prescription_fields(FmkeNode,prescription,[
      ?STATIC_ID,DatePrescribed1,?UNDEFINED_FIELD,Drugs1,?PRESCRIPTION_NOT_PROCESSED_VALUE
      ]))
    %% check that prescription is present inside each basic entity
    %% check if prescription is inside the patient record
    ,?_assert(fetch_prescription(FmkeNode,patient,#prescription{
        id = BinStaticId
        ,patient_id = BinStaticId
        ,pharmacy_id = BinStaticId
        ,prescriber_id = BinStaticId
        ,date_prescribed = BinDate1
        ,date_processed = ?UNDEFINED_FIELD
        ,drugs = BinDrugs1
        ,is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE
    }))
    %% check if prescription is inside the pharmacy record
    ,?_assert(fetch_prescription(FmkeNode,pharmacy,#prescription{
        id = BinStaticId
        ,patient_id = BinStaticId
        ,pharmacy_id = BinStaticId
        ,prescriber_id = BinStaticId
        ,date_prescribed = BinDate1
        ,date_processed = ?UNDEFINED_FIELD
        ,drugs = BinDrugs1
        ,is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE
    }))
    %% check if prescription is inside the staff record
    ,?_assert(fetch_prescription(FmkeNode,staff,#prescription{
        id = BinStaticId
        ,patient_id = BinStaticId
        ,pharmacy_id = BinStaticId
        ,prescriber_id = BinStaticId
        ,date_prescribed = BinDate1
        ,date_processed = ?UNDEFINED_FIELD
        ,drugs = BinDrugs1
        ,is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE
    }))
    ,?_assertEqual({error,invalid_update_operation},update_presc_drugs_w_non_valid_op(FmkeNode,remove_drugs,?STATIC_ID,NewDrugs1))
    ,?_assertEqual(ok,add_static_prescription_drugs(FmkeNode,?STATIC_ID,NewDrugs1))
    ,?_assert(verify_prescription_fields(FmkeNode,prescription,[
      ?STATIC_ID,DatePrescribed1,?UNDEFINED_FIELD,NewDrugList1,?PRESCRIPTION_NOT_PROCESSED_VALUE
      ]))
    ,?_assertEqual(ok,process_static_prescription(FmkeNode,?STATIC_ID,DateProcessed1))
    ,?_assert(verify_prescription_fields(FmkeNode,prescription,[
      ?STATIC_ID,DatePrescribed1,DateProcessed1,NewDrugList1,?PRESCRIPTION_PROCESSED_VALUE
      ]))
    ,?_assertEqual({error,prescription_already_processed},add_static_prescription_drugs(FmkeNode,?STATIC_ID,NewDrugs1))
    ,?_assertEqual({error,prescription_already_processed},process_static_prescription(FmkeNode,?STATIC_ID,DateProcessed1))
    ].

add_static_prescription_drugs(FmkeNode,PrescriptionId,ListDrugs) ->
    run_rpc_op(FmkeNode,update_prescription_medication,[PrescriptionId,add_drugs,ListDrugs]).

update_presc_drugs_w_non_valid_op(FmkeNode,Operation,PrescriptionId,ListDrugs) ->
    run_rpc_op(FmkeNode,update_prescription_medication,[PrescriptionId,Operation,ListDrugs]).

process_static_prescription(FmkeNode,PrescriptionId,DateProcessed) ->
    run_rpc_op(FmkeNode,process_prescription,[PrescriptionId,DateProcessed]).

fetch_prescription(FmkeNode,patient,ExpectedPrescription) ->
    PrescriptionList = (get_static_patient(FmkeNode))#patient.prescriptions,
    look_for_prescription(PrescriptionList,prescription,ExpectedPrescription);

fetch_prescription(FmkeNode,pharmacy,ExpectedPrescription) ->
    PrescriptionList = (get_static_pharmacy(FmkeNode))#pharmacy.prescriptions,
    look_for_prescription(PrescriptionList,prescription,ExpectedPrescription);

fetch_prescription(FmkeNode,staff,ExpectedPrescription) ->
    PrescriptionList = (get_static_staff(FmkeNode))#staff.prescriptions,
    look_for_prescription(PrescriptionList,prescription,ExpectedPrescription).

look_for_prescription(PrescriptionList,TypePrescriptions,ExpectedPrescription) ->
    Results = lists:map(
        fun(Prescription) ->
            cmp_presc_fields(TypePrescriptions,Prescription,ExpectedPrescription)
        end
    ,PrescriptionList),
    lists:member(true, Results).

verify_prescription_fields(FmkeNode,PrescriptionType,[Id,DatePrescribed,DateProcessed,Drugs,IsProcessed]) when is_list(DatePrescribed) ->
    verify_prescription_fields(FmkeNode,PrescriptionType,[Id,list_to_binary(DatePrescribed),DateProcessed,Drugs,IsProcessed]);

verify_prescription_fields(FmkeNode,PrescriptionType,[Id,DatePrescribed,DateProcessed,Drugs,IsProcessed]) when is_list(DateProcessed) ->
    verify_prescription_fields(FmkeNode,PrescriptionType,[Id,DatePrescribed,list_to_binary(DateProcessed),Drugs,IsProcessed]);

verify_prescription_fields(FmkeNode,PrescriptionType,[Id,DatePrescribed,DateProcessed,Drugs,IsProcessed]) ->
    DBPrescription = get_static_prescription(FmkeNode,Id),
    %% static id is used for the basic entities key
    BinStaticId = integer_to_binary(?STATIC_ID),
    cmp_presc_fields(PrescriptionType,DBPrescription, #prescription{
      id=BinStaticId
      ,patient_id = BinStaticId
      ,pharmacy_id = BinStaticId
      ,prescriber_id = BinStaticId
      ,date_prescribed = DatePrescribed
      ,date_processed = DateProcessed
      ,drugs = [list_to_binary(X) || X <- Drugs]
      ,is_processed = IsProcessed
    }).

cmp_presc_fields(prescription,DBPrescription = #prescription{}, Expected = #prescription{}) ->
    (DBPrescription#prescription.patient_id =:= Expected#prescription.patient_id)
    and (DBPrescription#prescription.pharmacy_id =:= Expected#prescription.pharmacy_id)
    and (DBPrescription#prescription.prescriber_id =:= Expected#prescription.prescriber_id)
    and (DBPrescription#prescription.id =:= Expected#prescription.id)
    and (DBPrescription#prescription.date_prescribed =:= Expected#prescription.date_prescribed)
    and (DBPrescription#prescription.date_processed =:= Expected#prescription.date_processed)
    and (DBPrescription#prescription.is_processed =:= Expected#prescription.is_processed)
    and cmp_drug_list(DBPrescription#prescription.drugs,Expected#prescription.drugs).

cmp_drug_list(List1, List2) ->
    lists:sort(List1) =:= lists:sort(List2).

create_static_patient(FmkeNode) ->
    run_generic_create_op(FmkeNode,patient,[?STATIC_ID,?STATIC_PATIENT_NAME,?STATIC_ADDRESS]).

create_static_pharmacy(FmkeNode) ->
    run_generic_create_op(FmkeNode,pharmacy,[?STATIC_ID,?STATIC_PHARMACY_NAME,?STATIC_ADDRESS]).

create_static_facility(FmkeNode) ->
    run_generic_create_op(FmkeNode,facility,[?STATIC_ID,?STATIC_FACILITY_NAME,?STATIC_ADDRESS,?STATIC_FACILITY_TYPE]).

create_static_staff(FmkeNode) ->
    run_generic_create_op(FmkeNode,staff,[?STATIC_ID,?STATIC_STAFF_NAME,?STATIC_ADDRESS,?STATIC_STAFF_SPECIALITY]).

create_static_prescription(FmkeNode,Id,Date,Drugs) ->
    run_generic_create_op(FmkeNode,prescription,[Id,?STATIC_ID,?STATIC_ID,?STATIC_ID,Date,Drugs]).

get_static_patient(FmkeNode) ->
    run_generic_get_op(FmkeNode,patient).

get_static_pharmacy(FmkeNode) ->
    run_generic_get_op(FmkeNode,pharmacy).

get_static_facility(FmkeNode) ->
    run_generic_get_op(FmkeNode,facility).

get_static_staff(FmkeNode) ->
    run_generic_get_op(FmkeNode,staff).

get_static_prescription(FmkeNode,Id) ->
    run_get_op(FmkeNode,prescription,Id).

update_static_patient(FmkeNode) ->
    run_generic_update_op(FmkeNode,patient,[?STATIC_ID,?STATIC_PATIENT_NAME_UPDATED,?STATIC_ADDRESS_UPDATED]).

update_static_pharmacy(FmkeNode) ->
    run_generic_update_op(FmkeNode,pharmacy,[?STATIC_ID,?STATIC_PHARMACY_NAME_UPDATED,?STATIC_ADDRESS_UPDATED]).

update_static_facility(FmkeNode) ->
    run_generic_update_op(FmkeNode,facility,[?STATIC_ID,?STATIC_FACILITY_NAME_UPDATED,?STATIC_ADDRESS_UPDATED,?STATIC_FACILITY_TYPE_UPDATED]).

update_static_staff(FmkeNode) ->
    run_generic_update_op(FmkeNode,staff,[?STATIC_ID,?STATIC_STAFF_NAME_UPDATED,?STATIC_ADDRESS_UPDATED,?STATIC_STAFF_SPECIALITY_UPDATED]).

run_generic_create_op(FmkeNode,Entity,Args) ->
    OpAtom = build_create_op(Entity),
    run_rpc_op(FmkeNode,OpAtom,Args).

run_generic_update_op(FmkeNode,Entity,Args) ->
    OpAtom = build_update_op(Entity),
    run_rpc_op(FmkeNode,OpAtom,Args).

run_generic_get_op(FmkeNode,Entity) ->
    run_get_op(FmkeNode,Entity,?STATIC_ID).

run_get_op(FmkeNode,Entity,Id) ->
    OpAtom = build_get_op(Entity),
    run_rpc_op(FmkeNode,OpAtom,[Id]).

gen_rand_date() ->
    Day = rand:uniform(20)+10,
    Month = rand:uniform(20)+10,
    Year = rand:uniform(2000)+1000,
    format_list("~p/~p/~p",[Day,Month,Year]).

gen_drugs() ->
    NumDrugs = rand:uniform(3)+1,
    gen_drugs_rec(NumDrugs).

gen_drugs_rec(1) ->
    [gen_rand_str()];
gen_drugs_rec(Num) when Num > 1 ->
    [gen_rand_str()] ++ gen_drugs_rec(Num-1).

gen_rand_str() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(32))).

build_update_op(Entity) ->
    build_generic_op("update_~p_details",[Entity]).

build_create_op(Entity) ->
    build_generic_op("create_~p",[Entity]).

build_get_op(Entity) ->
    build_generic_op("get_~p_by_id",[Entity]).

build_generic_op(List,Args) ->
    list_to_atom(format_list(List,Args)).

format_list(List,Args) ->
    lists:flatten(io_lib:format(List,Args)).

run_rpc_op(FmkeNode, Op, Params) ->
    rpc:block_call(FmkeNode, fmke, Op, Params).

-endif.
