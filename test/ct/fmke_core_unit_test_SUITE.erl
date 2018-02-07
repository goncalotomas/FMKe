-module(fmke_core_unit_test_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("fmke.hrl").
-include("fmk_kv.hrl").

-define (NODENAME, 'fmke@127.0.0.1').
-define (COOKIE, fmke).

%%%-------------------------------------------------------------------
%%% Common Test exports
%%%-------------------------------------------------------------------
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%-------------------------------------------------------------------
%%% Every Common test set to be performed will be called by a function
%%% with the same name as atoms returned in the all/0 function.
%%%-------------------------------------------------------------------
-export([
    event_unit_tests/1,
    facility_unit_tests/1,
    patient_unit_tests/1,
    pharmacy_unit_tests/1,
    prescription_unit_tests/1,
    staff_unit_tests/1,
    treatment_unit_tests/1
]).

%%%-------------------------------------------------------------------
%%% Common Test Callbacks
%%%-------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 180}}].

%% returns a list of all test sets to be executed by Common Test.
all() ->
    [{group, antidote}, {group, redis}, {group, riak}].

%%%-------------------------------------------------------------------
%%% Common Test configuration
%%%-------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = net_kernel:start(['fmke_ct_test@127.0.0.1']),
    true = erlang:set_cookie('fmke_ct_test@127.0.0.1', ?COOKIE),
    Config.

end_per_suite(_Config) ->
    net_kernel:stop(),
    ok.

groups() ->
    [{antidote, [shuffle, sequence], [
        event_unit_tests, facility_unit_tests, patient_unit_tests, pharmacy_unit_tests,
        prescription_unit_tests, staff_unit_tests, treatment_unit_tests
    ]},
    {riak, [shuffle, sequence], [
        event_unit_tests, facility_unit_tests, patient_unit_tests, pharmacy_unit_tests,
        prescription_unit_tests, staff_unit_tests, treatment_unit_tests
    ]},
    {redis, [shuffle, sequence], [
        event_unit_tests, facility_unit_tests, patient_unit_tests, pharmacy_unit_tests,
        prescription_unit_tests, staff_unit_tests, treatment_unit_tests
    ]}].

init_per_group(antidote, Config) ->
    Node = fmke_test_utils:start_node_with_antidote_backend(?NODENAME),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(riak, Config) ->
    Node = fmke_test_utils:start_node_with_riak_backend(?NODENAME),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(redis, Config) ->
    Node = fmke_test_utils:start_node_with_redis_backend(?NODENAME),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(antidote, _Config) ->
    fmke_test_utils:stop_antidote(),
    ct_slave:stop(?NODENAME),
    ok;
end_per_group(riak, _Config) ->
    fmke_test_utils:stop_riak(),
    ct_slave:stop(?NODENAME),
    ok;
end_per_group(redis, _Config) ->
    fmke_test_utils:stop_redis(),
    ct_slave:stop(?NODENAME),
    ok;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(facility_unit_tests, Config) ->
    TabId = ets:new(facilities, [set, protected, named_table]),
    FacilityId = rand:uniform(1000000000000),
    ets:insert(TabId, {facility, FacilityId, "Some Hospital", "Somewhere", "Hospital"}),
    ets:insert(TabId, {updated_facility, FacilityId, "Some Random Hospital", "Somewhere Portugal", "Treatment Facility"}),
    [{table,TabId} | Config];

init_per_testcase(patient_unit_tests, Config) ->
    TabId = ets:new(patients, [set, protected, named_table]),
    PatientId = rand:uniform(1000000000000),
    ets:insert(TabId, {patient, PatientId, "Goncalo Tomas", "Somewhere in Portugal"}),
    ets:insert(TabId, {updated_patient, PatientId, "Goncalo P. Tomas", "Caparica, Portugal"}),
    [{table,TabId} | Config];

init_per_testcase(pharmacy_unit_tests, Config) ->
    TabId = ets:new(pharmacies, [set, protected, named_table]),
    PharmacyId = rand:uniform(1000000000000),
    ets:insert(TabId, {pharmacy, PharmacyId, "Some Pharmacy", "Somewhere in Portugal"}),
    ets:insert(TabId, {updated_pharmacy, PharmacyId, "Some Random Pharmacy", "Caparica, Portugal"}),
    [{table,TabId} | Config];

init_per_testcase(prescription_unit_tests, Config) ->
    TabId = ets:new(prescriptions, [set, protected, named_table]),
    [R1, R2, R3, R4, R5] = lists:map(fun(_) -> rand:uniform(1000000000000) end, lists:seq(1,5)),
    ets:insert(TabId, {patient, R1, "Goncalo Tomas", "Somewhere in Portugal"}),
    ets:insert(TabId, {other_patient, R1+1, "Goncalo P. Tomas", "Caparica, Portugal"}),
    ets:insert(TabId, {facility, R2, "Some Hospital", "Somewhere", "Hospital"}),
    ets:insert(TabId, {other_facility, R2+1, "Some Random Hospital", "Somewhere Portugal", "Treatment Facility"}),
    ets:insert(TabId, {pharmacy, R3, "Some Pharmacy", "Somewhere in Portugal"}),
    ets:insert(TabId, {other_pharmacy, R3+1, "Some Random Pharmacy", "Caparica, Portugal"}),
    ets:insert(TabId, {staff, R4, "Some Doctor", "Somewhere in Portugal", "Traditional Chinese Medicine"}),
    ets:insert(TabId, {other_staff, R4+1, "Some Random Doctor", "Caparica, Portugal", "weird esoteric kind of medicine"}),
    ets:insert(TabId, {prescription, R5, R1, R4, R3, "12/12/2012", ["Penicillin", "Diazepam"]}),
    ets:insert(TabId, {updated_prescription_drugs, R5, ["Adrenaline"]}),
    ets:insert(TabId, {processed_prescription_date, R5, "24/12/2012"}),
    ets:insert(TabId, {other_prescription, R5+1, R1+1, R4+1, R3+1, "01/10/2015", ["Diazepam"]}),
    ets:insert(TabId, {other_updated_prescription_drugs, R5+1, ["Penicillin", "Adrenaline"]}),
    ets:insert(TabId, {other_processed_prescription_date, R5+1, "01/01/2016"}),
    [{table,TabId} | Config];

init_per_testcase(staff_unit_tests, Config) ->
    TabId = ets:new(staff, [set, protected, named_table]),
    StaffId = rand:uniform(1000000000000),
    ets:insert(TabId, {staff, StaffId, "Some Doctor", "Somewhere in Portugal", "Traditional Chinese Medicine"}),
    ets:insert(TabId, {updated_staff, StaffId, "Some Random Doctor", "Caparica, Portugal", "weird esoteric kind of medicine"}),
    [{table,TabId} | Config];

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(facility_unit_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(patient_unit_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(pharmacy_unit_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(prescription_unit_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(staff_unit_tests, Config) ->
    ets:delete(?config(table, Config));

end_per_testcase(_, Config) ->
    Config.

event_unit_tests(_Config) ->
    % {skip, "Events not implemented in this version of FMKe."}.
    ok.


%%%-------------------------------------------------------------------
%%% facility endpoint tests
%%%-------------------------------------------------------------------


facility_unit_tests(Config) ->
    get_unexisting_facility(Config),
    add_unexisting_facility(Config),
    get_existing_facility(Config),
    add_existing_facility(Config),
    update_existing_facility(Config),
    update_unexistent_facility(Config),
    get_facility_after_update(Config).

get_unexisting_facility(Config) ->
    TabId = ?config(table, Config),
    [{facility, FacilityId, _, _, _}] = ets:lookup(TabId, facility),
    {error, not_found} = rpc(Config, get_facility_by_id, [FacilityId]).

add_unexisting_facility(Config) ->
    TabId = ?config(table, Config),
    [{facility, Id, Name, Address, Type}] = ets:lookup(TabId, facility),
    ok = rpc(Config, create_facility, [Id, Name, Address, Type]).

get_existing_facility(Config) ->
    TabId = ?config(table, Config),
    [{facility, Id, Name, Address, Type}] = ets:lookup(TabId, facility),
    #facility{id=RemId, name=RemName, address=RemAddress, type=RemType} = rpc(Config, get_facility_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = list_to_binary(Type) =:= RemType.

add_existing_facility(Config) ->
    TabId = ?config(table, Config),
    [{facility, Id, Name, Address, Type}] = ets:lookup(TabId, facility),
    {error, facility_id_taken} = rpc(Config, create_facility, [Id, Name, Address, Type]).

update_existing_facility(Config) ->
    TabId = ?config(table, Config),
    [{updated_facility, Id, Name, Address, Type}] = ets:lookup(TabId, updated_facility),
    ok = rpc(Config, update_facility_details, [Id, Name, Address, Type]).

update_unexistent_facility(Config) ->
    TabId = ?config(table, Config),
    [{updated_facility, Id, Name, Address, Type}] = ets:lookup(TabId, updated_facility),
    UnusedId = Id+rand:uniform(100000000),
    {error, no_such_facility} = rpc(Config, update_facility_details, [UnusedId, Name, Address, Type]).

get_facility_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_facility, Id, Name, Address, Type}] = ets:lookup(TabId, updated_facility),
    #facility{id=RemId, name=RemName, address=RemAddress, type=RemType} = rpc(Config, get_facility_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = list_to_binary(Type) =:= RemType.

%%%-------------------------------------------------------------------
%%% patient endpoint tests
%%%-------------------------------------------------------------------


patient_unit_tests(Config) ->
    get_unexisting_patient(Config),
    add_unexisting_patient(Config),
    get_existing_patient(Config),
    add_existing_patient(Config),
    update_existing_patient(Config),
    update_unexistent_patient(Config),
    get_patient_after_update(Config).

get_unexisting_patient(Config) ->
    TabId = ?config(table, Config),
    [{patient, PatientId, _, _}] = ets:lookup(TabId, patient),
    {error, not_found} = rpc(Config, get_patient_by_id, [PatientId]).

add_unexisting_patient(Config) ->
    TabId = ?config(table, Config),
    [{patient, Id, Name, Address}] = ets:lookup(TabId, patient),
    ok = rpc(Config, create_patient, [Id, Name, Address]).

get_existing_patient(Config) ->
    TabId = ?config(table, Config),
    [{patient, Id, Name, Address}] = ets:lookup(TabId, patient),
    #patient{id=RemId, name=RemName, address=RemAddress, prescriptions=RemPrscs} = rpc(Config, get_patient_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = [] =:= RemPrscs.

add_existing_patient(Config) ->
    TabId = ?config(table, Config),
    [{patient, Id, Name, Address}] = ets:lookup(TabId, patient),
    {error, patient_id_taken} = rpc(Config, create_patient, [Id, Name, Address]).

update_existing_patient(Config) ->
    TabId = ?config(table, Config),
    [{updated_patient, Id, Name, Address}] = ets:lookup(TabId, updated_patient),
    ok = rpc(Config, update_patient_details, [Id, Name, Address]).

update_unexistent_patient(Config) ->
    TabId = ?config(table, Config),
    [{updated_patient, Id, Name, Address}] = ets:lookup(TabId, updated_patient),
    UnusedId = Id+rand:uniform(100000000),
    {error, no_such_patient} = rpc(Config, update_patient_details, [UnusedId, Name, Address]).

get_patient_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_patient, Id, Name, Address}] = ets:lookup(TabId, updated_patient),
    #patient{id=RemId, name=RemName, address=RemAddress, prescriptions=RemPrscs} = rpc(Config, get_patient_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = [] =:= RemPrscs.


%%%-------------------------------------------------------------------
%%% pharmacy endpoint tests
%%%-------------------------------------------------------------------


pharmacy_unit_tests(Config) ->
    get_unexisting_pharmacy(Config),
    add_unexisting_pharmacy(Config),
    get_existing_pharmacy(Config),
    add_existing_pharmacy(Config),
    update_existing_pharmacy(Config),
    update_unexistent_pharmacy(Config),
    get_pharmacy_after_update(Config).

get_unexisting_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmacyId, _, _}] = ets:lookup(TabId, pharmacy),
    {error, not_found} = rpc(Config, get_pharmacy_by_id, [PharmacyId]).

add_unexisting_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, Id, Name, Address}] = ets:lookup(TabId, pharmacy),
    ok = rpc(Config, create_pharmacy, [Id, Name, Address]).

get_existing_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, Id, Name, Address}] = ets:lookup(TabId, pharmacy),
    #pharmacy{id=RemId, name=RemName, address=RemAddress, prescriptions=RemPrescs} = rpc(Config, get_pharmacy_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = [] =:= RemPrescs.

add_existing_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, Id, Name, Address}] = ets:lookup(TabId, pharmacy),
    {error, pharmacy_id_taken} = rpc(Config, create_pharmacy, [Id, Name, Address]).

update_existing_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{updated_pharmacy, Id, Name, Address}] = ets:lookup(TabId, updated_pharmacy),
    ok = rpc(Config, update_pharmacy_details, [Id, Name, Address]).

update_unexistent_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{updated_pharmacy, Id, Name, Address}] = ets:lookup(TabId, updated_pharmacy),
    UnusedId = Id+rand:uniform(100000000),
    {error, no_such_pharmacy} = rpc(Config, update_pharmacy_details, [UnusedId, Name, Address]).

get_pharmacy_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_pharmacy, Id, Name, Address}] = ets:lookup(TabId, updated_pharmacy),
    #pharmacy{id=RemId, name=RemName, address=RemAddress, prescriptions=RemPrescs} = rpc(Config, get_pharmacy_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = [] =:= RemPrescs.


%%%-------------------------------------------------------------------
%%% prescription endpoint tests
%%%-------------------------------------------------------------------


prescription_unit_tests(Config) ->
    add_required_entities(Config),
    get_unexisting_prescription(Config),
    process_unexisting_prescription(Config),
    add_medication_to_unexisting_prescription(Config),
    add_unexisting_prescription(Config),
    get_existing_prescription(Config),
    add_existing_prescription(Config),
    update_prescription_medication(Config),
    process_existing_non_processed_prescription(Config),
    add_medication_to_processed_prescription(Config),
    process_already_processed_prescription(Config),
    get_prescription_after_updates(Config).

add_required_entities(Config) ->
    TabId = ?config(table, Config),
    [{facility, FacId1, FacName1, FacAddr1, FacType1}] = ets:lookup(TabId, facility),
    [{other_facility, FacId2, FacName2, FacAddr2, FacType2}] = ets:lookup(TabId, other_facility),
    [{patient, PatId1, PatName1, PatAddr1}] = ets:lookup(TabId, patient),
    [{other_patient, PatId2, PatName2, PatAddr2}] = ets:lookup(TabId, other_patient),
    [{pharmacy, PharmId1, PharmName1, PharmAddress1}] = ets:lookup(TabId, pharmacy),
    [{other_pharmacy, PharmId2, PharmName2, PharmAddress2}] = ets:lookup(TabId, other_pharmacy),
    [{staff, StaId1, StaName1, StaAddr1, StaSpec1}] = ets:lookup(TabId, staff),
    [{other_staff, StaId2, StaName2, StaAddr2, StaSpec2}] = ets:lookup(TabId, other_staff),

    ok = rpc(Config, create_facility, [FacId1, FacName1, FacAddr1, FacType1]),
    ok = rpc(Config, create_facility, [FacId2, FacName2, FacAddr2, FacType2]),
    ok = rpc(Config, create_patient, [PatId1, PatName1, PatAddr1]),
    ok = rpc(Config, create_patient, [PatId2, PatName2, PatAddr2]),
    ok = rpc(Config, create_pharmacy, [PharmId1, PharmName1, PharmAddress1]),
    ok = rpc(Config, create_pharmacy, [PharmId2, PharmName2, PharmAddress2]),
    ok = rpc(Config, create_staff, [StaId1, StaName1, StaAddr1, StaSpec1]),
    ok = rpc(Config, create_staff, [StaId2, StaName2, StaAddr2, StaSpec2]).

get_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    {error, not_found} = rpc(Config, get_prescription_by_id, [Id]).

process_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    {error, no_such_prescription} = rpc(Config, process_prescription, [Id, "14/08/2017"]).

add_medication_to_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    {error, no_such_prescription} = rpc(Config, update_prescription_medication, [Id,
        add_drugs, ["RandomDrug1, RandomDrug2, RandomDrug3"]]).

add_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    ok = rpc(Config, create_prescription, [Id, PatId, PrescId, PharmId, DatePresc, Drugs]).

get_existing_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    PrescriptionObject = #prescription{
        id = RemId
        ,patient_id = RemPatId
        ,pharmacy_id = RemPharmId
        ,prescriber_id = RemPrescriberId
        ,date_prescribed = RemDatePresc
        ,date_processed = RemDateProc
        ,drugs = RemDrugs
        ,is_processed = RemIsProcessed
    } = rpc(Config, get_prescription_by_id, [Id]),

    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(integer_to_list(PatId)) =:= RemPatId,
    true = list_to_binary(integer_to_list(PharmId)) =:= RemPharmId,
    true = list_to_binary(integer_to_list(PrescId)) =:= RemPrescriberId,
    true = list_to_binary(DatePresc) =:= RemDatePresc,
    true = <<"undefined">> =:= RemDateProc,
    true = ?PRESCRIPTION_NOT_PROCESSED_VALUE =:= RemIsProcessed,

    BinDrugs = lists:sort(lists:map(fun list_to_binary/1, Drugs)),
    BinDrugs = lists:sort(RemDrugs),

    io:format("### KEY ###~n~p~n", [gen_key(prescription, Id)]),
    io:format("### EXPECTED ###~nID=~p~n", [PatId]),
    io:format("~p~n", [rpc(Config, get_patient_by_id, [PatId])]),

    %% check for same prescription inside patient, pharmacy and staff
    #patient{
        id = _BinPatId
        ,name = _Name1
        ,address = _Address1
        ,prescriptions = PatientPrescriptions
    } = rpc(Config, get_patient_by_id, [PatId]),

    PrescriptionKey = gen_key(prescription, Id),
    true = lists:member(PrescriptionObject, PatientPrescriptions)
            orelse lists:member(PrescriptionKey, PatientPrescriptions),

    #pharmacy{
        id = _BinPharmId
        ,name = _Name2
        ,address = _Address2
        ,prescriptions = PharmacyPrescriptions
    } = rpc(Config, get_pharmacy_by_id, [PharmId]),

    true = lists:member(PrescriptionObject, PharmacyPrescriptions)
            orelse lists:member(PrescriptionKey, PharmacyPrescriptions),

    #staff{
        id = _BinPrescId
        ,name = _Name3
        ,address = _Address3
        ,speciality = _Speciality
        ,prescriptions = StaffPrescriptions
    } = rpc(Config, get_staff_by_id, [PrescId]),

    true = lists:member(PrescriptionObject, StaffPrescriptions)
            orelse lists:member(PrescriptionKey, StaffPrescriptions).

add_existing_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    {error, prescription_id_taken} = rpc(Config, create_prescription, [Id, PatId, PrescId, PharmId, DatePresc, Drugs]).

update_prescription_medication(Config) ->
    TabId = ?config(table, Config),
    [{updated_prescription_drugs, Id, Drugs}] = ets:lookup(TabId, updated_prescription_drugs),
    ok = rpc(Config, update_prescription_medication, [Id, add_drugs, Drugs]).

process_existing_non_processed_prescription(Config) ->
    TabId = ?config(table, Config),
    [{processed_prescription_date, Id, Date}] = ets:lookup(TabId, processed_prescription_date),
    ok = rpc(Config, process_prescription, [Id, Date]).

add_medication_to_processed_prescription(Config) ->
    TabId = ?config(table, Config),
    [{updated_prescription_drugs, Id, Drugs}] = ets:lookup(TabId, updated_prescription_drugs),
    {error, prescription_already_processed} = rpc(Config, update_prescription_medication, [Id, add_drugs, Drugs]).

process_already_processed_prescription(Config) ->
    TabId = ?config(table, Config),
    [{processed_prescription_date, Id, Date}] = ets:lookup(TabId, processed_prescription_date),
    {error, prescription_already_processed} = rpc(Config, process_prescription, [Id, Date]).

get_prescription_after_updates(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    [{processed_prescription_date, Id, DateProc}] = ets:lookup(TabId, processed_prescription_date),
    [{updated_prescription_drugs, Id, AdditionalDrugs}] = ets:lookup(TabId, updated_prescription_drugs),
    PrescriptionObject = #prescription{
        id = RemId
        ,patient_id = RemPatId
        ,pharmacy_id = RemPharmId
        ,prescriber_id = RemPrescriberId
        ,date_prescribed = RemDatePresc
        ,date_processed = RemDateProc
        ,drugs = RemDrugs
        ,is_processed = RemIsProcessed
    } = rpc(Config, get_prescription_by_id, [Id]),

    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(integer_to_list(PatId)) =:= RemPatId,
    true = list_to_binary(integer_to_list(PharmId)) =:= RemPharmId,
    true = list_to_binary(integer_to_list(PrescId)) =:= RemPrescriberId,
    true = list_to_binary(DatePresc) =:= RemDatePresc,
    true = list_to_binary(DateProc) =:= RemDateProc,
    true = ?PRESCRIPTION_PROCESSED_VALUE =:= RemIsProcessed,

    BinDrugs = lists:sort(lists:map(fun list_to_binary/1, lists:append(Drugs, AdditionalDrugs))),
    BinDrugs = lists:sort(RemDrugs),

    %% check for same prescription inside patient, pharmacy and staff
    #patient{
        id = _BinPatId
        ,name = _Name1
        ,address = _Address1
        ,prescriptions = PatientPrescriptions
    } = rpc(Config, get_patient_by_id, [PatId]),

    PrescriptionKey = gen_key(prescription, Id),
    true = lists:member(PrescriptionObject, PatientPrescriptions)
            orelse lists:member(PrescriptionKey, PatientPrescriptions),

    #pharmacy{
        id = _BinPharmId
        ,name = _Name2
        ,address = _Address2
        ,prescriptions = PharmacyPrescriptions
    } = rpc(Config, get_pharmacy_by_id, [PharmId]),

    true = lists:member(PrescriptionObject, PharmacyPrescriptions)
            orelse lists:member(PrescriptionKey, PharmacyPrescriptions),

    #staff{
        id = _BinPrescId
        ,name = _Name3
        ,address = _Address3
        ,speciality = _Speciality
        ,prescriptions = StaffPrescriptions
    } = rpc(Config, get_staff_by_id, [PrescId]),

    true = lists:member(PrescriptionObject, StaffPrescriptions)
            orelse lists:member(PrescriptionKey, StaffPrescriptions).


%%%-------------------------------------------------------------------
%%% staff endpoint tests
%%%-------------------------------------------------------------------


staff_unit_tests(Config) ->
    get_unexisting_staff(Config),
    add_unexisting_staff(Config),
    get_existing_staff(Config),
    add_existing_staff(Config),
    update_existing_staff(Config),
    update_unexistent_staff(Config),
    get_staff_after_update(Config).

get_unexisting_staff(Config) ->
    TabId = ?config(table, Config),
    [{staff, StaffId, _, _, _}] = ets:lookup(TabId, staff),
    {error, not_found} = rpc(Config, get_staff_by_id, [StaffId]).

add_unexisting_staff(Config) ->
    TabId = ?config(table, Config),
    [{staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, staff),
    ok = rpc(Config, create_staff, [Id, Name, Address, Speciality]).

get_existing_staff(Config) ->
    TabId = ?config(table, Config),
    [{staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, staff),
    #staff{id=RemId, name=RemName, address=RemAddress, speciality=RemSpecial, prescriptions=RemPrescs} =
        rpc(Config, get_staff_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = list_to_binary(Speciality) =:= RemSpecial,
    true = [] =:= RemPrescs.

add_existing_staff(Config) ->
    TabId = ?config(table, Config),
    [{staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, staff),
    {error, staff_id_taken} = rpc(Config, create_staff, [Id, Name, Address, Speciality]).

update_existing_staff(Config) ->
    TabId = ?config(table, Config),
    [{updated_staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, updated_staff),
    ok = rpc(Config, update_staff_details, [Id, Name, Address, Speciality]).

update_unexistent_staff(Config) ->
    TabId = ?config(table, Config),
    [{updated_staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, updated_staff),
    UnusedId = Id+rand:uniform(100000000),
    {error, no_such_staff} = rpc(Config, update_staff_details, [UnusedId, Name, Address, Speciality]).

get_staff_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, updated_staff),
    #staff{id=RemId, name=RemName, address=RemAddress, speciality=RemSpecial, prescriptions=RemPrescs} =
        rpc(Config, get_staff_by_id, [Id]),
    true = list_to_binary(integer_to_list(Id)) =:= RemId,
    true = list_to_binary(Name) =:= RemName,
    true = list_to_binary(Address) =:= RemAddress,
    true = list_to_binary(Speciality) =:= RemSpecial,
    true = [] =:= RemPrescs.


%%%-------------------------------------------------------------------
%%% treatment endpoint tests
%%%-------------------------------------------------------------------


treatment_unit_tests(_Config) ->
    % {skip, "Treatments not implemented in this version of FMKe."}.
    ok.


%%%-------------------------------------------------------------------
%%% Auxiliary functions
%%%-------------------------------------------------------------------

rpc(Config, Op, Params) ->
    %%TODO get Node from configuration proplist
    Node = ?config(node, Config),
    rpc:block_call(Node, fmke, Op, Params).

gen_key(Entity,Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p",[Entity,Id]))).
