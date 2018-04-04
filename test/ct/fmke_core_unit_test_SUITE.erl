-module(fmke_core_unit_test_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("fmke.hrl").
-include("fmke_kv.hrl").

-define (NODENAME, 'fmke@127.0.0.1').
-define (COOKIE, fmke).
-define (TEST_BATTERY, [event_unit_tests, facility_unit_tests, patient_unit_tests, pharmacy_unit_tests,
                        prescription_unit_tests, staff_unit_tests, treatment_unit_tests, status_tests]).

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
    treatment_unit_tests/1,
    status_tests/1
]).

%%%-------------------------------------------------------------------
%%% Common Test Callbacks
%%%-------------------------------------------------------------------

suite() ->
    [{timetrap, {minutes, 3}}].

%% returns a list of all test sets to be executed by Common Test.
all() ->
    [
        % {group, simple_antidote_nested}
        % ,{group, simple_antidote_non_nested}
        % ,{group, opt_antidote}
        % ,{group, redis}
        % {group, simple_riak_nested}
        {group, simple_riak_non_nested}
        ,{group, ets_nested}
        ,{group, ets_non_nested}
    ].

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
    [
        % {simple_antidote_nested, [shuffle], ?TEST_BATTERY}
        % ,{simple_antidote_non_nested, [shuffle], ?TEST_BATTERY}
        % ,{opt_antidote, [shuffle], ?TEST_BATTERY}
        % ,{simple_redis_nested, [shuffle], ?TEST_BATTERY}
        % ,{simple_redis_non_nested, [shuffle], ?TEST_BATTERY}
        % ,{simple_riak_nested, [shuffle], ?TEST_BATTERY}
        {simple_riak_non_nested, [shuffle], ?TEST_BATTERY}
        % ,{opt_riak, [shuffle], ?TEST_BATTERY}
        ,{ets_nested, [shuffle], ?TEST_BATTERY}
        ,{ets_non_nested, [shuffle], ?TEST_BATTERY}
    ].

init_per_group(simple_antidote_nested, Config) ->
    Node = fmke_test_utils:start_node_with_antidote_backend(?NODENAME, false, nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(simple_antidote_non_nested, Config) ->
    Node = fmke_test_utils:start_node_with_antidote_backend(?NODENAME, false, non_nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(opt_antidote, Config) ->
    Node = fmke_test_utils:start_node_with_antidote_backend(?NODENAME, true, non_nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(simple_riak_nested, Config) ->
    Node = fmke_test_utils:start_node_with_riak_backend(?NODENAME, false, nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(simple_riak_non_nested, Config) ->
    Node = fmke_test_utils:start_node_with_riak_backend(?NODENAME, false, non_nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(opt_riak, Config) ->
    Node = fmke_test_utils:start_node_with_riak_backend(?NODENAME, true, non_nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(simple_redis_nested, Config) ->
    Node = fmke_test_utils:start_node_with_redis_backend(?NODENAME, false, nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(simple_redis_non_nested, Config) ->
    Node = fmke_test_utils:start_node_with_redis_backend(?NODENAME, false, non_nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(ets_nested, Config) ->
    Node = fmke_test_utils:start_node_with_ets_backend(?NODENAME, nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(ets_non_nested, Config) ->
    Node = fmke_test_utils:start_node_with_ets_backend(?NODENAME, non_nested),
    erlang:set_cookie(?NODENAME, ?COOKIE),
    [{node, Node} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    fmke_test_utils:stop_node(?NODENAME),
    fmke_test_utils:stop_all().

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
    cmp_facs([Id, Name, Address, Type], [RemId, RemName, RemAddress, RemType]).

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
    cmp_facs([Id, Name, Address, Type], [RemId, RemName, RemAddress, RemType]).

cmp_facs([Id1, Name1, Address1, Type1], [Id2, Name2, Address2, Type2]) ->
    true = Id1 =:= Id2 orelse list_to_binary(integer_to_list(Id1)) =:= Id2,
    true = Name1 =:= Name2 orelse list_to_binary(Name1) =:= Name2,
    true = Address1 =:= Address2 orelse list_to_binary(Address1) =:= Address2,
    true = Type1 =:= Type2 orelse list_to_binary(Type1) =:= Type2.

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
    cmp_pats([Id, Name, Address, []], [RemId, RemName, RemAddress, RemPrscs]).

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
    cmp_pats([Id, Name, Address, []], [RemId, RemName, RemAddress, RemPrscs]).

cmp_pats([Id1, Name1, Address1, Prescs1], [Id2, Name2, Address2, Prescs2]) ->
    true = Id1 =:= Id2 orelse list_to_binary(integer_to_list(Id1)) =:= Id2,
    true = Name1 =:= Name2 orelse list_to_binary(Name1) =:= Name2,
    true = Address1 =:= Address2 orelse list_to_binary(Address1) =:= Address2,
    true = Prescs1 =:= Prescs2.


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
    cmp_pharms([Id, Name, Address, []], [RemId, RemName, RemAddress, RemPrescs]).

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
    cmp_pharms([Id, Name, Address, []], [RemId, RemName, RemAddress, RemPrescs]).

cmp_pharms([Id1, Name1, Address1, Prescs1], [Id2, Name2, Address2, Prescs2]) ->
    true = Id1 =:= Id2 orelse list_to_binary(integer_to_list(Id1)) =:= Id2,
    true = Name1 =:= Name2 orelse list_to_binary(Name1) =:= Name2,
    true = Address1 =:= Address2 orelse list_to_binary(Address1) =:= Address2,
    true = Prescs1 =:= Prescs2.

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
        add_drugs, ["RandomDrug1", "RandomDrug2", "RandomDrug3"]]).

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

    cmp_prescs([Id, PatId, PrescId, PharmId, DatePresc, <<"undefined">>, Drugs, ?PRESCRIPTION_NOT_PROCESSED_VALUE],
                [RemId, RemPatId, RemPrescriberId, RemPharmId, RemDatePresc, RemDateProc, RemDrugs, RemIsProcessed]),

    %% check for same prescription inside patient, pharmacy and staff
    #patient{
        id = _BinPatId
        ,name = _Name1
        ,address = _Address1
        ,prescriptions = PatientPrescriptions
    } = rpc(Config, get_patient_by_id, [PatId]),

    true = chk_presc_or_ref_in_list(PrescriptionObject, PatientPrescriptions)
            orelse lists:member({prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}, PatientPrescriptions),

    #pharmacy{
        id = _BinPharmId
        ,name = _Name2
        ,address = _Address2
        ,prescriptions = PharmacyPrescriptions
    } = rpc(Config, get_pharmacy_by_id, [PharmId]),

    true = chk_presc_or_ref_in_list(PrescriptionObject, PharmacyPrescriptions)
            orelse lists:member({prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}, PharmacyPrescriptions),

    #staff{
        id = _BinPrescId
        ,name = _Name3
        ,address = _Address3
        ,speciality = _Speciality
        ,prescriptions = StaffPrescriptions
    } = rpc(Config, get_staff_by_id, [PrescId]),

    true = chk_presc_or_ref_in_list(PrescriptionObject, StaffPrescriptions)
            orelse lists:member({prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}, StaffPrescriptions).

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

    cmp_prescs([Id, PatId, PrescId, PharmId, DatePresc, DateProc, lists:append(Drugs, AdditionalDrugs),
                    ?PRESCRIPTION_PROCESSED_VALUE], [RemId, RemPatId, RemPrescriberId, RemPharmId, RemDatePresc,
                            RemDateProc, RemDrugs, RemIsProcessed]),

    %% check for same prescription inside patient, pharmacy and staff
    #patient{
        id = _BinPatId
        ,name = _Name1
        ,address = _Address1
        ,prescriptions = PatientPrescriptions
    } = rpc(Config, get_patient_by_id, [PatId]),

    true = chk_presc_or_ref_in_list(PrescriptionObject, PatientPrescriptions),

    #pharmacy{
        id = _BinPharmId
        ,name = _Name2
        ,address = _Address2
        ,prescriptions = PharmacyPrescriptions
    } = rpc(Config, get_pharmacy_by_id, [PharmId]),

    true = chk_presc_or_ref_in_list(PrescriptionObject, PharmacyPrescriptions),

    #staff{
        id = _BinPrescId
        ,name = _Name3
        ,address = _Address3
        ,speciality = _Speciality
        ,prescriptions = StaffPrescriptions
    } = rpc(Config, get_staff_by_id, [PrescId]),

    true = chk_presc_or_ref_in_list(PrescriptionObject, StaffPrescriptions).

cmp_prescs(#prescription{id = Id, patient_id = PatId, prescriber_id = PrescId, pharmacy_id = PharmId,
                         date_prescribed = DatePresc, date_processed = DateProc, drugs = Drugs,
                         is_processed = IsProcessed}, Second) ->
    cmp_prescs([Id, PatId, PrescId, PharmId, DatePresc, DateProc, Drugs, IsProcessed], Second);

cmp_prescs(First, #prescription{id = Id, patient_id = PatId, prescriber_id = PrescId, pharmacy_id = PharmId,
                                date_prescribed = DatePresc, date_processed = DateProc, drugs = Drugs,
                                is_processed = IsProcessed}) ->
    cmp_prescs(First, [Id, PatId, PrescId, PharmId, DatePresc, DateProc, Drugs, IsProcessed]);

cmp_prescs([Id, PatId, PrescId, PharmId, DatePresc, DateProc, Drugs, IsProcessed],
            [Id2, PatId2, PrescId2, PharmId2, DatePresc2, DateProc2, Drugs2, IsProcessed2]) ->
    true = Id =:= Id2 orelse list_to_binary(integer_to_list(Id)) =:= Id2,
    true = PatId =:= PatId2 orelse list_to_binary(integer_to_list(PatId)) =:= PatId2,
    true = PrescId =:= PrescId2 orelse list_to_binary(integer_to_list(PrescId)) =:= PrescId2,
    true = PharmId =:= PharmId2 orelse list_to_binary(integer_to_list(PharmId)) =:= PharmId2,
    true = DatePresc =:= DatePresc2 orelse list_to_binary(DatePresc) =:= DatePresc2,
    true = DateProc =:= DateProc2 orelse list_to_binary(DateProc) =:= DateProc2,
    true = IsProcessed =:= IsProcessed2,

    true = lists:sort(Drugs) =:= lists:sort(Drugs2) orelse
                lists:map(fun list_to_binary/1, lists:sort(Drugs)) =:= lists:sort(Drugs2).

chk_presc_or_ref_in_list(P, L) ->
    chk_presc_or_ref_in_list(P, L, false).

chk_presc_or_ref_in_list(_, [], Accum) ->
    Accum;
chk_presc_or_ref_in_list(P, [H|T], Accum) ->
    cmp_prescs_or_key(P, H) orelse chk_presc_or_ref_in_list(P, T, Accum).

cmp_prescs_or_key(P1, P2) ->
    case gen_key(prescription, presc_id(P1)) =:= P2 of
        true -> true;
        false -> cmp_prescs(P1, P2)
    end.

presc_id({prescription, Id, _, _, _, _, _, _, _}) when is_integer(Id) -> Id;
presc_id({prescription, Id, _, _, _, _, _, _, _}) when is_binary(Id) -> list_to_integer(binary_to_list(Id));
presc_id([Id, _, _, _, _, _, _, _]) when is_integer(Id) -> Id;
presc_id([Id, _, _, _, _, _, _, _]) when is_binary(Id) -> list_to_integer(binary_to_list(Id)).

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
    cmp_staff([Id, Name, Address, Speciality, []], [RemId, RemName, RemAddress, RemSpecial, RemPrescs]).

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
    cmp_staff([Id, Name, Address, Speciality, []], [RemId, RemName, RemAddress, RemSpecial, RemPrescs]).

cmp_staff([Id1, Name1, Address1, Speciality1, Prescs1], [Id2, Name2, Address2, Speciality2, Prescs2]) ->
    true = Id1 =:= Id2 orelse list_to_binary(integer_to_list(Id1)) =:= Id2,
    true = Name1 =:= Name2 orelse list_to_binary(Name1) =:= Name2,
    true = Address1 =:= Address2 orelse list_to_binary(Address1) =:= Address2,
    true = Speciality1 =:= Speciality2 orelse list_to_binary(Speciality1) =:= Speciality2,
    true = Prescs1 =:= Prescs2.


%%%-------------------------------------------------------------------
%%% treatment endpoint tests
%%%-------------------------------------------------------------------


treatment_unit_tests(_Config) ->
    % {skip, "Treatments not implemented in this version of FMKe."}.
    ok.

%%%-------------------------------------------------------------------
%%% status tests
%%%-------------------------------------------------------------------

status_tests(Config) ->
    {ok, TargetDatabase} = rpc(Config, application, get_env, [?APP, target_database]),
    {ok, Addresses} = rpc(Config, application, get_env, [?APP, database_addresses]),
    {ok, Ports} = rpc(Config, application, get_env, [?APP, database_ports]),
    {ok, ConnPoolSize} = rpc(Config, application, get_env, [?APP, connection_pool_size]),
    {ok, HttpPort} = rpc(Config, application, get_env, [?APP, http_port]),
    ok = application:set_env(?APP, connection_pool_size, ConnPoolSize),
    ok = application:set_env(?APP, target_database, TargetDatabase),
    ok = application:set_env(?APP, database_addresses, Addresses),
    ok = application:set_env(?APP, database_ports, Ports),
    ok = application:set_env(?APP, http_port, HttpPort),
    PropList = rpc(Config, get_status, []),
    true = ([] =/= PropList),
    check_status(PropList).

check_status([]) -> ok;
check_status([{fmke_up, true} | Other]) -> check_status(Other);
check_status([{connection_manager_up, _Status} | Other]) -> check_status(Other);
check_status([{web_server_up, true} | Other]) -> check_status(Other);
check_status([{connection_pool_size, PoolSize} | Other]) ->
    {ok, PoolSize} = application:get_env(?APP, connection_pool_size),
    check_status(Other);
check_status([{target_database, Target} | Other]) ->
    {ok, Target} = application:get_env(?APP, target_database),
    check_status(Other);
check_status([{database_addresses, BinAddresses} | Other]) ->
    {ok, Addresses} = application:get_env(?APP, database_addresses),
    ListAddresses = lists:map(fun binary_to_list/1, BinAddresses),
    true = (ListAddresses == Addresses),
    check_status(Other);
check_status([{database_ports, Ports} | Other]) ->
    {ok, Ports} = application:get_env(?APP, database_ports),
    check_status(Other);
check_status([{http_port, Port} | Other]) ->
    {ok, Port} = application:get_env(?APP, http_port),
    check_status(Other);
check_status([{pools, Pools} | Other]) ->
    lists:map(
        fun({_PoolName, PoolData}) ->
            true = ([] =/= PoolData),
            ok = check_pool_status(PoolData)
        end, Pools),
    check_status(Other).

check_pool_status([]) -> ok;
check_pool_status([{pool_is_up, true} | Other]) -> check_pool_status(Other);
check_pool_status([{pool_status, ready} | Other]) -> check_pool_status(Other);
check_pool_status([{current_overflow, 0} | Other]) -> check_pool_status(Other);
check_pool_status([{worker_pool_size, S} | Other]) ->
    {ok, S} = application:get_env(?APP, connection_pool_size),
    check_pool_status(Other).

%%%-------------------------------------------------------------------
%%% Auxiliary functions
%%%-------------------------------------------------------------------
rpc(Config, Mod, Fun, Args) ->
    Node = ?config(node, Config),
    rpc:block_call(Node, Mod, Fun, Args).

rpc(Config, Fun, Args) ->
    Node = ?config(node, Config),
    rpc:block_call(Node, fmke, Fun, Args).

gen_key(Entity,Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p",[Entity,Id]))).
