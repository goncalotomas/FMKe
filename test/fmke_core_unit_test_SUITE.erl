-module(fmke_core_unit_test_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("fmke.hrl").
-include("fmke_kv.hrl").

-define (NODENAME, 'fmke@127.0.0.1').
-define (COOKIE, fmke).

%%%-------------------------------------------------------------------
%%% Common Test exports
%%%-------------------------------------------------------------------
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
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
        event_unit_tests,
        facility_unit_tests,
        patient_unit_tests,
        pharmacy_unit_tests,
        prescription_unit_tests,
        staff_unit_tests,
        treatment_unit_tests,
        status_tests
    ].

%%%-------------------------------------------------------------------
%%% Common Test configuration
%%%-------------------------------------------------------------------

init_per_suite(Config) ->
    CTNodename = ct:get_config(ct_nodename, 'ct_core_suite@127.0.0.1'),
    FMKeNodename = ct:get_config(fmke_nodename, ?NODENAME),
    Options = [target_database, driver, database_addresses, database_ports,
                connection_pool_size, http_port, data_model],
    ConfigSrc = get_initial_config_src(),
    ConfigVals = get_initial_config(Config),
    OptionValues = lists:map(
        fun(Opt) ->
            case ConfigSrc of
                default ->
                    {Opt, ?config(Opt, ConfigVals)};
                external ->
                    {Opt, ct:get_config(Opt, ?DEFAULT(Opt))}
            end
        end, Options),
    ok = fmke_test_setup:ensure_start_dist_node(CTNodename),
    true = erlang:set_cookie(CTNodename, ?COOKIE),
    Node = fmke_test_setup:launch_fmke(FMKeNodename, OptionValues),
    [{node, Node} | Config].

end_per_suite(_Config) ->
    Nodename = ct:get_config(fmke_nodename, ?NODENAME),
    fmke_test_setup:stop_node(Nodename),
    fmke_test_setup:stop_all(),
    net_kernel:stop(),
    ok.

init_per_testcase(facility_unit_tests, Config) ->
    TabId = ets:new(facilities, [set, protected, named_table]),
    FacilityId = rand:uniform(1000000),
    ets:insert(TabId, {facility, FacilityId, "Some Hospital", "Somewhere", "Hospital"}),
    ets:insert(TabId, {updated_facility, FacilityId, "Some Random Hospital", "Beja, Portugal", "Treatment Facility"}),
    [{table, TabId} | Config];

init_per_testcase(patient_unit_tests, Config) ->
    TabId = ets:new(patients, [set, protected, named_table]),
    PatientId = rand:uniform(1000000),
    ets:insert(TabId, {patient, PatientId, "Goncalo Tomas", "Somewhere in Portugal"}),
    ets:insert(TabId, {updated_patient, PatientId, "Goncalo P. Tomas", "Caparica, Portugal"}),
    [{table, TabId} | Config];

init_per_testcase(pharmacy_unit_tests, Config) ->
    TabId = ets:new(pharmacies, [set, protected, named_table]),
    PharmacyId = rand:uniform(1000000),
    ets:insert(TabId, {pharmacy, PharmacyId, "Some Pharmacy", "Somewhere in Portugal"}),
    ets:insert(TabId, {updated_pharmacy, PharmacyId, "Some Random Pharmacy", "Caparica, Portugal"}),
    [{table, TabId} | Config];

init_per_testcase(prescription_unit_tests, Config) ->
    TabId = ets:new(prescriptions, [set, protected, named_table]),
    [R1, R2, R3, R4, R5] = lists:map(fun(_) -> rand:uniform(1000000) end, lists:seq(1, 5)),
    ets:insert(TabId, {patient, R1, "Goncalo Tomas", "Somewhere in Portugal"}),
    ets:insert(TabId, {other_patient, R1+1, "Goncalo P. Tomas", "Caparica, Portugal"}),
    ets:insert(TabId, {facility, R2, "Some Hospital", "Somewhere", "Hospital"}),
    ets:insert(TabId, {other_facility, R2+1, "Some Random Hospital", "Somewhere Portugal", "Treatment Facility"}),
    ets:insert(TabId, {pharmacy, R3, "Some Pharmacy", "Somewhere in Portugal"}),
    ets:insert(TabId, {other_pharmacy, R3+1, "Some Random Pharmacy", "Caparica, Portugal"}),
    ets:insert(TabId, {staff, R4, "Some Doctor", "Somewhere in Portugal", "Traditional Chinese Medicine"}),
    ets:insert(TabId, {other_staff, R4+1, "Some Random Doctor", "Caparica, Portugal", "weird esoteric medicine"}),
    ets:insert(TabId, {prescription, R5, R1, R4, R3, "2012-12-12", ["Penicillin", "Diazepam"]}),
    ets:insert(TabId, {updated_prescription_drugs, R5, ["Adrenaline"]}),
    ets:insert(TabId, {processed_prescription_date, R5, "2012-12-24"}),
    ets:insert(TabId, {other_prescription, R5+1, R1+1, R4+1, R3+1, "2015-01-10", ["Diazepam"]}),
    ets:insert(TabId, {other_updated_prescription_drugs, R5+1, ["Penicillin", "Adrenaline"]}),
    ets:insert(TabId, {other_processed_prescription_date, R5+1, "2016-01-01"}),
    [{table, TabId} | Config];

init_per_testcase(staff_unit_tests, Config) ->
    TabId = ets:new(staff, [set, protected, named_table]),
    StaffId = rand:uniform(1000000),
    ets:insert(TabId, {staff, StaffId, "Some Doctor", "Somewhere in Portugal", "Traditional Chinese Medicine"}),
    ets:insert(TabId, {updated_staff, StaffId, "Some Random Doctor", "Caparica, Portugal", "weird esoteric medicine"}),
    [{table, TabId} | Config];

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
    ExpectedFacility = #facility{id = Id, name = Name, address = Address, type = Type},
    RemFacility = rpc(Config, get_facility_by_id, [Id]),
    true = fmke_test_utils:compare_facilities(ExpectedFacility, RemFacility).

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
    ExpectedFacility = #facility{id = Id, name = Name, address = Address, type = Type},
    RemFacility = rpc(Config, get_facility_by_id, [Id]),
    true = fmke_test_utils:compare_facilities(ExpectedFacility, RemFacility).


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
    ExpectedPatient = #patient{id = Id, name = Name, address = Address, prescriptions = []},
    RemPatient = rpc(Config, get_patient_by_id, [Id]),
    true = fmke_test_utils:compare_patients(ExpectedPatient, RemPatient).

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
    UnusedId = Id+rand:uniform(1000000),
    {error, no_such_patient} = rpc(Config, update_patient_details, [UnusedId, Name, Address]).

get_patient_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_patient, Id, Name, Address}] = ets:lookup(TabId, updated_patient),
    ExpectedPatient = #patient{id = Id, name = Name, address = Address, prescriptions = []},
    RemPatient = rpc(Config, get_patient_by_id, [Id]),
    true = fmke_test_utils:compare_patients(ExpectedPatient, RemPatient).


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
    ExpectedPharmacy = #pharmacy{id = Id, name = Name, address = Address, prescriptions = []},
    RemPharmacy = rpc(Config, get_pharmacy_by_id, [Id]),
    true = fmke_test_utils:compare_pharmacies(ExpectedPharmacy, RemPharmacy).

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
    UnusedId = Id+rand:uniform(1000000),
    {error, no_such_pharmacy} = rpc(Config, update_pharmacy_details, [UnusedId, Name, Address]).

get_pharmacy_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_pharmacy, Id, Name, Address}] = ets:lookup(TabId, updated_pharmacy),
    ExpectedPharmacy = #pharmacy{id = Id, name = Name, address = Address, prescriptions = []},
    RemPharmacy = rpc(Config, get_pharmacy_by_id, [Id]),
    true = fmke_test_utils:compare_pharmacies(ExpectedPharmacy, RemPharmacy).


%%%-------------------------------------------------------------------
%%% prescription endpoint tests
%%%-------------------------------------------------------------------


prescription_unit_tests(Config) ->
    get_staff_prescriptions_from_unexistant_staff(Config),
    get_pharmacy_prescriptions_from_unexistant_pharmacy(Config),
    get_processed_pharmacy_prescriptions_from_unexistant_pharmacy(Config),
    add_required_entities(Config),
    get_staff_prescriptions_from_staff_with_no_prescriptions(Config),
    get_pharmacy_prescriptions_from_pharmacy_with_no_prescriptions(Config),
    get_processed_prescriptions_from_pharmacy_with_no_prescriptions(Config),
    get_unexisting_prescription(Config),
    process_unexisting_prescription(Config),
    add_medication_to_unexisting_prescription(Config),
    add_prescription_with_unexistant_patient(Config),
    add_prescription_with_unexistant_pharmacy(Config),
    add_prescription_with_unexistant_staff(Config),
    add_unexisting_prescription(Config),
    get_existing_prescription(Config),
    get_staff_prescriptions_from_staff_with_prescriptions(Config),
    get_pharmacy_prescriptions_from_pharmacy_with_prescriptions(Config),
    get_processed_prescriptions_from_pharmacy_with_prescriptions(Config),
    add_existing_prescription(Config),
    update_prescription_medication(Config),
    process_existing_non_processed_prescription(Config),
    add_medication_to_processed_prescription(Config),
    process_already_processed_prescription(Config),
    get_prescription_after_updates(Config),
    get_staff_prescriptions_from_staff_with_processed_prescriptions(Config),
    get_pharmacy_prescriptions_from_pharmacy_with_processed_prescriptions(Config),
    get_processed_prescriptions_from_pharmacy_with_processed_prescriptions(Config).

get_staff_prescriptions_from_unexistant_staff(Config) ->
    TabId = ?config(table, Config),
    [{staff, StaffId, _, _, _}] = ets:lookup(TabId, staff),
    {error, no_such_staff} = rpc(Config, get_staff_prescriptions, [StaffId]).

get_pharmacy_prescriptions_from_unexistant_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    {error, no_such_pharmacy} = rpc(Config, get_pharmacy_prescriptions, [PharmId]).

get_processed_pharmacy_prescriptions_from_unexistant_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    {error, no_such_pharmacy} = rpc(Config, get_processed_pharmacy_prescriptions, [PharmId]).

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

get_staff_prescriptions_from_staff_with_no_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{staff, StaffId, _, _, _}] = ets:lookup(TabId, staff),
    [] = rpc(Config, get_staff_prescriptions, [StaffId]).

get_pharmacy_prescriptions_from_pharmacy_with_no_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    [] = rpc(Config, get_pharmacy_prescriptions, [PharmId]).

get_processed_prescriptions_from_pharmacy_with_no_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    [] = rpc(Config, get_processed_pharmacy_prescriptions, [PharmId]).

get_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    {error, not_found} = rpc(Config, get_prescription_by_id, [Id]).

process_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    {error, no_such_prescription} = rpc(Config, process_prescription, [Id, "2017-08-14"]).

add_medication_to_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    {error, no_such_prescription} = rpc(Config, update_prescription_medication, [Id,
        add_drugs, ["RandomDrug1", "RandomDrug2", "RandomDrug3"]]).

add_prescription_with_unexistant_patient(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    {error, no_such_patient} = rpc(Config, create_prescription, [Id, PatId*2-1, PrescId, PharmId, DatePresc, Drugs]).

add_prescription_with_unexistant_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    {error, no_such_pharmacy} = rpc(Config, create_prescription, [Id, PatId, PrescId, PharmId*2-1, DatePresc, Drugs]).

add_prescription_with_unexistant_staff(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    {error, no_such_staff} = rpc(Config, create_prescription, [Id, PatId, PrescId*2-1, PharmId, DatePresc, Drugs]).

add_unexisting_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    ok = rpc(Config, create_prescription, [Id, PatId, PrescId, PharmId, DatePresc, Drugs]).

get_existing_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),

    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = <<"undefined">>, drugs = Drugs,
                                         is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE},

    RemPrescription = rpc(Config, get_prescription_by_id, [Id]),

    true = fmke_test_utils:compare_prescriptions(ExpectedPrescription, RemPrescription),

    %% check for same prescription inside patient, pharmacy and staff
    #patient{
        id = _BinPatId,
        name = _Name1,
        address = _Address1,
        prescriptions = PatientPrescriptions
    } = rpc(Config, get_patient_by_id, [PatId]),

    true = fmke_test_utils:search_prescription(ExpectedPrescription, PatientPrescriptions),

    #pharmacy{
        id = _BinPharmId,
        name = _Name2,
        address = _Address2,
        prescriptions = PharmacyPrescriptions
    } = rpc(Config, get_pharmacy_by_id, [PharmId]),

    true = fmke_test_utils:search_prescription(ExpectedPrescription, PharmacyPrescriptions),

    #staff{
        id = _BinPrescId,
        name = _Name3,
        address = _Address3,
        speciality = _Speciality,
        prescriptions = StaffPrescriptions
    } = rpc(Config, get_staff_by_id, [PrescId]),

    true = fmke_test_utils:search_prescription(ExpectedPrescription, StaffPrescriptions).

get_staff_prescriptions_from_staff_with_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{staff, StaffId, _, _, _}] = ets:lookup(TabId, staff),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = <<"undefined">>, drugs = Drugs,
                                         is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE},
    [Prescription] = rpc(Config, get_staff_prescriptions, [StaffId]),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, [Prescription]).

get_pharmacy_prescriptions_from_pharmacy_with_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = <<"undefined">>, drugs = Drugs,
                                         is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE},
    [Prescription] = rpc(Config, get_pharmacy_prescriptions, [PharmId]),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, [Prescription]).

get_processed_prescriptions_from_pharmacy_with_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    [] = rpc(Config, get_processed_pharmacy_prescriptions, [PharmId]).

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

    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = DateProc, drugs = lists:append(Drugs, AdditionalDrugs),
                                         is_processed = ?PRESCRIPTION_PROCESSED_VALUE},

    RemPrescription = rpc(Config, get_prescription_by_id, [Id]),

    true = fmke_test_utils:compare_prescriptions(ExpectedPrescription, RemPrescription),

    %% check for same prescription inside patient, pharmacy and staff
    #patient{
        id = _BinPatId,
        name = _Name1,
        address = _Address1,
        prescriptions = PatientPrescriptions
    } = rpc(Config, get_patient_by_id, [PatId]),

    true = fmke_test_utils:search_prescription(ExpectedPrescription, PatientPrescriptions),

    #pharmacy{
        id = _BinPharmId,
        name = _Name2,
        address = _Address2,
        prescriptions = PharmacyPrescriptions
    } = rpc(Config, get_pharmacy_by_id, [PharmId]),

    true = fmke_test_utils:search_prescription(ExpectedPrescription, PharmacyPrescriptions),

    #staff{
        id = _BinPrescId,
        name = _Name3,
        address = _Address3,
        speciality = _Speciality,
        prescriptions = StaffPrescriptions
    } = rpc(Config, get_staff_by_id, [PrescId]),

    true = fmke_test_utils:search_prescription(ExpectedPrescription, StaffPrescriptions).

get_staff_prescriptions_from_staff_with_processed_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{staff, StaffId, _, _, _}] = ets:lookup(TabId, staff),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    [{processed_prescription_date, Id, DateProc}] = ets:lookup(TabId, processed_prescription_date),
    [{updated_prescription_drugs, Id, AdditionalDrugs}] = ets:lookup(TabId, updated_prescription_drugs),
    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = DateProc, drugs = lists:append(Drugs, AdditionalDrugs),
                                         is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    [Prescription] = rpc(Config, get_staff_prescriptions, [StaffId]),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, [Prescription]).

get_pharmacy_prescriptions_from_pharmacy_with_processed_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    [{processed_prescription_date, Id, DateProc}] = ets:lookup(TabId, processed_prescription_date),
    [{updated_prescription_drugs, Id, AdditionalDrugs}] = ets:lookup(TabId, updated_prescription_drugs),
    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = DateProc, drugs = lists:append(Drugs, AdditionalDrugs),
                                         is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    [Prescription] = rpc(Config, get_pharmacy_prescriptions, [PharmId]),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, [Prescription]).

get_processed_prescriptions_from_pharmacy_with_processed_prescriptions(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmId, _, _}] = ets:lookup(TabId, pharmacy),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    [{processed_prescription_date, Id, DateProc}] = ets:lookup(TabId, processed_prescription_date),
    [{updated_prescription_drugs, Id, AdditionalDrugs}] = ets:lookup(TabId, updated_prescription_drugs),
    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, pharmacy_id = PharmId,
                                         prescriber_id = PrescId, date_prescribed = DatePresc,
                                         date_processed = DateProc, drugs = lists:append(Drugs, AdditionalDrugs),
                                         is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    [Prescription] = rpc(Config, get_processed_pharmacy_prescriptions, [PharmId]),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, [Prescription]).

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
    ExpectedStaff = #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = []},
    RemStaff = rpc(Config, get_staff_by_id, [Id]),
    true = fmke_test_utils:compare_staff(ExpectedStaff, RemStaff).

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
    UnusedId = Id+rand:uniform(1000000),
    {error, no_such_staff} = rpc(Config, update_staff_details, [UnusedId, Name, Address, Speciality]).

get_staff_after_update(Config) ->
    TabId = ?config(table, Config),
    [{updated_staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, updated_staff),
    ExpectedStaff = #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = []},
    RemStaff = rpc(Config, get_staff_by_id, [Id]),
    true = fmke_test_utils:compare_staff(ExpectedStaff, RemStaff).


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
    ExpectedVals = lists:map(
        fun(Prop) ->
            case rpc(Config, application, get_env, [?APP, Prop]) of
                {ok, Val} ->
                    {Prop, Val};
                undefined ->
                    {Prop, undefined}
            end
        end, [target_database, driver, database_addresses, database_ports, connection_pool_size, http_port, pools]),
    PropList = rpc(Config, get_status, []),
    true = ([] =/= PropList),
    %% remove all options that we aren't checking for
    FilteredProps = lists:filter(fun({Opt, _Val}) -> proplists:is_defined(Opt, ExpectedVals) end, PropList),
    check_status(lists:sort(ExpectedVals), lists:sort(FilteredProps)).

check_status([], []) ->
    ok;
check_status([A | T1], [A | T2]) ->
    check_status(T1, T2).

%%%-------------------------------------------------------------------
%%% Auxiliary functions
%%%-------------------------------------------------------------------
rpc(Config, Mod, Fun, Args) ->
    Node = ?config(node, Config),
    rpc:block_call(Node, Mod, Fun, Args).

rpc(Config, Fun, Args) ->
    Node = ?config(node, Config),
    rpc:block_call(Node, fmke, Fun, Args).

get_initial_config_src() ->
    Driver = ct:get_config(driver, undefined),
    Database = ct:get_config(target_database, undefined),
    case valid_config(Driver, Database) of
        false -> default;
        true -> external
    end.

get_initial_config(Config) ->
    Driver = ct:get_config(driver, undefined),
    Database = ct:get_config(target_database, undefined),
    case valid_config(Driver, Database) of
        false ->
            default_config(Config);
        true ->
            Config
    end.

valid_config(undefined, undefined) ->
    false;
valid_config(_, _) ->
    %% we actually don't know if this is a valid config
    %% but it should be enough to boot a valid config of FMKe
    true.

default_config(Config) ->
    DataDir = ?config(data_dir, Config),
    Filepath = DataDir ++ "default.config",
    {ok, FileConfig} = file:consult(Filepath),
    FileConfig.
