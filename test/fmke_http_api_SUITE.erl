-module(fmke_http_api_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("fmke.hrl").
-include("fmke_kv.hrl").
-include("fmke_http.hrl").

-define(NODENAME, 'fmke@127.0.0.1').
-define(COOKIE, fmke).

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
    event_http_tests/1,
    facility_http_tests/1,
    patient_http_tests/1,
    pharmacy_http_tests/1,
    prescription_http_tests/1,
    staff_http_tests/1,
    treatment_http_tests/1,
    status_http_tests/1
]).

%%%-------------------------------------------------------------------
%%% Common Test Callbacks
%%%-------------------------------------------------------------------

suite() ->
    [{timetrap, {minutes, 3}}].

%% returns a list of all test sets to be executed by Common Test.
all() ->
    [event_http_tests, facility_http_tests, patient_http_tests, pharmacy_http_tests,
     prescription_http_tests, staff_http_tests, treatment_http_tests, status_http_tests].

%%%-------------------------------------------------------------------
%%% Common Test configuration
%%%-------------------------------------------------------------------

init_per_suite(Config) ->
    CTNodename = ct:get_config(ct_nodename, 'ct_http_suite@127.0.0.1'),
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
    HttpPort = proplists:get_value(http_port, OptionValues),
    [{node, Node}, {http_port, HttpPort} | Config].

end_per_suite(_Config) ->
    Nodename = ct:get_config(fmke_nodename, ?NODENAME),
    fmke_test_setup:stop_node(Nodename),
    fmke_test_setup:stop_all(),
    net_kernel:stop(),
    ok.

init_per_testcase(facility_http_tests, Config) ->
    TabId = ets:new(facilities, [set, protected, named_table]),
    FacilityId = rand:uniform(1000000),
    ets:insert(TabId, {facility, FacilityId, "Some Hospital", "Somewhere", "Hospital"}),
    ets:insert(TabId, {updated_facility, FacilityId, "Some Random Hospital", "Beja, Portugal", "Treatment Facility"}),
    [{table, TabId} | Config];

init_per_testcase(patient_http_tests, Config) ->
    TabId = ets:new(patients, [set, protected, named_table]),
    PatientId = rand:uniform(1000000),
    ets:insert(TabId, {patient, PatientId, "Goncalo Tomas", "Somewhere in Portugal"}),
    ets:insert(TabId, {updated_patient, PatientId, "Goncalo P. Tomas", "Caparica, Portugal"}),
    [{table, TabId} | Config];

init_per_testcase(pharmacy_http_tests, Config) ->
    TabId = ets:new(pharmacies, [set, protected, named_table]),
    PharmacyId = rand:uniform(1000000),
    ets:insert(TabId, {pharmacy, PharmacyId, "Some Pharmacy", "Somewhere in Portugal"}),
    ets:insert(TabId, {updated_pharmacy, PharmacyId, "Some Random Pharmacy", "Caparica, Portugal"}),
    [{table, TabId} | Config];

init_per_testcase(prescription_http_tests, Config) ->
    TabId = ets:new(prescriptions, [set, protected, named_table]),
    ets:insert(TabId, {patient, 1, "Goncalo Tomas", "Somewhere in Portugal"}),
    ets:insert(TabId, {other_patient, 2, "Goncalo P. Tomas", "Caparica, Portugal"}),
    ets:insert(TabId, {facility, 1, "Some Hospital", "Somewhere", "Hospital"}),
    ets:insert(TabId, {other_facility, 2, "Some Random Hospital", "Somewhere Portugal", "Treatment Facility"}),
    ets:insert(TabId, {pharmacy, 1, "Some Pharmacy", "Somewhere in Portugal"}),
    ets:insert(TabId, {other_pharmacy, 2, "Some Random Pharmacy", "Caparica, Portugal"}),
    ets:insert(TabId, {staff, 1, "Some Doctor", "Somewhere in Portugal", "Traditional Chinese Medicine"}),
    ets:insert(TabId, {other_staff, 2, "Some Random Doctor", "Caparica, Portugal", "weird esoteric kind of medicine"}),
    PrescriptionId = rand:uniform(1000000),
    ets:insert(TabId, {prescription, PrescriptionId, 1, 1, 1, "2012-12-12", "Penicillin, Diazepam"}),
    ets:insert(TabId, {updated_prescription_drugs, PrescriptionId, "Adrenaline"}),
    ets:insert(TabId, {processed_prescription_date, PrescriptionId, "2012-12-24"}),
    ets:insert(TabId, {other_prescription, PrescriptionId+1, 2, 2, 2, "2015-10-01", "Diazepam"}),
    ets:insert(TabId, {other_updated_prescription_drugs, PrescriptionId+1, "Penicillin, Adrenaline"}),
    ets:insert(TabId, {other_processed_prescription_date, PrescriptionId+1, "2016-01-01"}),
    [{table, TabId} | Config];

init_per_testcase(staff_http_tests, Config) ->
    TabId = ets:new(staff, [set, protected, named_table]),
    StaffId = rand:uniform(1000000),
    ets:insert(TabId, {staff, StaffId, "Some Doctor", "Somewhere in Portugal", "Traditional Chinese Medicine"}),
    ets:insert(TabId, {updated_staff, StaffId, "Some Random Doctor", "Caparica, Portugal", "weird esoteric medicine"}),
    [{table, TabId} | Config];

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(facility_http_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(patient_http_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(pharmacy_http_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(prescription_http_tests, Config) ->
    ets:delete(?config(table, Config));
end_per_testcase(staff_http_tests, Config) ->
    ets:delete(?config(table, Config));

end_per_testcase(_, Config) ->
    Config.

event_http_tests(_Config) ->
    % {skip, "Events not implemented in this version of FMKe."}.
    ok.


%%%-------------------------------------------------------------------
%%% facility endpoint tests
%%%-------------------------------------------------------------------


facility_http_tests(Config) ->
    facility_handler_rejects_empty_post_request(Config),
    facility_handler_rejects_empty_put_request(Config),
    create_facility(Config),
    update_facility(Config),
    read_facility(Config).

facility_handler_rejects_empty_post_request(Config) ->
    PropList = bad_req_http_post(Config, "/facilities", <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

facility_handler_rejects_empty_put_request(Config) ->
    TabId = ?config(table, Config),
    [{facility, FacilityId, _, _, _}] = ets:lookup(TabId, facility),
    PropList = bad_req_http_put(Config, "/facilities/" ++ integer_to_list(FacilityId), <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

create_facility(Config) ->
    TabId = ?config(table, Config),
    [{facility, Id, Name, Address, Type}] = ets:lookup(TabId, facility),
    FacilityProps = build_facility_props([Id, Name, Address, Type]),
    ResponseJson = http_post(Config, "/facilities", FacilityProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

update_facility(Config) ->
    TabId = ?config(table, Config),
    [{updated_facility, Id, Name, Address, Type}] = ets:lookup(TabId, updated_facility),
    FacilityProps = [{name, Name}, {address, Address}, {type, Type}],
    ResponseJson = http_put(Config, "/facilities/" ++ integer_to_list(Id), FacilityProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

read_facility(Config) ->
    TabId = ?config(table, Config),
    [{updated_facility, Id, Name, Address, Type}] = ets:lookup(TabId, updated_facility),
    PropListJson = http_get(Config, "/facilities/" ++ integer_to_list(Id)),
    true = proplists:get_value(<<"success">>, PropListJson),
    FacilityObject = proplists:get_value(<<"result">>, PropListJson),
    JsonId = proplists:get_value(<<"facilityId">>, FacilityObject),
    JsonName = proplists:get_value(<<"facilityName">>, FacilityObject),
    JsonAddress = proplists:get_value(<<"facilityAddress">>, FacilityObject),
    JsonType = proplists:get_value(<<"facilityType">>, FacilityObject),
    ExpectedFacility = #facility{id = Id, name = Name, address = Address, type = Type},
    JsonFacility = #facility{id = JsonId, name = JsonName, address = JsonAddress, type = JsonType},
    true = fmke_test_utils:compare_facilities(ExpectedFacility, JsonFacility).


%%%-------------------------------------------------------------------
%%% patient endpoint tests
%%%-------------------------------------------------------------------


patient_http_tests(Config) ->
    patient_handler_rejects_empty_post_request(Config),
    patient_handler_rejects_empty_put_request(Config),
    create_patient(Config),
    update_patient(Config),
    read_patient(Config).

patient_handler_rejects_empty_post_request(Config) ->
    PropList = bad_req_http_post(Config, "/patients", <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

patient_handler_rejects_empty_put_request(Config) ->
    TabId = ?config(table, Config),
    [{patient, PatientId, _, _}] = ets:lookup(TabId, patient),
    PropList = bad_req_http_put(Config, "/patients/" ++ integer_to_list(PatientId), <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

create_patient(Config) ->
    TabId = ?config(table, Config),
    [{patient, Id, Name, Address}] = ets:lookup(TabId, patient),
    PatientProps = [{id, Id}, {name, Name}, {address, Address}],
    ResponseJson = http_post(Config, "/patients", PatientProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

update_patient(Config) ->
    TabId = ?config(table, Config),
    [{updated_patient, Id, Name, Address}] = ets:lookup(TabId, updated_patient),
    PatientProps = [{name, Name}, {address, Address}],
    ResponseJson = http_put(Config, "/patients/" ++ integer_to_list(Id), PatientProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

read_patient(Config) ->
    TabId = ?config(table, Config),
    [{updated_patient, Id, Name, Address}] = ets:lookup(TabId, updated_patient),
    PropListJson = http_get(Config, "/patients/" ++ integer_to_list(Id)),
    true = proplists:get_value(<<"success">>, PropListJson),
    PatientObject = proplists:get_value(<<"result">>, PropListJson),
    JsonId = proplists:get_value(<<"patientId">>, PatientObject),
    JsonName = proplists:get_value(<<"patientName">>, PatientObject),
    JsonAddress = proplists:get_value(<<"patientAddress">>, PatientObject),
    ExpectedPatient = #patient{id = Id, name = Name, address = Address},
    JsonPatient = #patient{id = JsonId, name = JsonName, address = JsonAddress},
    true = fmke_test_utils:compare_patients(ExpectedPatient, JsonPatient).


%%%-------------------------------------------------------------------
%%% pharmacy endpoint tests
%%%-------------------------------------------------------------------


pharmacy_http_tests(Config) ->
    pharmacy_handler_rejects_empty_post_request(Config),
    pharmacy_handler_rejects_empty_put_request(Config),
    create_pharmacy(Config),
    update_pharmacy(Config),
    read_pharmacy(Config).

pharmacy_handler_rejects_empty_post_request(Config) ->
    PropList = bad_req_http_post(Config, "/pharmacies", <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

pharmacy_handler_rejects_empty_put_request(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, PharmacyId, _, _}] = ets:lookup(TabId, pharmacy),
    PropList = bad_req_http_put(Config, "/pharmacies/" ++ integer_to_list(PharmacyId), <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

create_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{pharmacy, Id, Name, Address}] = ets:lookup(TabId, pharmacy),
    PharmacyProps = [{id, Id}, {name, Name}, {address, Address}],
    ResponseJson = http_post(Config, "/pharmacies", PharmacyProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

update_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{updated_pharmacy, Id, Name, Address}] = ets:lookup(TabId, updated_pharmacy),
    PharmacyProps = [{name, Name}, {address, Address}],
    ResponseJson = http_put(Config, "/pharmacies/" ++ integer_to_list(Id), PharmacyProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

read_pharmacy(Config) ->
    TabId = ?config(table, Config),
    [{updated_pharmacy, Id, Name, Address}] = ets:lookup(TabId, updated_pharmacy),
    PropListJson = http_get(Config, "/pharmacies/" ++ integer_to_list(Id)),
    true = proplists:get_value(<<"success">>, PropListJson),
    PharmacyObject = proplists:get_value(<<"result">>, PropListJson),
    JsonId = proplists:get_value(<<"pharmacyId">>, PharmacyObject),
    JsonName = proplists:get_value(<<"pharmacyName">>, PharmacyObject),
    JsonAddress = proplists:get_value(<<"pharmacyAddress">>, PharmacyObject),
    ExpectedPharmacy = #pharmacy{id = Id, name = Name, address = Address},
    JsonPharmacy = #pharmacy{id = JsonId, name = JsonName, address = JsonAddress},
    true = fmke_test_utils:compare_pharmacies(ExpectedPharmacy, JsonPharmacy).


%%%-------------------------------------------------------------------
%%% prescription endpoint tests
%%%-------------------------------------------------------------------


prescription_http_tests(Config) ->
    TabId = ?config(table, Config),
    [{facility, FacId1, FacName1, FacAddr1, FacType1}] = ets:lookup(TabId, facility),
    [{other_facility, FacId2, FacName2, FacAddr2, FacType2}] = ets:lookup(TabId, other_facility),
    [{patient, PatId1, PatName1, PatAddr1}] = ets:lookup(TabId, patient),
    [{other_patient, PatId2, PatName2, PatAddr2}] = ets:lookup(TabId, other_patient),
    [{pharmacy, PharmId1, PharmName1, PharmAddress1}] = ets:lookup(TabId, pharmacy),
    [{other_pharmacy, PharmId2, PharmName2, PharmAddress2}] = ets:lookup(TabId, other_pharmacy),
    [{staff, StaId1, StaName1, StaAddr1, StaSpec1}] = ets:lookup(TabId, staff),
    [{other_staff, StaId2, StaName2, StaAddr2, StaSpec2}] = ets:lookup(TabId, other_staff),

    FacilityProps1 = build_facility_props([FacId1, FacName1, FacAddr1, FacType1]),
    FacilityProps2 = build_facility_props([FacId2, FacName2, FacAddr2, FacType2]),
    PatientProps1 = build_patient_props([PatId1, PatName1, PatAddr1]),
    PatientProps2 = build_patient_props([PatId2, PatName2, PatAddr2]),
    PharmacyProps1 = build_pharmacy_props([PharmId1, PharmName1, PharmAddress1]),
    PharmacyProps2 = build_pharmacy_props([PharmId2, PharmName2, PharmAddress2]),
    StaffProps1 = build_staff_props([StaId1, StaName1, StaAddr1, StaSpec1]),
    StaffProps2 = build_staff_props([StaId2, StaName2, StaAddr2, StaSpec2]),

    successful_http_post(Config, "/facilities/" ++ integer_to_list(FacId1), FacilityProps1),
    successful_http_post(Config, "/facilities/" ++ integer_to_list(FacId2), FacilityProps2),
    successful_http_post(Config, "/patients/" ++ integer_to_list(PatId1), PatientProps1),
    successful_http_post(Config, "/patients/" ++ integer_to_list(PatId2), PatientProps2),
    successful_http_post(Config, "/pharmacies/" ++ integer_to_list(PharmId1), PharmacyProps1),
    successful_http_post(Config, "/pharmacies/" ++ integer_to_list(PharmId2), PharmacyProps2),
    successful_http_post(Config, "/staff/" ++ integer_to_list(StaId1), StaffProps1),
    successful_http_post(Config, "/staff/" ++ integer_to_list(StaId2), StaffProps2),

    prescription_handler_rejects_empty_post_request(Config),
    prescription_handler_rejects_empty_put_request(Config),
    create_prescription(Config),
    update_prescription_medication(Config),
    process_prescription(Config),
    read_prescription(Config).

prescription_handler_rejects_empty_post_request(Config) ->
    PropList = bad_req_http_post(Config, "/prescriptions", <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

prescription_handler_rejects_empty_put_request(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, _, _, _, _, _}] = ets:lookup(TabId, prescription),
    PropList = bad_req_http_put(Config, "/prescriptions/" ++ integer_to_list(Id), <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

create_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    PrescriptionProps = build_prescription_props([Id, PatId, PrescId, PharmId, DatePresc, Drugs]),
    ResponseJson = http_post(Config, "/prescriptions", PrescriptionProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

update_prescription_medication(Config) ->
    TabId = ?config(table, Config),
    [{updated_prescription_drugs, Id, Drugs}] = ets:lookup(TabId, updated_prescription_drugs),
    PrescriptionProps = [{drugs, Drugs}],
    ResponseJson = http_put(Config, "/prescriptions/" ++ integer_to_list(Id), PrescriptionProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

process_prescription(Config) ->
    TabId = ?config(table, Config),
    [{processed_prescription_date, Id, Date}] = ets:lookup(TabId, processed_prescription_date),
    PrescriptionProps = [{date_processed, Date}],
    ResponseJson = http_put(Config, "/prescriptions/" ++ integer_to_list(Id), PrescriptionProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

read_prescription(Config) ->
    TabId = ?config(table, Config),
    [{prescription, Id, PatId, PrescId, PharmId, DatePresc, Drugs}] = ets:lookup(TabId, prescription),
    [{processed_prescription_date, Id, DateProc}] = ets:lookup(TabId, processed_prescription_date),
    [{updated_prescription_drugs, Id, AdditionalDrugs}] = ets:lookup(TabId, updated_prescription_drugs),
    ExpectedDrugs = lists:append(fmke_http_utils:parse_csv_string(Drugs),
                                fmke_http_utils:parse_csv_string(AdditionalDrugs)),
    PropListJson = http_get(Config, "/prescriptions/" ++ integer_to_list(Id)),
    true = proplists:get_value(<<"success">>, PropListJson),
    PrescriptionObject = proplists:get_value(<<"result">>, PropListJson),

    JsonId = proplists:get_value(<<"prescriptionId">>, PrescriptionObject),
    JsonPatId = proplists:get_value(<<"prescriptionPatientId">>, PrescriptionObject),
    JsonPrescId = proplists:get_value(<<"prescriptionPrescriberId">>, PrescriptionObject),
    JsonPharmId = proplists:get_value(<<"prescriptionPharmacyId">>, PrescriptionObject),
    JsonDatePresc = proplists:get_value(<<"prescriptionDatePrescribed">>, PrescriptionObject),
    JsonDateProc = proplists:get_value(<<"prescriptionDateProcessed">>, PrescriptionObject),
    JsonIsProcessed = proplists:get_value(<<"prescriptionIsProcessed">>, PrescriptionObject),
    JsonDrugs = lists:sort(proplists:get_value(<<"prescriptionDrugs">>, PrescriptionObject)),

    ExpectedPrescription = #prescription{id = Id, patient_id = PatId, prescriber_id = PrescId, pharmacy_id = PharmId,
                                         date_prescribed = DatePresc, date_processed = DateProc,
                                         drugs = ExpectedDrugs, is_processed = ?PRESCRIPTION_PROCESSED_VALUE},

    JsonPrescription = #prescription{id = JsonId, patient_id = JsonPatId, prescriber_id = JsonPrescId,
                                     pharmacy_id = JsonPharmId, date_prescribed = JsonDatePresc,
                                     date_processed = JsonDateProc, drugs = JsonDrugs,
                                     is_processed = JsonIsProcessed},

    true = fmke_test_utils:compare_prescriptions(ExpectedPrescription, JsonPrescription),

    %% check for same prescription inside patient, pharmacy and staff
    PatientReqResult = http_get(Config, "/patients/" ++ integer_to_list(PatId)),
    true = proplists:get_value(<<"success">>, PatientReqResult),
    PatientObject = proplists:get_value(<<"result">>, PatientReqResult),
    PatientPrescriptions = lists:map(fun(P) -> fmke_json:decode(prescription, P) end,
        proplists:get_value(<<"patientPrescriptions">>, PatientObject)),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, PatientPrescriptions),

    PharmacyReqResult = http_get(Config, "/pharmacies/" ++ integer_to_list(PharmId)),
    true = proplists:get_value(<<"success">>, PharmacyReqResult),
    PharmacyObject = proplists:get_value(<<"result">>, PharmacyReqResult),
    PharmacyPrescriptions = lists:map(fun(P) -> fmke_json:decode(prescription, P) end,
        proplists:get_value(<<"pharmacyPrescriptions">>, PharmacyObject)),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, PharmacyPrescriptions),

    StaffReqResult = http_get(Config, "/staff/" ++ integer_to_list(PrescId)),
    true = proplists:get_value(<<"success">>, StaffReqResult),
    StaffObject = proplists:get_value(<<"result">>, StaffReqResult),
    StaffPrescriptions = lists:map(fun(P) -> fmke_json:decode(prescription, P) end,
        proplists:get_value(<<"staffPrescriptions">>, StaffObject)),
    true = fmke_test_utils:search_prescription(ExpectedPrescription, StaffPrescriptions).


%%%-------------------------------------------------------------------
%%% staff endpoint tests
%%%-------------------------------------------------------------------


staff_http_tests(Config) ->
    staff_handler_rejects_empty_post_request(Config),
    staff_handler_rejects_empty_put_request(Config),
    create_staff(Config),
    update_staff(Config),
    read_staff(Config).

staff_handler_rejects_empty_post_request(Config) ->
    PropList = bad_req_http_post(Config, "/staff", <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

staff_handler_rejects_empty_put_request(Config) ->
    TabId = ?config(table, Config),
    [{staff, StaffId, _, _, _}] = ets:lookup(TabId, staff),
    PropList = bad_req_http_put(Config, "/staff/" ++ integer_to_list(StaffId), <<>>),
    false = proplists:get_value(<<"success">>, PropList),
    ?ERR_MISSING_BODY = proplists:get_value(<<"result">>, PropList).

create_staff(Config) ->
    TabId = ?config(table, Config),
    [{staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, staff),
    StaffProps = build_staff_props([Id, Name, Address, Speciality]),
    ResponseJson = http_post(Config, "/staff", StaffProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

update_staff(Config) ->
    TabId = ?config(table, Config),
    [{updated_staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, updated_staff),
    StaffProps = [{name, Name}, {address, Address}, {speciality, Speciality}],
    ResponseJson = http_put(Config, "/staff/" ++ integer_to_list(Id), StaffProps),
    true = proplists:get_value(<<"success">>, ResponseJson),
    <<"ok">> = proplists:get_value(<<"result">>, ResponseJson).

read_staff(Config) ->
    TabId = ?config(table, Config),
    [{updated_staff, Id, Name, Address, Speciality}] = ets:lookup(TabId, updated_staff),
    PropListJson = http_get(Config, "/staff/" ++ integer_to_list(Id)),
    true = proplists:get_value(<<"success">>, PropListJson),
    StaffObject = proplists:get_value(<<"result">>, PropListJson),
    JsonId = proplists:get_value(<<"staffId">>, StaffObject),
    JsonName = proplists:get_value(<<"staffName">>, StaffObject),
    JsonAddress = proplists:get_value(<<"staffAddress">>, StaffObject),
    JsonSpeciality = proplists:get_value(<<"staffSpeciality">>, StaffObject),
    ExpectedStaff = #staff{id = Id, name = Name, address = Address, speciality = Speciality},
    JsonStaff = #staff{id = JsonId, name = JsonName, address = JsonAddress, speciality = JsonSpeciality},
    true = fmke_test_utils:compare_staff(ExpectedStaff, JsonStaff).


%%%-------------------------------------------------------------------
%%% treatment endpoint tests
%%%-------------------------------------------------------------------


treatment_http_tests(_Config) ->
    % {skip, "Treatments not implemented in this version of FMKe."}.
    ok.

%%%-------------------------------------------------------------------
%%% status tests
%%%-------------------------------------------------------------------

status_http_tests(Config) ->
    ExpectedVals = lists:map(
        fun(Prop) ->
            case rpc(Config, application, get_env, [?APP, Prop]) of
                {ok, Val} ->
                    {list_to_binary(atom_to_list(Prop)), maybe_bin(Val)};
                undefined ->
                    {list_to_binary(atom_to_list(Prop)), <<"undefined">>}
            end
        end, [target_database, driver, database_addresses, database_ports, connection_pool_size, pools]),
    PropList = proplists:get_value(<<"result">>, http_get(Config, "/")),
    true = ([] =/= PropList),
    %% remove all options that we aren't checking for
    FilteredProps = lists:filter(
        fun({Opt, _Val}) ->
            proplists:is_defined(Opt, ExpectedVals)
        end, PropList),
    check_status(lists:sort(ExpectedVals), lists:sort(FilteredProps)).

check_status([], []) ->
    ok;
check_status([{O, V} | T1], [{O, [V]} | T2]) ->
    check_status(T1, T2);
check_status([O | T1], [O | T2]) ->
    check_status(T1, T2).

maybe_bin([]) -> [];
maybe_bin([E]) when is_integer(E) -> [E];
maybe_bin([E]) when is_atom(E) -> [list_to_binary(atom_to_list(E))];
maybe_bin(E) when is_integer(E) -> E;
maybe_bin(E) when is_list(E) -> list_to_binary(E);
maybe_bin(E) when is_boolean(E) -> E;
maybe_bin(E) when is_atom(E) -> list_to_binary(atom_to_list(E)).

%%%-------------------------------------------------------------------
%%% Auxiliary functions
%%%-------------------------------------------------------------------

successful_http_post(Config, Url, Data)->
    JsonResponse = http_post(Config, Url, Data),
    true = proplists:get_value(<<"success">>, JsonResponse).

http_get(Config, Url) ->
    Port = integer_to_list(?config(http_port, Config)),
    FullUrl = "http://localhost:" ++ Port ++ Url,
    Headers = [],
    HttpOptions = [],
    Options = [{sync, true}],
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {FullUrl, Headers}, HttpOptions, Options),
    jsx:decode(list_to_binary(Body)).

http_post(Config, Url, Data) ->
    http_req_w_body(Config, post, Url, Data).

bad_req_http_post(Config, Url, Data) ->
    http_req_w_body(Config, post, Url, Data, 400).

http_put(Config, Url, Data) ->
    http_req_w_body(Config, put, Url, Data).

bad_req_http_put(Config, Url, Data) ->
    http_req_w_body(Config, put, Url, Data, 400).

http_req_w_body(Config, Method, Url, Data) ->
    http_req_w_body(Config, Method, Url, Data, 200).

http_req_w_body(Config, Method, Url, Data, ExpectedReturn) ->
    Port = integer_to_list(?config(http_port, Config)),
    FullUrl = "http://localhost:" ++ Port ++ Url,
    Headers = [],
    HttpOptions = [],
    Options = [{sync, true}],
    Json = jsx:encode(Data),
    RequestDescription = {FullUrl, Headers, "application/json", Json},
    {ok, {{_, ExpectedReturn, _}, _, Body}} = httpc:request(Method, RequestDescription, HttpOptions, Options),
    jsx:decode(list_to_binary(Body)).

build_facility_props(PropValues) ->
    build_generic_props([id, name, address, type], PropValues).

build_patient_props(PropValues) ->
    build_generic_props([id, name, address], PropValues).

build_pharmacy_props(PropValues) ->
    build_generic_props([id, name, address], PropValues).

build_prescription_props(PropValues) ->
    build_generic_props([id, patient_id, prescriber_id, pharmacy_id, date_prescribed, drugs], PropValues).

build_staff_props(PropValues) ->
    build_generic_props([id, name, address, speciality], PropValues).

build_generic_props(List1, List2) ->
    build_generic_props(List1, List2, []).

build_generic_props([], [], Accum) ->
    Accum;
build_generic_props([H1|T1], [H2|T2], Accum) ->
    build_generic_props(T1, T2, lists:append(Accum, [{H1, H2}])).

rpc(Config, Mod, Fun, Args) ->
    Nodename = ?config(node, Config),
    rpc:block_call(Nodename, Mod, Fun, Args).

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
