%% This module acts as a library for encoding FMKe entities into JSON objects, used in the HTTP API.
-module(fmke_json).
-include("fmke.hrl").
-include("fmke_kv.hrl").

-export([encode/1, decode/2]).

-spec encode(Object :: app_record()) -> jsx:json_term().

encode(Object = #pharmacy{}) ->
    PharmacyId = Object#pharmacy.id,
    PharmacyName = Object#pharmacy.name,
    PharmacyAddress = Object#pharmacy.address,
    PharmacyPrescriptions = Object#pharmacy.prescriptions,
    JsonPrescriptions = encode_list_prescriptions(PharmacyPrescriptions),
    [
        {<<"pharmacyId">>, PharmacyId},
        {<<"pharmacyName">>, PharmacyName},
        {<<"pharmacyAddress">>, PharmacyAddress},
        {<<"pharmacyPrescriptions">>, JsonPrescriptions}
    ];

encode(Object = #facility{}) ->
    FacilityId = Object#facility.id,
    FacilityName = Object#facility.name,
    FacilityAddress = Object#facility.address,
    FacilityType = Object#facility.type,
    [
        {<<"facilityId">>, FacilityId},
        {<<"facilityName">>, FacilityName},
        {<<"facilityAddress">>, FacilityAddress},
        {<<"facilityType">>, FacilityType}
    ];

encode(Object = #patient{}) ->
    PatientId = Object#patient.id,
    PatientName = Object#patient.name,
    PatientAddress = Object#patient.address,
    PatientPrescriptions = Object#patient.prescriptions,
    JsonPrescriptions = encode_list_prescriptions(PatientPrescriptions),
    [
        {<<"patientId">>, PatientId},
        {<<"patientName">>, PatientName},
        {<<"patientAddress">>, PatientAddress},
        {<<"patientPrescriptions">>, JsonPrescriptions}
    ];

encode(Object = #staff{}) ->
    StaffId = Object#staff.id,
    StaffName = Object#staff.name,
    StaffAddress = Object#staff.address,
    StaffSpeciality = Object#staff.speciality,
    StaffPrescriptions = Object#staff.prescriptions,
    JsonPrescriptions = encode_list_prescriptions(StaffPrescriptions),
    [
        {<<"staffId">>, StaffId},
        {<<"staffName">>, StaffName},
        {<<"staffAddress">>, StaffAddress},
        {<<"staffSpeciality">>, StaffSpeciality},
        {<<"staffPrescriptions">>, JsonPrescriptions}
    ];

encode(Object = #prescription{}) ->
    PrescriptionId = Object#prescription.id,
    PrescriptionPatientId = Object#prescription.patient_id,
    PrescriptionPharmacyId = Object#prescription.pharmacy_id,
    PrescriptionPrescriberId = Object#prescription.prescriber_id,
    PrescriptionDrugs = Object#prescription.drugs,
    PrescriptionIsProcessed = Object#prescription.is_processed,
    PrescriptionDatePrescribed = Object#prescription.date_prescribed,
    PrescriptionDateProcessed = Object#prescription.date_processed,
    [
        {<<"prescriptionId">>, PrescriptionId},
        {<<"prescriptionPatientId">>, PrescriptionPatientId},
        {<<"prescriptionPharmacyId">>, PrescriptionPharmacyId},
        {<<"prescriptionPrescriberId">>, PrescriptionPrescriberId},
        {<<"prescriptionDrugs">>, encode_string_list(PrescriptionDrugs)},
        {<<"prescriptionIsProcessed">>, PrescriptionIsProcessed},
        {<<"prescriptionDatePrescribed">>, PrescriptionDatePrescribed},
        {<<"prescriptionDateProcessed">>, PrescriptionDateProcessed}
    ].

-spec encode_list_prescriptions(L :: list(binary() | list(prescription()) | list(non_neg_integer()))) ->
                    list(binary()) | list(non_neg_integer()).
encode_list_prescriptions(L) ->
    encode_list_prescriptions(L, []).

encode_list_prescriptions([], Accum) ->
    lists:reverse(Accum);
encode_list_prescriptions([H|T], Accum) when is_record(H, prescription) ->
    encode_list_prescriptions(T, [encode(H) | Accum]);
encode_list_prescriptions([H|T], Accum) ->
    encode_list_prescriptions(T, [H | Accum]).


-spec encode_string_list(L :: list(string() | binary())) -> list(binary()).
encode_string_list(L) ->
    encode_string_list(L, []).

encode_string_list([], Accum) ->
    lists:reverse(Accum);
encode_string_list([H|T], Accum) when is_list(H) ->
    encode_string_list(T, [list_to_binary(H) | Accum]);
encode_string_list([H|T], Accum) when is_binary(H) ->
    encode_string_list(T, [H | Accum]).

-spec decode(Entity :: entity(), Object :: jsx:json_term()) -> app_record().

decode(prescription, PropList) when is_list(PropList)->
    #prescription{
        id = proplists:get_value(<<"prescriptionId">>, PropList)
        ,patient_id = proplists:get_value(<<"prescriptionPatientId">>, PropList)
        ,prescriber_id = proplists:get_value(<<"prescriptionPrescriberId">>, PropList)
        ,pharmacy_id = proplists:get_value(<<"prescriptionPharmacyId">>, PropList)
        ,date_prescribed = proplists:get_value(<<"prescriptionDatePrescribed">>, PropList)
        ,date_processed = proplists:get_value(<<"prescriptionDateProcessed">>, PropList)
        ,is_processed = proplists:get_value(<<"prescriptionIsProcessed">>, PropList)
        ,drugs = lists:sort(proplists:get_value(<<"prescriptionDrugs">>, PropList))
    };

decode(prescription, Prescription) when is_binary(Prescription); is_integer(Prescription) ->
    Prescription.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_prescription_obj_list_test() ->
    ?assertEqual([[
        {<<"prescriptionId">>, 2},
        {<<"prescriptionPatientId">>, 1},
        {<<"prescriptionPharmacyId">>, 1},
        {<<"prescriptionPrescriberId">>, 1},
        {<<"prescriptionDrugs">>, [<<"Rupatadine">>]},
        {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
        {<<"prescriptionDatePrescribed">>, "19/01/2018"},
        {<<"prescriptionDateProcessed">>, "20/01/2018"}
    ]],
    encode_list_prescriptions([
        #prescription{id = 2, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, date_prescribed = "19/01/2018",
                      drugs = ["Rupatadine"], date_processed = "20/01/2018",
                      is_processed = <<"prescription_processed">>
                      }
    ])).

encode_prescription_ref_list_test() ->
    [<<"prescription1">>, <<"prescription2">>] = encode_list_prescriptions([<<"prescription1">>, <<"prescription2">>]).

encode_string_list_test() ->
    [<<"b">>, <<"D">>] = encode_string_list(["b", <<"D">>]).

encode_basic_facility_test() ->
    [
        {<<"facilityId">>, 1},
        {<<"facilityName">>, "fmke"},
        {<<"facilityAddress">>, "github"},
        {<<"facilityType">>, "benchmark"}
    ] = encode(#facility{id = 1, name = "fmke", address = "github", type = "benchmark"}).

encode_basic_patient_test() ->
    [
        {<<"patientId">>, 1},
        {<<"patientName">>, "fmke"},
        {<<"patientAddress">>, "github"},
        {<<"patientPrescriptions">>, []}
    ] = encode(#patient{id = 1, name = "fmke", address = "github"}).

encode_patient_with_nested_prescriptions_test() ->
    [
        {<<"patientId">>, 1},
        {<<"patientName">>, "fmke"},
        {<<"patientAddress">>, "github"},
        {<<"patientPrescriptions">>, [
            [
                {<<"prescriptionId">>, 1},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, [<<"Acetaminophen">>, <<"Ibuprofen">>]},
                {<<"prescriptionIsProcessed">>, <<"prescription_not_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, <<"undefined">>}
            ],
            [
                {<<"prescriptionId">>, 2},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, [<<"Rupatadine">>]},
                {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, "19/01/2018"}
            ]
        ]}
    ] = encode(
        #patient{id = 1, name = "fmke", address = "github", prescriptions =
            [#prescription{id = 1, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, drugs = ["Acetaminophen",
                "Ibuprofen"], date_prescribed = "19/01/2018"
            },
            #prescription{id = 2, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, date_prescribed = "19/01/2018",
                drugs = ["Rupatadine"], date_processed = "19/01/2018", is_processed = <<"prescription_processed">>
            }]
        }).

encode_patient_with_prescription_references_test() ->
    [
        {<<"patientId">>, 1},
        {<<"patientName">>, "fmke"},
        {<<"patientAddress">>, "github"},
        {<<"patientPrescriptions">>, [<<"prescription_2">>, <<"prescription_1">>]}
    ] = encode(
        #patient{id = 1, name="fmke", address="github", prescriptions=[<<"prescription_2">>, <<"prescription_1">>]}
    ).

encode_basic_pharmacy_test() ->
    [
        {<<"pharmacyId">>, 1},
        {<<"pharmacyName">>, "fmke"},
        {<<"pharmacyAddress">>, "github"},
        {<<"pharmacyPrescriptions">>, []}
    ] = encode(#pharmacy{id = 1, name = "fmke", address = "github"}).

encode_pharmacy_with_nested_prescriptions_test() ->
    [
        {<<"pharmacyId">>, 1},
        {<<"pharmacyName">>, "fmke"},
        {<<"pharmacyAddress">>, "github"},
        {<<"pharmacyPrescriptions">>, [
            [
                {<<"prescriptionId">>, 1},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, [<<"Acetaminophen">>, <<"Ibuprofen">>]},
                {<<"prescriptionIsProcessed">>, <<"prescription_not_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, <<"undefined">>}
            ],
            [
                {<<"prescriptionId">>, 2},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, [<<"Rupatadine">>]},
                {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, "19/01/2018"}
            ]
        ]}
    ] = encode(
        #pharmacy{id = 1, name = "fmke", address = "github", prescriptions =
            [#prescription{id = 1, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, drugs = ["Acetaminophen",
                "Ibuprofen"], date_prescribed = "19/01/2018"
            },
            #prescription{id = 2, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, date_prescribed = "19/01/2018",
                drugs = ["Rupatadine"], date_processed = "19/01/2018", is_processed = <<"prescription_processed">>
            }]
        }).

encode_pharmacy_with_prescription_references_test() ->
    [
        {<<"pharmacyId">>, 1},
        {<<"pharmacyName">>, "fmke"},
        {<<"pharmacyAddress">>, "github"},
        {<<"pharmacyPrescriptions">>, [<<"prescription_2">>, <<"prescription_1">>]}
    ] = encode(
        #pharmacy{id = 1, name="fmke", address="github", prescriptions=[<<"prescription_2">>, <<"prescription_1">>]}
    ).

encode_basic_medical_staff_test() ->
    [
        {<<"staffId">>, 1},
        {<<"staffName">>, "fmke"},
        {<<"staffAddress">>, "github"},
        {<<"staffSpeciality">>, "benchmarks"},
        {<<"staffPrescriptions">>, []}
    ] = encode(#staff{id = 1, name = "fmke", address = "github", speciality = "benchmarks"}).

encode_medical_staff_with_nested_prescriptions_test() ->
    [
        {<<"staffId">>, 1},
        {<<"staffName">>, "fmke"},
        {<<"staffAddress">>, "github"},
        {<<"staffSpeciality">>, "benchmarks"},
        {<<"staffPrescriptions">>, [
            [
                {<<"prescriptionId">>, 1},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, [<<"Acetaminophen">>, <<"Ibuprofen">>]},
                {<<"prescriptionIsProcessed">>, <<"prescription_not_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, <<"undefined">>}
            ],
            [
                {<<"prescriptionId">>, 2},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, [<<"Rupatadine">>]},
                {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, "19/01/2018"}
            ]
        ]}
    ] = encode(
        #staff{id = 1, name = "fmke", address = "github", speciality = "benchmarks", prescriptions =
            [#prescription{id = 1, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, drugs = ["Acetaminophen",
                "Ibuprofen"], date_prescribed = "19/01/2018"
            },
            #prescription{id = 2, patient_id = 1, pharmacy_id = 1, prescriber_id = 1, date_prescribed = "19/01/2018",
                drugs = ["Rupatadine"], date_processed = "19/01/2018", is_processed = <<"prescription_processed">>
            }]
        }).

encode_medical_staff_with_prescription_references_test() ->
    [
        {<<"staffId">>, 1},
        {<<"staffName">>, "fmke"},
        {<<"staffAddress">>, "github"},
        {<<"staffSpeciality">>, "benchmarks"},
        {<<"staffPrescriptions">>, [<<"prescription_2">>, <<"prescription_1">>]}
    ] = encode(
        #staff{id = 1, name = "fmke", address = "github", speciality = "benchmarks", prescriptions = [
            <<"prescription_2">>, <<"prescription_1">>
        ]}
    ).

decode_prescription_test() ->
    JsonPrescription = [
        {<<"prescriptionId">>, 1}
        ,{<<"prescriptionPatientId">>, 2}
        ,{<<"prescriptionPharmacyId">>, 3}
        ,{<<"prescriptionPrescriberId">>, 4}
        ,{<<"prescriptionDatePrescribed">>, "02/04/2018"}
        ,{<<"prescriptionDateProcessed">>, "06/04/2018"}
        ,{<<"prescriptionIsProcessed">>, ?PRESCRIPTION_PROCESSED_VALUE}
        ,{<<"prescriptionDrugs">>, [<<"Rupafin">>, <<"Ibuprofen">>]}
    ],
    ExpectedPrescription =  #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    DecodedPrescription = decode(prescription, JsonPrescription),
    ?assert(fmke_test_utils:compare_prescriptions(ExpectedPrescription, DecodedPrescription)).

-endif.
