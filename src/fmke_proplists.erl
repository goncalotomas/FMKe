-module (fmke_proplists).
-include ("fmke.hrl").

-export([encode_object/1]).

encode_object(Object = #pharmacy{}) ->
    PharmacyId = Object#pharmacy.id,
    PharmacyName = Object#pharmacy.name,
    PharmacyAddress = Object#pharmacy.address,
    PharmacyPrescriptions = Object#pharmacy.prescriptions,
    JsonPrescriptions = encode_object(list_prescriptions, PharmacyPrescriptions),
    [
        {<<"pharmacyId">>, PharmacyId},
        {<<"pharmacyName">>, PharmacyName},
        {<<"pharmacyAddress">>, PharmacyAddress},
        {<<"pharmacyPrescriptions">>, JsonPrescriptions}
    ];

encode_object(Object = #facility{}) ->
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

encode_object(Object = #patient{}) ->
    PatientId = Object#patient.id,
    PatientName = Object#patient.name,
    PatientAddress = Object#patient.address,
    PatientPrescriptions = Object#patient.prescriptions,
    JsonPrescriptions = encode_object(list_prescriptions, PatientPrescriptions),
    [
        {<<"patientId">>, PatientId},
        {<<"patientName">>, PatientName},
        {<<"patientAddress">>, PatientAddress},
        {<<"patientPrescriptions">>, JsonPrescriptions}
    ];

encode_object(Object = #staff{}) ->
    StaffId = Object#staff.id,
    StaffName = Object#staff.name,
    StaffAddress = Object#staff.address,
    StaffSpeciality = Object#staff.speciality,
    StaffPrescriptions = Object#staff.prescriptions,
    JsonPrescriptions = encode_object(list_prescriptions, StaffPrescriptions),
    [
        {<<"staffId">>, StaffId},
        {<<"staffName">>, StaffName},
        {<<"staffAddress">>, StaffAddress},
        {<<"staffSpeciality">>, StaffSpeciality},
        {<<"staffPrescriptions">>, JsonPrescriptions}
    ];

encode_object(Object = #prescription{}) ->
    PrescriptionId = Object#prescription.id,
    PrescriptionPatientId = Object#prescription.patient_id,
    PrescriptionPharmacyId = Object#prescription.pharmacy_id,
    PrescriptionPrescriberId = Object#prescription.prescriber_id,
    PrescriptionDrugs = Object#prescription.drugs,
    PrescriptionIsProcessed = Object#prescription.is_processed,
    PrescriptionDatePrescribed = Object#prescription.date_prescribed,
    PrescriptionDateProcessed = Object#prescription.date_processed,
    JsonDrugs = encode_string_list(PrescriptionDrugs),
    [
        {<<"prescriptionId">>, PrescriptionId},
        {<<"prescriptionPatientId">>, PrescriptionPatientId},
        {<<"prescriptionPharmacyId">>, PrescriptionPharmacyId},
        {<<"prescriptionPrescriberId">>, PrescriptionPrescriberId},
        {<<"prescriptionDrugs">>, JsonDrugs},
        {<<"prescriptionIsProcessed">>, PrescriptionIsProcessed},
        {<<"prescriptionDatePrescribed">>, PrescriptionDatePrescribed},
        {<<"prescriptionDateProcessed">>, PrescriptionDateProcessed}
    ].

encode_object(list_prescriptions, NestedObject) ->
    case is_list(NestedObject) andalso (length(NestedObject)>0) andalso is_binary(hd(NestedObject)) of
        true -> NestedObject;
        false -> lists:map(fun encode_object/1, NestedObject)
    end.

encode_string_list(not_found) -> [];
encode_string_list(List) -> List.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_basic_facility_test() ->
    [
        {<<"facilityId">>, 1},
        {<<"facilityName">>, "fmke"},
        {<<"facilityAddress">>, "github"},
        {<<"facilityType">>, "benchmark"}
    ] = encode_object(#facility{id = 1, name = "fmke", address = "github", type = "benchmark"}).

encode_basic_patient_test() ->
    [
        {<<"patientId">>, 1},
        {<<"patientName">>, "fmke"},
        {<<"patientAddress">>, "github"},
        {<<"patientPrescriptions">>, []}
    ] = encode_object(#patient{id = 1, name = "fmke", address = "github"}).

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
                {<<"prescriptionDrugs">>, ["Acetaminophen", "Ibuprofen"]},
                {<<"prescriptionIsProcessed">>, <<"prescription_not_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, <<"undefined">>}
            ],
            [
                {<<"prescriptionId">>, 2},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, ["Rupatadine"]},
                {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, "19/01/2018"}
            ]
        ]}
    ] = encode_object(
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
    ] = encode_object(
        #patient{id = 1, name="fmke", address="github", prescriptions=[<<"prescription_2">>, <<"prescription_1">>]}
    ).

encode_basic_pharmacy_test() ->
    [
        {<<"pharmacyId">>, 1},
        {<<"pharmacyName">>, "fmke"},
        {<<"pharmacyAddress">>, "github"},
        {<<"pharmacyPrescriptions">>, []}
    ] = encode_object(#pharmacy{id = 1, name = "fmke", address = "github"}).

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
                {<<"prescriptionDrugs">>, ["Acetaminophen", "Ibuprofen"]},
                {<<"prescriptionIsProcessed">>, <<"prescription_not_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, <<"undefined">>}
            ],
            [
                {<<"prescriptionId">>, 2},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, ["Rupatadine"]},
                {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, "19/01/2018"}
            ]
        ]}
    ] = encode_object(
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
    ] = encode_object(
        #pharmacy{id = 1, name="fmke", address="github", prescriptions=[<<"prescription_2">>, <<"prescription_1">>]}
    ).

encode_basic_medical_staff_test() ->
    [
        {<<"staffId">>, 1},
        {<<"staffName">>, "fmke"},
        {<<"staffAddress">>, "github"},
        {<<"staffSpeciality">>, "benchmarks"},
        {<<"staffPrescriptions">>, []}
    ] = encode_object(#staff{id = 1, name = "fmke", address = "github", speciality = "benchmarks"}).

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
                {<<"prescriptionDrugs">>, ["Acetaminophen", "Ibuprofen"]},
                {<<"prescriptionIsProcessed">>, <<"prescription_not_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, <<"undefined">>}
            ],
            [
                {<<"prescriptionId">>, 2},
                {<<"prescriptionPatientId">>, 1},
                {<<"prescriptionPharmacyId">>, 1},
                {<<"prescriptionPrescriberId">>, 1},
                {<<"prescriptionDrugs">>, ["Rupatadine"]},
                {<<"prescriptionIsProcessed">>, <<"prescription_processed">>},
                {<<"prescriptionDatePrescribed">>, "19/01/2018"},
                {<<"prescriptionDateProcessed">>, "19/01/2018"}
            ]
        ]}
    ] = encode_object(
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
    ] = encode_object(
        #staff{id = 1, name = "fmke", address = "github", speciality = "benchmarks", prescriptions = [
            <<"prescription_2">>, <<"prescription_1">>
        ]}
    ).

-endif.
