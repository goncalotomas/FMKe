-module (crdt_json_encoder).

-export ([encode/2]).


encode(Type, Object) ->
    jsx:encode(encode_intern(Type, Object)).

encode_intern(pharmacy, Object) ->
    PharmacyId = pharmacy:id(Object),
    PharmacyName = list_to_binary(pharmacy:name(Object)),
    PharmacyAddress = list_to_binary(pharmacy:address(Object)),
    PharmacyPrescriptions = pharmacy:prescriptions(Object),
    JsonPrescriptions = encode_intern(list_prescriptions, PharmacyPrescriptions),
    [
        {<<"pharmacyId">>, PharmacyId},
        {<<"pharmacyName">>, PharmacyName},
        {<<"pharmacyAddress">>, PharmacyAddress},
        {<<"pharmacyPrescriptions">>, JsonPrescriptions}
    ];

encode_intern(facility, Object) ->
    FacilityId = facility:id(Object),
    FacilityName = list_to_binary(facility:name(Object)),
    FacilityAddress = list_to_binary(facility:address(Object)),
    FacilityType = list_to_binary(facility:type(Object)),
    FacilityTreatments = facility:treatments(Object),
    FacilityPrescriptions = facility:treatments(Object),
    JsonTreatments = encode_intern(list_treatments, FacilityTreatments),
    JsonPrescriptions = encode_intern(list_treatments, FacilityPrescriptions),
    [
        {<<"facilityId">>, FacilityId},
        {<<"facilityName">>, FacilityName},
        {<<"facilityAddress">>, FacilityAddress},
        {<<"facilityType">>, FacilityType},
        {<<"facilityTreatments">>, JsonTreatments},
        {<<"facilityPrescriptions">>, JsonPrescriptions}
    ];

encode_intern(patient, Object) ->
    PatientId = patient:id(Object),
    PatientName = list_to_binary(patient:name(Object)),
    PatientAddress = list_to_binary(patient:address(Object)),
    PatientEvents = patient:events(Object),
    PatientTreatments = patient:treatments(Object),
    PatientPrescriptions = patient:treatments(Object),
    JsonEvents = encode_intern(list_events, PatientEvents),
    JsonTreatments = encode_intern(list_treatments, PatientTreatments),
    JsonPrescriptions = encode_intern(list_treatments, PatientPrescriptions),
    [
        {<<"patientId">>, PatientId},
        {<<"patientName">>, PatientName},
        {<<"patientAddress">>, PatientAddress},
        {<<"patientEvents">>, JsonEvents},
        {<<"patientTreatments">>, JsonTreatments},
        {<<"patientPrescriptions">>, JsonPrescriptions}
    ];

encode_intern(staff, Object) ->
    StaffId = staff:id(Object),
    StaffName = list_to_binary(staff:name(Object)),
    StaffSpeciality = list_to_binary(staff:speciality(Object)),
    StaffAddress = list_to_binary(staff:address(Object)),
    StaffTreatments = staff:treatments(Object),
    StaffPrescriptions = staff:prescriptions(Object),
    JsonTreatments = encode_intern(list_treatments, StaffTreatments),
    JsonPrescriptions = encode_intern(list_prescriptions, StaffPrescriptions),
    [
        {<<"staffId">>, StaffId},
        {<<"staffName">>, StaffName},
        {<<"staffAddress">>, StaffAddress},
        {<<"staffSpeciality">>, StaffSpeciality},
        {<<"staffTreatments">>, JsonTreatments},
        {<<"staffPrescriptions">>, JsonPrescriptions}
    ];


encode_intern(event, Object) ->
    EventId = event:id(Object),
    EventPatientId = event:patient_id(Object),
    EventStaffId = event:staff_id(Object),
    EventTimestamp = list_to_binary(event:timestamp(Object)),
    EventDescription = list_to_binary(event:description(Object)),
    [
        {<<"eventId">>, EventId},
        {<<"eventPatientId">>, EventPatientId},
        {<<"eventStaffId">>, EventStaffId},
        {<<"eventTimestamp">>, EventTimestamp},
        {<<"eventDescription">>, EventDescription}
    ];

encode_intern(prescription, Object) ->
    PrescriptionId = prescription:id(Object),
    PrescriptionFacilityId = prescription:facility_id(Object),
    PrescriptionPatientId = prescription:patient_id(Object),
    PrescriptionPharmacyId = prescription:pharmacy_id(Object),
    PrescriptionPrescriberId = prescription:prescriber_id(Object),
    PrescriptionDrugs = prescription:drugs(Object),
    PrescriptionIsProcessed = prescription:is_processed(Object),
    PrescriptionDatePrescribed = list_to_binary(prescription:date_prescribed(Object)),
    PrescriptionDateProcessed = list_to_binary(prescription:date_processed(Object)),
    JsonDrugs = encode_intern(list_drugs, PrescriptionDrugs),
    [
        {<<"prescriptionId">>, PrescriptionId},
        {<<"prescriptionFacilityId">>, PrescriptionFacilityId},
        {<<"prescriptionPatientId">>, PrescriptionPatientId},
        {<<"prescriptionPharmacyId">>, PrescriptionPharmacyId},
        {<<"prescriptionPrescriberId">>, PrescriptionPrescriberId},
        {<<"prescriptionDrugs">>, JsonDrugs},
        {<<"prescriptionIsProcessed">>, PrescriptionIsProcessed},
        {<<"prescriptionDatePrescribed">>, PrescriptionDatePrescribed},
        {<<"prescriptionDateProcessed">>, PrescriptionDateProcessed}
    ];

encode_intern(treatment, Object) ->
    Id = treatment:id(Object),
    FacilityId = treatment:facility_id(Object),
    PatientId = treatment:patient_id(Object),
    PrescriberId = treatment:prescriber_id(Object),
    Events = treatment:events(Object),
    Prescriptions = treatment:prescriptions(Object),
    HasEnded= treatment:has_ended(Object),
    DatePrescribed = treatment:date_prescribed(Object),
    DateEnded = treatment:date_ended(Object),
    JsonEvents = encode_intern(list_events, Events),
    JsonPrescriptions = encode_intern(list_treatments, Prescriptions),
    [
        {<<"treatmentId">>, Id},
        {<<"treatmentFacilityId">>, FacilityId},
        {<<"treatmentPatientId">>, PatientId},
        {<<"treatmentPrescriberId">>, PrescriberId},
        {<<"treatmentHasEnded">>, HasEnded},
        {<<"treatmentEvents">>, JsonEvents},
        {<<"treatmentPrescriptions">>, JsonPrescriptions},
        {<<"treatmentDatePrescribed">>, DatePrescribed},
        {<<"treatmentDateEnded">>, DateEnded}
    ];


encode_intern(list_prescriptions, NestedObject) ->
    encode_list(prescription, NestedObject);

encode_intern(list_drugs, NestedObject) ->
    encode_string_list(NestedObject);

encode_intern(list_events, NestedObject) ->
    encode_list(event, NestedObject);

encode_intern(list_treatments, NestedObject) ->
    encode_list(treatment, NestedObject).



encode_string_list(not_found) -> [];
encode_string_list(List) -> List.

encode_list(_Type, not_found) ->
    [];
encode_list(Type, List) ->
    [encode_intern(Type, X) || {{_EventKey, antidote_crdt_gmap}, X} <- List].