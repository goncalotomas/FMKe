-module (crdt_json_encoder).

-export ([encode/2]).

encode(pharmacy, Object) ->
    PharmacyId = pharmacy:id(Object),
    PharmacyName = pharmacy:name(Object),
    PharmacyAddress = pharmacy:address(Object),
    PharmacyPrescriptions = pharmacy:prescriptions(Object),
    JsonPrescriptions = encode(list_prescriptions, PharmacyPrescriptions),
    lists:flatten(io_lib:format(
        "{\"pharmacyId\": \"~p\",\"pharmacyName\": ~p,\"pharmacyAddress\": ~p,\"pharmacyPrescriptions\": " ++ JsonPrescriptions ++ "}",
        [PharmacyId, PharmacyName, PharmacyAddress]
    ));

encode(facility, Object) ->
    FacilityId = facility:id(Object),
    FacilityName = facility:name(Object),
    FacilityAddress = facility:address(Object),
    FacilityType = facility:type(Object),
    FacilityTreatments = facility:treatments(Object),
    FacilityPrescriptions = facility:treatments(Object),
    JsonTreatments = encode(list_treatments, FacilityTreatments),
    JsonPrescriptions = encode(list_treatments, FacilityPrescriptions),
    lists:flatten(io_lib:format(
        "{\"facilityId\": \"~p\",\"facilityName\": ~p,\"facilityAddress\": ~p,\"facilityType\": ~p,\"facilityTreatments\": " ++ JsonTreatments ++ ",\"facilityPrescriptions\": " ++ JsonPrescriptions ++ "}",
        [FacilityId, FacilityName, FacilityAddress, FacilityType]
    ));

encode(patient, Object) ->
    PatientId = patient:id(Object),
    PatientName = patient:name(Object),
    PatientAddress = patient:address(Object),
    PatientEvents = patient:events(Object),
    PatientTreatments = patient:treatments(Object),
    PatientPrescriptions = patient:treatments(Object),
    JsonEvents = encode(list_events, PatientEvents),
    JsonTreatments = encode(list_treatments, PatientTreatments),
    JsonPrescriptions = encode(list_treatments, PatientPrescriptions),
    lists:flatten(io_lib:format(
        "{\"patientId\": \"~p\",\"patientName\": ~p,\"patientAddress\": ~p,\"patientEvents\": " ++ JsonEvents ++ ",\"patientTreatments\": " ++ JsonTreatments ++ ",\"patientPrescriptions\": " ++ JsonPrescriptions ++ "}",
        [PatientId, PatientName, PatientAddress]
    ));

encode(staff, Object) ->
    StaffId = staff:id(Object),
    StaffName = staff:name(Object),
    StaffSpeciality = staff:speciality(Object),
    StaffAddress = staff:address(Object),
    StaffTreatments = staff:treatments(Object),
    StaffPrescriptions = staff:prescriptions(Object),
    JsonTreatments = encode(list_treatments, StaffTreatments),
    JsonPrescriptions = encode(list_prescriptions, StaffPrescriptions),
    lists:flatten(io_lib:format(
        "{\"staffId\": \"~p\",\"staffName\": ~p,\"staffAddress\": ~p,\"staffSpeciality\": ~p,\"staffTreatments\": " ++ JsonTreatments ++ ",\"staffPrescriptions\": " ++ JsonPrescriptions ++ "}",
        [StaffId, StaffName, StaffAddress, StaffSpeciality]
    ));

encode(event, Object) ->
    EventId = event:id(Object),
    EventPatientId = event:patient_id(Object),
    EventStaffId = event:staff_id(Object),
    EventTimestamp = event:timestamp(Object),
    EventDescription = event:description(Object),
    lists:flatten(io_lib:format(
        "{\"eventId\": \"~p\",\"eventPatientId\": \"~p\",\"eventStaffId\": \"~p\",\"eventTimestamp\": ~p,\"eventDescription\": ~p}",
        [EventId, EventPatientId, EventStaffId, EventTimestamp, EventDescription]
    ));

encode(prescription, Object) ->
    PrescriptionId = prescription:id(Object),
    PrescriptionFacilityId = prescription:facility_id(Object),
    PrescriptionPatientId = prescription:patient_id(Object),
    PrescriptionPharmacyId = prescription:pharmacy_id(Object),
    PrescriptionPrescriberId = prescription:prescriber_id(Object),
    PrescriptionDrugs = prescription:drugs(Object),
    PrescriptionIsProcessed = prescription:is_processed(Object),
    PrescriptionDatePrescribed = prescription:date_prescribed(Object),
    PrescriptionDateProcessed = prescription:date_processed(Object),
    JsonDrugs = encode(list_drugs, PrescriptionDrugs),
    lists:flatten(io_lib:format(
        "{\"prescriptionId\": \"~p\",\"prescriptionFacilityId\": \"~p\",\"prescriptionPatientId\": \"~p\",\"prescriptionPharmacyId\": \"~p\",\"prescriptionPrescriberId\": \"~p\",\"prescriptionDrugs\": ~p,\"prescriptionIsProcessed\": ~p,\"prescriptionDatePrescribed\": ~p,\"prescriptionDateProcessed\": ~p}",
        [PrescriptionId, PrescriptionFacilityId, PrescriptionPatientId, PrescriptionPharmacyId, PrescriptionPrescriberId, JsonDrugs, PrescriptionIsProcessed, PrescriptionDatePrescribed, PrescriptionDateProcessed]
    ));

encode(treatment, Object) ->
    Id = treatment:id(Object),
    FacilityId = treatment:facility_id(Object),
    PatientId = treatment:patient_id(Object),
    PrescriberId = treatment:prescriber_id(Object),
    Events = treatment:events(Object),
    Prescriptions = treatment:prescriptions(Object),
    HasEnded= treatment:has_ended(Object),
    DatePrescribed = treatment:date_prescribed(Object),
    DateEnded = treatment:date_ended(Object),
    JsonEvents = encode(list_events, Events),
    JsonPrescriptions = encode(list_treatments, Prescriptions),
    lists:flatten(io_lib:format(
        "{\"treatmentId\": \"~p\",\"treatmentFacilityId\": \"~p\",\"treatmentPatientId\": \"~p\",\"treatmentPrescriberId\": \"~p\"\"treatmentHasEnded\": ~p,\"treatmentEvents\": " ++ JsonEvents ++ ",\"treatmentPrescriptions\": " ++ JsonPrescriptions ++ ",\"treatmentDatePrescribed\": ~p,\"treatmentDateEnded\": ~p}",
        [Id, FacilityId, PatientId, PrescriberId, HasEnded, DatePrescribed, DateEnded]
    ));

encode(list_prescriptions, NestedObject) ->
    case NestedObject of
        not_found ->
            "[]";
        _ListPrescriptions ->
            "[" ++ [encode(prescription,PrescriptionObject) || {{_PrescriptionKey, antidote_crdt_gmap},PrescriptionObject} <- NestedObject] ++ "]"
    end;

encode(list_drugs, PrescriptionDrugs) ->
    case PrescriptionDrugs of
        [] ->
            "[]";
        _ListDrugs ->
            [binary_to_list(X) || X <- PrescriptionDrugs]
    end;

encode(list_events, NestedObject) ->
    case NestedObject of
        not_found ->
            "[]";
        _ListEvents ->
            "[" ++ [encode(treatment,EventObject) || {{_EventKey, antidote_crdt_gmap},EventObject} <- NestedObject] ++ "]"
    end;

encode(list_treatments, NestedObject) ->
    case NestedObject of
        not_found ->
            "[]";
        _ListPrescriptions ->
            "[" ++ [encode(treatment,TreatmentObject) || {{_TreatmentKey, antidote_crdt_gmap},TreatmentObject} <- NestedObject] ++ "]"
    end.
