-module (fmke_proplists).
-include ("fmke.hrl").

-export([encode/2, encode_object/2]).


encode(Type, Object) ->
    encode_object(Type, Object).

encode_object(pharmacy,Object = #pharmacy{}) ->
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

encode_object(facility, Object = #facility{}) ->
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

encode_object(patient, Object = #patient{}) ->
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

encode_object(staff, Object = #staff{}) ->
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

encode_object(prescription, Object = #prescription{}) ->
    PrescriptionId = Object#prescription.id,
    PrescriptionPatientId = Object#prescription.patient_id,
    PrescriptionPharmacyId = Object#prescription.pharmacy_id,
    PrescriptionPrescriberId = Object#prescription.prescriber_id,
    PrescriptionDrugs = Object#prescription.drugs,
    PrescriptionIsProcessed = Object#prescription.is_processed,
    PrescriptionDatePrescribed = Object#prescription.date_prescribed,
    PrescriptionDateProcessed = Object#prescription.date_processed,
    JsonDrugs = encode_object(list_drugs, PrescriptionDrugs),
    [
        {<<"prescriptionId">>, PrescriptionId},
        {<<"prescriptionPatientId">>, PrescriptionPatientId},
        {<<"prescriptionPharmacyId">>, PrescriptionPharmacyId},
        {<<"prescriptionPrescriberId">>, PrescriptionPrescriberId},
        {<<"prescriptionDrugs">>, JsonDrugs},
        {<<"prescriptionIsProcessed">>, PrescriptionIsProcessed},
        {<<"prescriptionDatePrescribed">>, PrescriptionDatePrescribed},
        {<<"prescriptionDateProcessed">>, PrescriptionDateProcessed}
    ];

encode_object(prescription_key, Key) -> Key;

%% TODO unused
encode_object(event, Object) ->
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
%% TODO unused
encode_object(treatment, Object) ->
    Id = treatment:id(Object),
    FacilityId = treatment:facility_id(Object),
    PatientId = treatment:patient_id(Object),
    PrescriberId = treatment:prescriber_id(Object),
    Events = treatment:events(Object),
    Prescriptions = treatment:prescriptions(Object),
    HasEnded= treatment:has_ended(Object),
    DatePrescribed = treatment:date_prescribed(Object),
    DateEnded = treatment:date_ended(Object),

    JsonEvents = encode_object(list_events, Events),
    JsonPrescriptions = encode_object(list_treatments, Prescriptions),
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


encode_object(list_prescriptions, NestedObject) ->
    case is_list(NestedObject) andalso (length(NestedObject)>0) andalso is_binary(hd(NestedObject)) of
        true -> encode_list(prescription_key, NestedObject);
        false -> encode_list(prescription, NestedObject)
    end;

encode_object(list_drugs, NestedObject) ->
    encode_string_list(NestedObject);

encode_object(list_events, NestedObject) ->
    encode_list(event, NestedObject);

encode_object(list_treatments, NestedObject) ->
    encode_list(treatment, NestedObject).

encode_string_list(not_found) -> [];
encode_string_list(List) -> List.

encode_list(_Type, not_found) ->    [];
encode_list(_Type, []) ->           [];
encode_list(Type, List) ->          lists:map(fun(Elem) -> encode_object(Type, Elem) end, List).
