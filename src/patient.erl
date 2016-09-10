-module(patient).
-include("fmk.hrl").


%% Functions to handle single patient objects
-export ([
  new/3,
  update_personal_details/2,
  name/1,
  id/1,
  address/1,
  treatments/1,
  prescriptions/1,
  events/1,
  add_treatment/4,
  add_treatment/5
  ]).

%% Creates a new patient object from an ID, Name and Address. Returns an update operation ready to insert into Antidote
new(Id,Name,Address) ->
  IdOp = antidote_lib:build_map_op(?PATIENT_ID,?PATIENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?PATIENT_NAME,?PATIENT_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  [IdOp,NameOp,AddressOp].

update_personal_details(Name,Address) ->
  NameOp = antidote_lib:build_map_op(?PATIENT_NAME,?PATIENT_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  [NameOp,AddressOp].

%% Returns the patient name as a list from a patient object
-spec name(Patient::?NESTED_MAP:map()) -> string().
name(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_NAME,?PATIENT_NAME_CRDT) of
    not_found -> {error,not_found};
    Name -> binary_to_list(Name)
  end.

%% Returns the patient id as an integer from a patient object
-spec id(Patient::?NESTED_MAP:map()) -> pos_integer().
id(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_ID,?PATIENT_ID_CRDT) of
    not_found -> {error,not_found};
    Id -> Id
  end.

%% Returns the patient address as a list from a patient object
-spec address(Patient::?NESTED_MAP:map()) -> string().
address(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

%% Returns the patient treatments as a Riak map from a patient object
-spec treatments(Patient::?NESTED_MAP:map()) -> ?NESTED_MAP:map().
treatments(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_TREATMENTS,?NESTED_MAP) of
    not_found -> [];
    Treatments -> Treatments
  end.

%% Returns the patient prescriptions as a Riak map from a patient object
-spec prescriptions(Patient::?NESTED_MAP:map()) -> ?NESTED_MAP:map().
prescriptions(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_PRESCRIPTIONS,?NESTED_MAP) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

%% Returns the patient events as a Riak map from a patient object
-spec events(Patient::?NESTED_MAP:map()) -> ?NESTED_MAP:map().
events(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_EVENTS,?NESTED_MAP) of
    not_found -> [];
    Events -> Events
  end.

add_treatment(TreatmentId, PrescriberId, FacilityId, DateStarted) ->
  TreatmentIdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(TreatmentId)),
  PrescriberIdOp = antidote_lib:build_map_op(?STAFF_ID,?STAFF_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  FacilityIdOp = antidote_lib:build_map_op(?FACILITY_ID,?FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  DateStartedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateStarted))),
  ListOps = [TreatmentIdOp,PrescriberIdOp,FacilityIdOp,DateStartedOp],
  PatientTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  PatientTreatmentsOp = antidote_lib:build_nested_map_op(?PATIENT_TREATMENTS,?NESTED_MAP,PatientTreatmentKey,ListOps),
  [PatientTreatmentsOp].

add_treatment(TreatmentId, PrescriberId, FacilityId, DateStarted, DateEnded) ->
  TreatmentIdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(TreatmentId)),
  PrescriberIdOp = antidote_lib:build_map_op(?STAFF_ID,?STAFF_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  FacilityIdOp = antidote_lib:build_map_op(?FACILITY_ID,?FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  DateStartedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateStarted))),
  DateEndedOp = antidote_lib:build_map_op(?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateEnded))),
  ListOps = [TreatmentIdOp,PrescriberIdOp,FacilityIdOp,DateStartedOp,DateEndedOp],
  PatientTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  PatientTreatmentsOp = antidote_lib:build_nested_map_op(?PATIENT_TREATMENTS,?NESTED_MAP,PatientTreatmentKey,ListOps),
  [PatientTreatmentsOp].

add_prescription(PrescriptionId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  PrescriptionIdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(PrescriptionId)),
  PrescriberIdOp = antidote_lib:build_map_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  PharmacyIdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(PharmacyId)),
  FacilityIdOp = antidote_lib:build_map_op(?FACILITY_ID,?FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  DateStartedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PrescriberIdOp,PharmacyIdOp,FacilityIdOp,DateStartedOp,DrugsOp],
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  PatientPrescriptionsOp = antidote_lib:build_nested_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,ListOps),
  [PatientPrescriptionsOp].