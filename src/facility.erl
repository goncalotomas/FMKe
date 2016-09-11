-module(facility).
-include("fmk.hrl").

%% Functions to handle single Facility objects
-export ([
  new/4,
  update_details/3,
  name/1,
  id/1,
  address/1,
  type/1,
  prescriptions/1,
  treatments/1,
  add_prescription/6,
  add_event/1,
  add_treatment/4,
  add_treatment/5
  ]).

new(Id,Name,Address,Type) ->
  IdOp = antidote_lib:build_map_op(?FACILITY_ID,?FACILITY_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?FACILITY_NAME,?FACILITY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  TypeOp = antidote_lib:build_map_op(?FACILITY_TYPE,?FACILITY_TYPE_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Type))),
  [IdOp,NameOp,AddressOp,TypeOp].


update_details(Name,Address,Type) ->
  NameOp = antidote_lib:build_map_op(?FACILITY_NAME,?FACILITY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  TypeOp = antidote_lib:build_map_op(?FACILITY_TYPE,?FACILITY_TYPE_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Type))),
  [NameOp,AddressOp,TypeOp].

name(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_NAME,?FACILITY_NAME_CRDT) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

type(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_TYPE,?FACILITY_TYPE_CRDT) of
    not_found -> "";
    Type -> binary_to_list(Type)
  end.

id(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_ID,?FACILITY_ID) of
    not_found -> 0;
    Id -> Id
  end.

address(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

prescriptions(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_PRESCRIPTIONS,?FACILITY_PRESCRIPTIONS_CRDT) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

treatments(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_TREATMENTS,?FACILITY_TREATMENTS_CRDT) of
    not_found -> [];
    Treatments -> Treatments
  end.

add_event(Event) ->
  Id = event:id(Event),
  Description = event:description(Event),
  Timestamp = event:timestamp(Event),
  %% Make operations to insert a new nested map
  IdOp = antidote_lib:build_map_op(?EVENT_ID,?EVENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  DescriptionOp = antidote_lib:build_map_op(?EVENT_DESCRIPTION,?EVENT_DESCRIPTION_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Description))),
  TimestampOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Timestamp))),
  [IdOp,DescriptionOp,TimestampOp].

add_treatment(TreatmentId, PatientId, PrescriberId, DateStarted) ->
  TreatmentIdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(TreatmentId)),
  PatientIdOp = antidote_lib:build_map_op(?PATIENT_ID,?PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PrescriberIdOp = antidote_lib:build_map_op(?STAFF_ID,?STAFF_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  DateStartedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateStarted))),
  ListOps = [TreatmentIdOp,PrescriberIdOp,PatientIdOp,DateStartedOp],
  FacilityTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  FacilityTreatmentsOp = antidote_lib:build_nested_map_op(?FACILITY_TREATMENTS,?NESTED_MAP,FacilityTreatmentKey,ListOps),
  [FacilityTreatmentsOp]. 

add_treatment(TreatmentId, PatientId, PrescriberId, DateStarted, DateEnded) ->
  TreatmentIdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(TreatmentId)),
  PatientIdOp = antidote_lib:build_map_op(?PATIENT_ID,?PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PrescriberIdOp = antidote_lib:build_map_op(?STAFF_ID,?STAFF_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  DateStartedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateStarted))),
  DateEndedOp = antidote_lib:build_map_op(?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateEnded))),
  ListOps = [TreatmentIdOp,PrescriberIdOp,PatientIdOp,DateStartedOp,DateEndedOp],
  FacilityTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  FacilityTreatmentsOp = antidote_lib:build_nested_map_op(?FACILITY_TREATMENTS,?NESTED_MAP,FacilityTreatmentKey,ListOps),
  [FacilityTreatmentsOp]. 

add_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs) ->
  PrescriptionIdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(PrescriptionId)),
  PatientIdOp = antidote_lib:build_map_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PrescriberIdOp = antidote_lib:build_map_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  PharmacyIdOp = antidote_lib:build_map_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,antidote_lib:counter_increment(PharmacyId)),
  DateStartedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PatientIdOp,PrescriberIdOp,PharmacyIdOp,DateStartedOp,DrugsOp],
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  PatientPrescriptionsOp = antidote_lib:build_nested_map_op(?FACILITY_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,ListOps),
  [PatientPrescriptionsOp].