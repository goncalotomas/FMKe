%% This module represents the patient entity in the FMK system.
%% Patients are associated with prescriptions, treatments and events.
-module(patient).
-include("fmk.hrl").


%% Functions to handle single patient objects
-export ([
  new/3,
  update_details/2,
  name/1,
  id/1,
  address/1,
  treatments/1,
  prescriptions/1,
  events/1,
  add_treatment/4,
  add_treatment/5,
  add_prescription/6,
  add_event/5,
  process_prescription/2,
  add_prescription_drugs/2
  ]).

%% Returns a list of operations ready to be inserted into antidote.
%% All Ids must be of type pos_integer() and name and address should be binary()
-spec new(id(),binary(),binary()) -> [map_field_update()].
new(Id,Name,Address) ->
  IdOp = build_id_op(?PATIENT_ID,?PATIENT_ID_CRDT,Id),
  NameOp = build_lwwreg_op(?PATIENT_NAME,?PATIENT_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT,Address),
  [IdOp,NameOp,AddressOp].

%% Returns a list of operations ready to be inserted into antidote, with the purpose
%% of updating a specific pharmacy's details.
-spec update_details(binary(),binary()) -> [map_field_update()].
update_details(Name,Address) ->
  NameOp = build_lwwreg_op(?PATIENT_NAME,?PATIENT_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT,Address),
  [NameOp,AddressOp].

%% Returns the patient name as a list from a patient object
-spec name(crdt()) -> binary().
name(Patient) ->
  antidote_lib:find_key(Patient,?PATIENT_NAME,?PATIENT_NAME_CRDT).

%% Returns the patient id as an integer from a patient object
-spec id(crdt()) -> id().
id(Patient) ->
  antidote_lib:find_key(Patient,?PATIENT_ID,?PATIENT_ID_CRDT).

%% Returns the patient address as a list from a patient object
-spec address(crdt()) -> binary().
address(Patient) ->
  antidote_lib:find_key(Patient,?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT).

%% Returns the patient treatments as a Riak map from a patient object
-spec treatments(crdt()) -> term().
treatments(Patient) ->
  antidote_lib:find_key(Patient,?PATIENT_TREATMENTS,?PATIENT_TREATMENTS_CRDT).

%% Returns the patient prescriptions as a Riak map from a patient object
-spec prescriptions(crdt()) -> term().
prescriptions(Patient) ->
  antidote_lib:find_key(Patient,?PATIENT_PRESCRIPTIONS,?PATIENT_PRESCRIPTIONS_CRDT).

%% Returns the patient events as a Riak map from a patient object
-spec events(crdt()) -> term().
events(Patient) ->
  antidote_lib:find_key(Patient,?PATIENT_EVENTS,?PATIENT_EVENTS_CRDT).

%% Returns an update operation for adding a treatment to a specific patient.
-spec add_treatment(id(), id(), id(), binary()) -> [map_field_update()].
add_treatment(TreatmentId, PrescriberId, FacilityId, DateStarted) ->
  %% nested treatment operations
  TreatmentIdOp = build_id_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,TreatmentId),
  PrescriberIdOp = build_id_op(?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT,PrescriberId),
  FacilityIdOp = build_id_op(?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT,FacilityId),
  DateStartedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DateStarted),
  ListOps = [TreatmentIdOp,PrescriberIdOp,FacilityIdOp,DateStartedOp],
  %% now to insert the nested operations inside the treatments map
  PatientTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  %% return a top level patient update that contains the treatment map update
  PatientTreatmentsOp = antidote_lib:build_nested_map_op(?PATIENT_TREATMENTS,?NESTED_MAP,PatientTreatmentKey,ListOps),
  [PatientTreatmentsOp].

%% Same as add_treatment/4, but includes an ending date for the treatment.
-spec add_treatment(id(), id(), id(), binary(), binary()) -> [map_field_update()].
add_treatment(TreatmentId, PrescriberId, FacilityId, DateStarted, DateEnded) ->
  %% nested treatment operations
  TreatmentIdOp = build_id_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,TreatmentId),
  PrescriberIdOp = build_id_op(?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT,PrescriberId),
  FacilityIdOp = build_id_op(?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT,FacilityId),
  DateStartedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DateStarted),
  DateEndedOp = build_lwwreg_op(?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT,DateEnded),
  ListOps = [TreatmentIdOp,PrescriberIdOp,FacilityIdOp,DateStartedOp,DateEndedOp],
  %% now to insert the nested operations inside the treatments map
  PatientTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  %% return a top level patient update that contains the treatment map update
  PatientTreatmentsOp = antidote_lib:build_nested_map_op(?PATIENT_TREATMENTS,?NESTED_MAP,PatientTreatmentKey,ListOps),
  [PatientTreatmentsOp].

%% Returns an update operation for adding a prescription to a specific patient.
-spec add_prescription(id(), id(), id(), id(), binary(), [crdt()]) -> [map_field_update()].
add_prescription(PrescriptionId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  %% nested prescription operations
  PrescriptionIdOp = build_id_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,PrescriptionId),
  PrescriberIdOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,PrescriberId),
  PharmacyIdOp = build_id_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,PharmacyId),
  FacilityIdOp = build_id_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,FacilityId),
  DateStartedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,DatePrescribed),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PrescriberIdOp,PharmacyIdOp,FacilityIdOp,DateStartedOp,DrugsOp],
  %% now to insert the nested operations inside the prescriptions map
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  PatientPrescriptionsOp = antidote_lib:build_nested_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,ListOps),
  [PatientPrescriptionsOp].

-spec process_prescription(id(), binary()) -> [map_field_update()].
process_prescription(PrescriptionId, CurrentDate) ->
  PrescriptionUpdate = prescription:process(CurrentDate),
  %% now to insert the nested operations inside the prescriptions map
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  PatientPrescriptionsOp = antidote_lib:build_nested_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,PrescriptionUpdate),
  [PatientPrescriptionsOp].

-spec add_prescription_drugs(id(), [binary()]) -> [map_field_update()].
add_prescription_drugs(PrescriptionId, Drugs) ->
  PrescriptionUpdate = prescription:add_drugs(Drugs),
  %% now to insert the nested operations inside the prescriptions map
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  PatientPrescriptionsOp = antidote_lib:build_nested_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,PrescriptionUpdate),
  [PatientPrescriptionsOp].

% Returns an update operation for adding an event to a specific patient.
-spec add_event(id(), id(), id(), binary(), binary()) -> [map_field_update()].
add_event(TreatmentId,EventId,StaffMemberId,Timestamp,Description) ->
  %% nested event operations
  EventIdOp = build_id_op(?EVENT_ID,?EVENT_ID_CRDT,EventId),
  PrescriberIdOp = build_id_op(?EVENT_STAFF_ID,?EVENT_STAFF_ID_CRDT,StaffMemberId),
  TimestampOp = build_lwwreg_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,Timestamp),
  DescriptionOp = build_lwwreg_op(?EVENT_DESCRIPTION,?EVENT_DESCRIPTION_CRDT,Description),
  ListOps = [EventIdOp,PrescriberIdOp,TimestampOp,DescriptionOp],
  %% now to insert the nested operations inside the events map
  PatientTreatmentsKey = fmk_core:binary_treatment_key(TreatmentId),
  %% return a top level patient update that contains the events map update
  PatientEventsOp = antidote_lib:build_nested_map_op(?TREATMENT_EVENTS,?NESTED_MAP,PatientTreatmentsKey,ListOps),
  [PatientEventsOp].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key,KeyType,Id) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:counter_increment(Id)).

build_lwwreg_op(Key,KeyType,Value) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:lwwreg_assign(Value)).
