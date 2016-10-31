%% This module represents the faclity entity in the FMK system.
%% Facilities are associated with prescriptions and treatments.
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
  add_event/5,
  add_treatment/4,
  add_treatment/5,
  process_prescription/2,
  add_prescription_drugs/2
  ]).

%% Returns a list of operations ready to be inserted into antidote.
%% All Ids must be of type pos_integer() and name, address and type should be binary()
-spec new(id(),binary(),binary(),binary()) -> [map_field_update()].
new(Id,Name,Address,Type) ->
  IdOp = build_id_op(?FACILITY_ID,?FACILITY_ID_CRDT,Id),
  NameOp = build_lwwreg_op(?FACILITY_NAME,?FACILITY_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT,Address),
  TypeOp = build_lwwreg_op(?FACILITY_TYPE,?FACILITY_TYPE_CRDT,Type),
  [IdOp,NameOp,AddressOp,TypeOp].

%% Returns a list of operations ready to be inserted into antidote, with the purpose
%% of updating a specific facility's details.
-spec update_details(binary(),binary(),binary()) -> [map_field_update()].
update_details(Name,Address,Type) ->
  NameOp = build_lwwreg_op(?FACILITY_NAME,?FACILITY_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT,Address),
  TypeOp = build_lwwreg_op(?FACILITY_TYPE,?FACILITY_TYPE_CRDT,Type),
  [NameOp,AddressOp,TypeOp].

%% Returns the facility name as from a facility object
-spec name(crdt()) -> binary().
name(Facility) ->
  antidote_lib:find_key(Facility,?FACILITY_NAME,?FACILITY_NAME_CRDT).

%% Returns the facility type from a facility object
-spec type(crdt()) -> binary().
type(Facility) ->
  antidote_lib:find_key(Facility,?FACILITY_TYPE,?FACILITY_TYPE_CRDT).

%% Returns the facility id from a facility object
-spec id(crdt()) -> id().
id(Facility) ->
  antidote_lib:find_key(Facility,?FACILITY_ID,?FACILITY_ID_CRDT).

%% Returns the facility address from a facility object
-spec address(crdt()) -> binary().
address(Facility) ->
  antidote_lib:find_key(Facility,?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT).

%% Returns the facility prescriptions from a facility object
-spec prescriptions(crdt()) -> term().
prescriptions(Facility) ->
  antidote_lib:find_key(Facility,?FACILITY_PRESCRIPTIONS,?FACILITY_PRESCRIPTIONS_CRDT).

%% Returns the facility treatments from a facility object
-spec treatments(crdt()) -> term().
treatments(Facility) ->
  antidote_lib:find_key(Facility,?FACILITY_TREATMENTS,?FACILITY_TREATMENTS_CRDT).

%% Returns an update operation for adding a treatment to a specific facility.
-spec add_treatment(id(), id(), id(), binary()) -> [map_field_update()].
add_treatment(TreatmentId, PatientId, PrescriberId, DateStarted) ->
  %% nested treatment operations
  TreatmentIdOp = build_id_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,TreatmentId),
  PatientIdOp = build_id_op(?PATIENT_ID,?PATIENT_ID_CRDT,PatientId),
  PrescriberIdOp = build_id_op(?STAFF_ID,?STAFF_ID_CRDT,PrescriberId),
  DateStartedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DateStarted),
  ListOps = [TreatmentIdOp,PrescriberIdOp,PatientIdOp,DateStartedOp],
  %% now to insert the nested operations inside the treatments map
  FacilityTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  %% return a top level facility update that contains the treatment map update
  FacilityTreatmentsOp = antidote_lib:build_nested_map_op(?FACILITY_TREATMENTS,?NESTED_MAP,FacilityTreatmentKey,ListOps),
  [FacilityTreatmentsOp].

%% Same as add_treatment/4, but includes an ending date for the treatment.
-spec add_treatment(id(), id(), id(), binary(), binary()) -> [map_field_update()].
add_treatment(TreatmentId, PatientId, PrescriberId, DateStarted, DateEnded) ->
  %% nested treatment operations
  TreatmentIdOp = build_id_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,TreatmentId),
  PatientIdOp = build_id_op(?PATIENT_ID,?PATIENT_ID_CRDT,PatientId),
  PrescriberIdOp = build_id_op(?STAFF_ID,?STAFF_ID_CRDT,PrescriberId),
  DateStartedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DateStarted),
  DateEndedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DateEnded),
  ListOps = [TreatmentIdOp,PrescriberIdOp,PatientIdOp,DateStartedOp,DateEndedOp],
  %% now to insert the nested operations inside the treatments map
  FacilityTreatmentKey = fmk_core:binary_treatment_key(TreatmentId),
  %% return a top level facility update that contains the treatment map update
  FacilityTreatmentsOp = antidote_lib:build_nested_map_op(?FACILITY_TREATMENTS,?NESTED_MAP,FacilityTreatmentKey,ListOps),
  [FacilityTreatmentsOp].

%% Returns an update operation for adding a prescription to a specific patient.
-spec add_prescription(id(), id(), id(), id(), binary(), [crdt()]) -> [map_field_update()].
add_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs) ->
  %% nested prescription operations
  PrescriptionIdOp = build_id_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,PrescriptionId),
  PatientIdOp = build_id_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,PatientId),
  PrescriberIdOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,PrescriberId),
  PharmacyIdOp = build_id_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,PharmacyId),
  DateStartedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DatePrescribed),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PatientIdOp,PrescriberIdOp,PharmacyIdOp,DateStartedOp,DrugsOp],
  %% now to insert the nested operations inside the prescriptions map
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level facility update that contains the prescriptions map update
  PatientPrescriptionsOp = antidote_lib:build_nested_map_op(?FACILITY_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,ListOps),
  [PatientPrescriptionsOp].

-spec process_prescription(id(), binary()) -> [map_field_update()].
process_prescription(PrescriptionId, CurrentDate) ->
  PrescriptionUpdate = prescription:process(CurrentDate),
  %% now to insert the nested operations inside the prescriptions map
  FacilityPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  FacilityPrescriptionsOp = antidote_lib:build_nested_map_op(?FACILITY_PRESCRIPTIONS,?NESTED_MAP,FacilityPrescriptionsKey,PrescriptionUpdate),
  [FacilityPrescriptionsOp].

-spec add_prescription_drugs(id(), [binary()]) -> [map_field_update()].
add_prescription_drugs(PrescriptionId, Drugs) ->
  PrescriptionUpdate = prescription:add_drugs(Drugs),
  %% now to insert the nested operations inside the prescriptions map
  FacilityPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  FacilityPrescriptionsOp = antidote_lib:build_nested_map_op(?FACILITY_PRESCRIPTIONS,?NESTED_MAP,FacilityPrescriptionsKey,PrescriptionUpdate),
  [FacilityPrescriptionsOp].

%% Returns an update operation for adding an event to a specific facility treatment.
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
  PatientEventsOp = antidote_lib:build_nested_map_op(?FACILITY_TREATMENTS,?NESTED_MAP,PatientTreatmentsKey,ListOps),
  [PatientEventsOp].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key,KeyType,Id) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:counter_increment(Id)).

build_lwwreg_op(Key,KeyType,Value) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:lwwreg_assign(Value)).
