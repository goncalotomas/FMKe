%% This module represents the treatment entity in the FMK system.
%% Treatments are associated with patients, treatment facilities, and they can
%% have nested prescriptions and events.
-module (treatment).
-include("fmk.hrl").

-export ([
    new/5,
    new/6,
    has_ended/1,
    date_prescribed/1,
    date_ended/1,
    facility_id/1,
    finish/1,
    id/1,
    prescriber_id/1,
    patient_id/1,
    events/1,
    prescriptions/1,
    add_prescription/7,
    add_event/4
	]).

%% This function returns a list of operations ready to be inserted into antidote.
%% In order to create an event, multiple ids must be supplied (self-explanatory),
%% as well as a date when the treatment was prescribed.
%% All Ids must be of type pos_integer() prescription date (DatePrescribed) should
%% be supplied in binary
-spec new(id(),id(),id(),id(),string()) -> [term()].
new(Id,PatientId,PrescriberId,FacilityId,DatePrescribed) ->
  IdOp = build_id_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,Id),
  PatientIdOp = build_id_op(?TREATMENT_PATIENT_ID,?TREATMENT_PATIENT_ID_CRDT,PatientId),
  PrescriberIdOp = build_id_op(?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT,PrescriberId),
  FacilityIdOp = build_id_op(?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT,FacilityId),
  DatePrescribedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DatePrescribed),
  HasEndedOp = build_lwwreg_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,?TREATMENT_ONGOING),
  [IdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DatePrescribedOp,HasEndedOp].

%% Same as new/5, but includes a date that symbolizes when the treatment was finished.
%% Indentically to new/5, DateEnded should be binary
-spec new(id(),id(),id(),id(),string(), string()) -> [term()].
new(Id,PatientId,PrescriberId,FacilityId,DatePrescribed,DateEnded) ->
  %% trying to keep DRY code by calling new/5 and discarding unnecessary ops
  [IdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DatePrescribedOp,_IncorrectHasEndedOp] =
    new(Id,PatientId,PrescriberId,FacilityId,DatePrescribed),
  %% build missing ops
  DateEndedOp = build_lwwreg_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,DateEnded),
  HasEndedOp = build_lwwreg_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,?TREATMENT_ENDED),
  [IdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DatePrescribedOp,DateEndedOp,HasEndedOp].

%% Returns the prescriber's medical staff ID from an already existant treatment object.
-spec prescriber_id(crdt()) -> id().
prescriber_id(Treatment) ->
  antidote_lib:find_key(Treatment,?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT).

%% Returns the treatment's ID from an already existant treatment object.
-spec id(crdt()) -> id().
id(Treatment) ->
  antidote_lib:find_key(Treatment,?TREATMENT_ID,?TREATMENT_ID_CRDT).

%% Returns the patient's ID from an already existant treatment object.
-spec patient_id(crdt()) -> id().
patient_id(Treatment) ->
  antidote_lib:find_key(Treatment,?TREATMENT_PATIENT_ID,?TREATMENT_PATIENT_ID_CRDT).

%% Returns the facility ID from an already existant treatment object.
-spec facility_id(crdt()) -> id().
facility_id(Treatment) ->
  antidote_lib:find_key(Treatment,?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT).

%% Returns the date of prescription from an already existant treatment object.
-spec date_prescribed(crdt()) -> string().
date_prescribed(Treatment) ->
  binary_to_list(antidote_lib:find_key(Treatment,?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT)).

%% Returns the finishing date of an already existant treatment object.
-spec date_ended(crdt()) -> string().
date_ended(Treatment) ->
  DateEnded = antidote_lib:find_key(Treatment,?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT),
  case DateEnded of
      not_found ->
          "not_found";
      Date ->
          binary_to_list(Date)
  end.

%% Returns the prescriptions associated with an already existant treatment object.
-spec prescriptions(crdt()) -> term().
prescriptions(Treatment) ->
  antidote_lib:find_key(Treatment,?TREATMENT_PRESCRIPTIONS,?TREATMENT_PRESCRIPTIONS_CRDT).

%% Returns the events associated with an already existant treatment object.
-spec events(crdt()) -> term().
events(Treatment) ->
  antidote_lib:find_key(Treatment,?TREATMENT_EVENTS,?TREATMENT_EVENTS_CRDT).

%% Checks if a treatment has ended.
-spec has_ended(crdt()) -> string().
has_ended(Treatment) ->
  Result = antidote_lib:find_key(Treatment,?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT),
  case Result of
      not_found ->
          "not_found";
      HasEnded ->
          binary_to_list(HasEnded)
  end.

%% Returns a list of antidote operations to update a treatment with an ending date,
%% also updating the HAS_ENDED field.
-spec finish(string()) -> [term()].
finish(CurrentDate) ->
  IsProcessedOp = build_lwwreg_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,?TREATMENT_ENDED),
  ProcessedOp = build_lwwreg_op(?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT,CurrentDate),
  [IsProcessedOp,ProcessedOp].

%% Returns a list of antidote operation to add a nested prescription to a treatment.
%% Some IDs are necessary in order to create a prescription which should be self-explanatory.
%% Drugs is supposed to be passed as a simple erlang list containing binary elements, that will in
%% turn be converted into a riak_dt_orset.
-spec add_prescription(id(), id(), id(), id(), id(), string(), [crdt()]) -> [term()].
add_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  PrescriptionIdOp = build_id_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,PrescriptionId),
  PatientIdOp = build_id_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,PatientId),
  PrescriberIdOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,PrescriberId),
  PharmacyIdOp = build_id_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,PharmacyId),
  FacilityIdOp = build_id_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,FacilityId),
  DateStartedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,DatePrescribed),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PatientIdOp,PrescriberIdOp,PharmacyIdOp,FacilityIdOp,DateStartedOp,DrugsOp],
  %% the nested prescription will be indexed by its key.
  PatientPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a list of operations for the top level treatment map that includes all
  %% of the nested operations for the prescription fields.
  [antidote_lib:build_nested_map_op(?TREATMENT_PRESCRIPTIONS,?NESTED_MAP,PatientPrescriptionsKey,ListOps)].

%% Returns a list of antidote operation to add a nested event to a treatment.
%% Some IDs are necessary in order to create a prescription which should be self-explanatory.
%% Timestamp and Description are supposed to be binaries.
-spec add_event(id(), id(), string(), string()) -> [term()].
add_event(EventId,StaffMemberId,Timestamp,Description) ->
  %% nested operations
  EventIdOp = build_id_op(?EVENT_ID,?EVENT_ID_CRDT,EventId),
  PrescriberIdOp = build_id_op(?EVENT_STAFF_ID,?EVENT_STAFF_ID_CRDT,StaffMemberId),
  TimestampOp = build_lwwreg_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,Timestamp),
  DescriptionOp = build_lwwreg_op(?EVENT_DESCRIPTION,?EVENT_DESCRIPTION_CRDT,Description),
  ListOps = [EventIdOp,PrescriberIdOp,TimestampOp,DescriptionOp],
  %% the nested event will be indexed by its key.
  PatientEventKey = fmk_core:binary_event_key(EventId),
  %% return a list of operations for the top level treatment map that includes all
  %% of the nested operations for the event fields.
  [antidote_lib:build_nested_map_op(?TREATMENT_EVENTS,?NESTED_MAP,PatientEventKey,ListOps)].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key,KeyType,Id) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:counter_increment(Id)).

build_lwwreg_op(Key,KeyType,Value) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:lwwreg_assign(Value)).
