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
    add_prescription/1,
    add_event/1
	]).

new(Id,PatientId,PrescriberId,FacilityId,DatePrescribed) ->
  IdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientIdOp = antidote_lib:build_map_op(?TREATMENT_PATIENT_ID,?TREATMENT_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PrescriberIdOp = antidote_lib:build_map_op(?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  FacilityIdOp = antidote_lib:build_map_op(?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  DatePrescribedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  HasEndedOp = antidote_lib:build_map_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,antidote_lib:lwwreg_assign(<<"0">>)),
  [IdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DatePrescribedOp,HasEndedOp].

new(Id,PatientId,PrescriberId,FacilityId,DatePrescribed,DateEnded) ->
  IdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientIdOp = antidote_lib:build_map_op(?TREATMENT_PATIENT_ID,?TREATMENT_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PrescriberIdOp = antidote_lib:build_map_op(?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  FacilityIdOp = antidote_lib:build_map_op(?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  DatePrescribedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  DateEndedOp = antidote_lib:build_map_op(?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateEnded))),
  HasEndedOp = antidote_lib:build_map_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,antidote_lib:lwwreg_assign(<<"1">>)),
  [IdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DatePrescribedOp,DateEndedOp,HasEndedOp].

prescriber_id(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_PRESCRIBER_ID,?TREATMENT_PRESCRIBER_ID_CRDT) of
    not_found -> 0;
    PrescriberId -> PrescriberId
  end.

id(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_ID,?TREATMENT_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

patient_id(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_PATIENT_ID,?TREATMENT_PATIENT_ID_CRDT) of
    not_found -> 0;
    PatientId -> PatientId
  end.

date_prescribed(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT) of
    not_found -> "";
    Date -> binary_to_list(Date)
  end.

date_ended(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT) of
    not_found -> ongoing_treatment;
    Date -> binary_to_list(Date)
  end.

prescriptions(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_PRESCRIPTIONS,?TREATMENT_PRESCRIPTIONS_CRDT) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

events(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_EVENTS,?TREATMENT_EVENTS_CRDT) of
    not_found -> [];
    Events -> Events
  end.

has_ended(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT) of
    not_found -> unknown;
    <<"0">> -> no;
    <<"1">> -> yes
  end.

facility_id(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_FACILITY_ID,?TREATMENT_FACILITY_ID_CRDT) of
    not_found -> 0;
    FacilityId -> FacilityId
  end.

finish(CurrentDate) ->
  IsProcessedOp = antidote_lib:build_map_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,antidote_lib:lwwreg_assign(<<"1">>)),
  ProcessedOp = antidote_lib:build_map_op(?TREATMENT_DATE_ENDED,?TREATMENT_DATE_ENDED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(CurrentDate))),
  [IsProcessedOp,ProcessedOp].

add_prescription(Prescription) ->
  Id = prescription:id(Prescription),
  Drugs = prescription:drugs(Prescription),
  %% Make operations to insert a new nested map
  IdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(Id)),
  [DrugsOp] = prescription:add_drugs(Drugs),
  [IdOp,DrugsOp].

add_event(Event) ->
  Id = event:id(Event),
  Description = event:description(Event),
  Timestamp = event:timestamp(Event),
  %% Make operations to insert a new nested map
  IdOp = antidote_lib:build_map_op(?EVENT_ID,?EVENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  DescriptionOp = antidote_lib:build_map_op(?EVENT_DESCRIPTION,?EVENT_DESCRIPTION_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Description))),
  TimestampOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Timestamp))),
  [IdOp,DescriptionOp,TimestampOp].