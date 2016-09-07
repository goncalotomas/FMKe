-module (treatment).
-include("fmk.hrl").

-export ([
    new/5,
    has_ended/1,
    date_prescribed/1,
    date_ended/1,
    finish/1,
    id/1,
    prescriber/1,
    patient/1,
    events/1,
    prescriptions/1,
    add_prescription/1,
    add_event/1
	]).

%% nested treatment
new(Id,PatientName,PrescriberName,FacilityName,DatePrescribed) ->
  IdOp = antidote_lib:build_map_op(?TREATMENT_ID,?TREATMENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientOp = antidote_lib:build_map_op(?TREATMENT_PATIENT_NAME,?TREATMENT_PATIENT_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(PatientName))),
  PrescriberOp = antidote_lib:build_map_op(?TREATMENT_PRESCRIBER_NAME,?TREATMENT_PRESCRIBER_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(PrescriberName))),
  FacilityNameOp = antidote_lib:build_map_op(?TREATMENT_FACILITY_NAME,?TREATMENT_FACILITY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(FacilityName))),
  DatePrescribedOp = antidote_lib:build_map_op(?TREATMENT_DATE_PRESCRIBED,?TREATMENT_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  HasEndedOp = antidote_lib:build_map_op(?TREATMENT_HAS_ENDED,?TREATMENT_HAS_ENDED_CRDT,antidote_lib:lwwreg_assign(<<"0">>)),
  [IdOp,PatientOp,PrescriberOp,FacilityNameOp,DatePrescribedOp,HasEndedOp].

prescriber(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_PRESCRIBER_NAME,?TREATMENT_PRESCRIBER_NAME_CRDT) of
    not_found -> "";
    PrescriberName -> binary_to_list(PrescriberName)
  end.

id(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_ID,?TREATMENT_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

patient(Treatment) ->
  case antidote_lib:find_key(Treatment,?TREATMENT_PATIENT_NAME,?TREATMENT_PATIENT_NAME_CRDT) of
    not_found -> "";
    Patient -> binary_to_list(Patient)
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