-module(event).
-include("fmk.hrl").

%% Functions to handle single Pharmacy objects
-export ([
  new/5,
  patient_id/1,
  id/1,
  timestamp/1,
  staff_member_id/1,
  description/1
  ]).

new(Id,PatientId,StaffMemberId,Timestamp,Description) ->
  IdOp = antidote_lib:build_map_op(?EVENT_ID,?EVENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientNameOp = antidote_lib:build_map_op(?EVENT_PATIENT_ID,?EVENT_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  StaffMemberNameOp = antidote_lib:build_map_op(?EVENT_STAFF_MEMBER_ID,?EVENT_STAFF_MEMBER_ID_CRDT,antidote_lib:counter_increment(StaffMemberId)),
  TimestampOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Timestamp))),
  DescriptionOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Description))),
  [IdOp,PatientNameOp,StaffMemberNameOp,TimestampOp,DescriptionOp].

patient_id(Event) ->
  case antidote_lib:find_key(Event,?EVENT_PATIENT_ID,?EVENT_PATIENT_ID_CRDT) of
    not_found -> 0;
    Patient -> binary_to_list(Patient)
  end.

id(Event) ->
  case antidote_lib:find_key(Event,?EVENT_ID,?EVENT_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

timestamp(Event) ->
  case antidote_lib:find_key(Event,?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT) of
    not_found -> "";
    Timestamp -> Timestamp
  end.

staff_member_id(Event) ->
  case antidote_lib:find_key(Event,?EVENT_STAFF_MEMBER_ID,?EVENT_STAFF_MEMBER_ID_CRDT) of
    not_found -> 0;
    PrescriberName -> binary_to_list(PrescriberName)
  end.

description(Event) ->
  case antidote_lib:find_key(Event,?EVENT_ID,?EVENT_ID_CRDT) of
    not_found -> "";
    Description -> Description
  end.

