-module(event).
-include("fmk.hrl").

%% Functions to handle single Pharmacy objects
-export ([
  new/5,
  patient/1,
  id/1,
  timestamp/1,
  staff_member/1,
  description/1
  ]).

new(Id,PatientName,StaffMemberName,Timestamp,Description) ->
  IdOp = antidote_lib:build_map_op(?EVENT_ID,?EVENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientNameOp = antidote_lib:build_map_op(?EVENT_STAFF_MEMBER_NAME,?EVENT_STAFF_MEMBER_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(PatientName))),
  StaffMemberNameOp = antidote_lib:build_map_op(?EVENT_STAFF_MEMBER_NAME,?EVENT_STAFF_MEMBER_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(StaffMemberName))),
  TimestampOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Timestamp))),
  DescriptionOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Description))),
  [IdOp,PatientNameOp,StaffMemberNameOp,TimestampOp,DescriptionOp].

patient(Event) ->
  case antidote_lib:find_key(Event,?EVENT_PATIENT_NAME,?EVENT_PATIENT_NAME_CRDT) of
    not_found -> "";
    Patient -> binary_to_list(Patient)
  end.

id(Event) ->
  case antidote_lib:find_key(Event,?EVENT_ID,?EVENT_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

timestamp(Event) ->
  case antidote_lib:find_key(Event,?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT) of
    not_found -> 0;
    Timestamp -> Timestamp
  end.

staff_member(Event) ->
  case antidote_lib:find_key(Event,?EVENT_STAFF_MEMBER_NAME,?EVENT_STAFF_MEMBER_NAME_CRDT) of
    not_found -> "";
    PrescriberName -> binary_to_list(PrescriberName)
  end.

description(Event) ->
  case antidote_lib:find_key(Event,?EVENT_ID,?EVENT_ID_CRDT) of
    not_found -> "";
    Description -> Description
  end.

