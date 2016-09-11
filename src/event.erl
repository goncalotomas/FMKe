%% This module represents the event entity in the FMK system.
%% Events happen within treatments, which means they are associated
%% to a patient, hospital and medical staff.
-module(event).
-include("fmk.hrl").

-export ([
  new/5,
  patient_id/1,
  id/1,
  timestamp/1,
  staff_id/1,
  description/1
  ]).

%% This function returns a list of operations ready to be inserted into antidote.
%% In order to create an event, multiple ids must be supplied (self-explanatory),
%% as well as a date when the event occured and also a small description.
%% All Ids must be of type pos_integer() and the Timestamp and Description should
%% be supplied in binary
new(Id,PatientId,StaffMemberId,Timestamp,Description) ->
  IdOp = build_id_op(?EVENT_ID,?EVENT_ID_CRDT,Id),
  PatientNameOp = build_id_op(?EVENT_PATIENT_ID,?EVENT_PATIENT_ID_CRDT,PatientId),
  StaffMemberNameOp = build_id_op(?EVENT_STAFF_ID,?EVENT_STAFF_ID_CRDT,StaffMemberId),
  TimestampOp = build_lwwreg_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,Timestamp),
  DescriptionOp = build_lwwreg_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,Description),
  [IdOp,PatientNameOp,StaffMemberNameOp,TimestampOp,DescriptionOp].

%% Returns the patient ID from an already existant event object.
patient_id(Event) ->
  antidote_lib:find_key(Event,?EVENT_PATIENT_ID,?EVENT_PATIENT_ID_CRDT).

%% Returns the event ID from an already existant event object.
id(Event) ->
  antidote_lib:find_key(Event,?EVENT_ID,?EVENT_ID_CRDT).

%% Returns the event timestamp from an already existant event object.
timestamp(Event) ->
  antidote_lib:find_key(Event,?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT).

%% Returns the staff ID from an already existant event object.
staff_id(Event) ->
  antidote_lib:find_key(Event,?EVENT_STAFF_ID,?EVENT_STAFF_ID_CRDT).

%% Returns the description from an already existant event object.
description(Event) ->
  antidote_lib:find_key(Event,?EVENT_ID,?EVENT_ID_CRDT).


%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - just here to make your head stop hurting
%%-----------------------------------------------------------------------------
build_id_op(Key,KeyType,Id) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:counter_increment(Id)).

build_lwwreg_op(Key,KeyType,Value) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:lwwreg_assign(Value)),
  