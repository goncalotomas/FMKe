-module(patient).
-include("fmk.hrl").

-export ([
  create_patient/3,
  update_patient/2,
  full_name/1,
  patient_id/1,
  address/1,
  treatments/1,
  prescriptions/1,
  events/1
  ]).

create_patient(Id,Name,Address) ->
  IdOp = {update,{patient_id, riak_dt_gcounter},{increment, Id}},
  NameOp = {update,{patient_name, riak_dt_lwwreg},{assign, list_to_binary(Name)}},
  AddressOp = {update,{patient_address, riak_dt_lwwreg},{assign, list_to_binary(Address)}},
  EventsOp = {update,{patient_events, riak_dt_map},{update,{num_events,riak_dt_pncounter},{increment,0}}},
  _TreatmentsOp = {update,{key, riak_dt_lwwreg},{assign, <<"patient_treatments">>}},
  _PrescriptionsKey = {update,{key, riak_dt_lwwreg},{assign, <<"patient_prescriptions">>}},
  OpList = [IdOp,NameOp,AddressOp,EventsOp],
  {ok,_Something} = antidote_lib:write_to_antidote(list_to_atom("patient_"+Id),riak_dt_map,{update,OpList}). %% TODO SWITCH THIS TO THE NEW API

update_patient(Key,PatientUpdate) ->
  antidote_lib:write_to_antidote(Key,riak_dt_map,{PatientUpdate}).

% create_patient_bucket(PatientId) ->
%   antidote_lib:create_bucket(PatientId,riak_dt_map).

% update_patient(PatientObject) ->
%   Txn = antidote_lib:start_txn(),
%   ok = antidote_lib:write_object(PatientObject,Txn),
%   ok = antidote_lib:commit_txn(Txn).

% read_patient(PatientId) ->
%   Txn = antidote_lib:start_txn(),
%   PatientBucket = create_patient_bucket(PatientId),
%   Value = antidote_lib:read_object(PatientBucket,Txn),
%   _CommitTime = antidote_lib:commit_txn(Txn),
%   Value.

full_name(_PatientObject) ->
  "Bilbo".

patient_id(_PatientObject) ->
  1.

address(_PatientObject) ->
  "Sessame Street".

treatments(_PatientObject) ->
  [pet_kittens].

prescriptions(_PatientObject) ->
  [pet_kittens].

events(_PatientObject) ->
  [kitten_was_pet].