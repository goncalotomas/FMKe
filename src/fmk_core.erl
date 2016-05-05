-module(fmk_core).
-include("fmk.hrl").

-export([
  find_patient/1,
  create_patient_bucket/1,
  update_patient/1,
  read_patient/1
  ]).

%% Finds a patient in the Antidote Key-Value store by Patient ID.
-spec findpatient(Id::pos_integer()) -> riak_dt_map:map().
findpatient(Id) ->
  Patients = antidote_lib:read_from_antidote(?FMK_PATIENTS,riak_dt_map),
  case lists:keyfind({Id,riak_dt_map},1,Patients) of
    false -> not_found;
    {{Id,riak_dt_map},Patient} -> Patient
  end.

create_patient_bucket(PatientId) ->
  antidote_lib:create_object(PatientId,riak_dt_map).

update_patient(PatientObject) ->
  Txn = antidote_lib:start_txn(),
  ok = antidote_lib:write_object(PatientObject,Txn),
  ok = antidote_lib:commit_txn(Txn).

read_patient(PatientId) ->
  Txn = antidote_lib:start_txn(),
  PatientObject = create_patient_bucket(PatientId),
  ok = antidote_lib:read_object(PatientObject,Txn),
  ok = antidote_lib:commit_txn(Txn).