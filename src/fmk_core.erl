-module(fmk_core).
-include("fmk.hrl").

-export([
  create_patient_bucket/1,
  update_patient/1,
  read_patient/1
  ]).

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



