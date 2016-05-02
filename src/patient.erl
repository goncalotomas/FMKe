-module(patient).
-include("fmk.hrl").

-export ([function/arity]).

create_patient_bucket(PatientId) ->
  antidote_lib:create_bucket(PatientId,riak_dt_map).

update_patient(PatientObject) ->
  Txn = antidote_lib:start_txn(),
  ok = antidote_lib:write_object(PatientObject,Txn),
  ok = antidote_lib:commit_txn(Txn).

read_patient(PatientId) ->
  Txn = antidote_lib:start_txn(),
  PatientBucket = create_patient_bucket(PatientId),
  Value = antidote_lib:read_object(PatientBucket,Txn),
  {ok} = antidote_lib:commit_txn(Txn),
  Value.

full_name(PatientObject) ->
  "Bilbo".

patient_id(PatientObject) ->
  1.

address(PatientObject) ->
  "Sessame Street".

treatments(PatientObject) ->
  [pet_kittens].

prescriptions(PatientObject) ->
  [pet_kittens].

events(PatientObject) ->
  [kitten_was_pet].