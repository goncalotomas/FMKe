-module(fmk_core).
-include("fmk.hrl").

-export([
  get_patient/1,
  create_patient/3,
  update_patient/1,
  add_prescription/1
  ]).

%% Finds a patient in the Antidote Key-Value store by Patient ID.
-spec get_patient(Id::pos_integer()) -> riak_dt_map:map().
get_patient(Id) ->
  Patient = antidote_lib:get(concatenate_patient_id(Id),?MAP),
  case Patient of
    [] -> {error,not_found};
    PatientMap -> PatientMap
  end.
  
create_patient(Id,Name,Address) ->
  %%TODO create name->id index in Antidote
  Patient = patient:new(Id,Name,Address),
  PatientKey = concatenate_patient_id(Id),
  ok = antidote_lib:put(PatientKey,?MAP,Patient,fmk).

update_patient(PatientObject) ->
  Txn = antidote_lib:start_txn(),
  ok = antidote_lib:write_object(PatientObject,Txn),
  ok = antidote_lib:commit_txn(Txn).

add_prescription(PatientId) ->
  Patient = get_patient(PatientId),
  PatientId = antidote_lib:find_key(Patient,?PATIENT_ID,?PATIENT_ID_CRDT).
  %% TODO complete

concatenate_patient_id(Id) ->
  list_to_atom(lists:flatten(io_lib:format("patient~p", [Id]))).