-module(fmk_core).
-include("fmk.hrl").

-export([
  get_patient/1,
  update_patient_details/3,
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
  case get_patient(Id) of
    {error,not_found} ->
      Patient = patient:new(Id,Name,Address),
      PatientKey = concatenate_patient_id(Id),
      ok = index_patient(Id,Name),
      ok = antidote_lib:put(PatientKey,?MAP,update,Patient,fmk);
    _Patient ->
      {error, patient_id_taken}
  end.

update_patient_details(Id,Name,Address) ->
  case get_patient(Id) of
    {error,not_found} ->
      {error,no_such_patient};
    Patient ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      PatientKey = concatenate_patient_id(Id),
      PatientName = patient:name(Patient),
      PatientUpdate = patient:update_personal_details(Name,Address),
      antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,fmk),
      case string:equal(PatientName,Name) of
        true ->
          ok;
        false ->
          ok = reindex_patient(Id,PatientName,Name)
      end
  end.

%create_patient_stub(Id,Name,Address) ->
%  patient:new(Id,Name,Address).
  
update_patient(PatientObject) ->
  Txn = antidote_lib:start_txn(),
  ok = antidote_lib:write_object(PatientObject,Txn),
  ok = antidote_lib:commit_txn(Txn).

add_prescription(PatientId) ->
  Patient = get_patient(PatientId),
  PatientId = antidote_lib:find_key(Patient,?PATIENT_ID,?PATIENT_ID_CRDT).
  %% TODO complete

index_patient(Id,Name) ->
  AddOperation = {list_to_binary(Name),concatenate_patient_id(Id)},
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation).

reindex_patient(Id,OldName,NewName) ->
  PatientKey = concatenate_patient_id(Id),
  RemoveOperation = {list_to_binary(OldName),PatientKey},
  AddOperation = {list_to_binary(NewName),PatientKey},
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,remove,RemoveOperation),
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation).

concatenate_patient_id(Id) ->
  list_to_binary(lists:flatten(io_lib:format("patient~p", [Id]))).