-module(fmk_core).
-include("fmk.hrl").

-export([
  get_patient/1,
  update_patient_details/3,
  create_patient/3,
  update_patient/1,
  create_pharmacy/3,
  %update_pharmacy/3,
  create_facility/4,
  %update_facility/3,
  add_prescription/1
  ]).

%%TODO MAKE THIS WORK WORK WORK WORK WORK WORK
create_pharmacy(Id,Name,Address) ->
  Pharmacy = pharmacy:new(Id,Name,Address),
  PharmacyKey = list_to_binary(concatenate_pharmacy_id(Id)),
  ok = index_pharmacy(Id,Name),
  ok = antidote_lib:put(PharmacyKey,?MAP,update,Pharmacy,fmk).

create_facility(Id,Name,Address,Type) ->
  Facility = facility:new(Id,Name,Address,Type),
  FacilityKey = list_to_binary(concatenate_facility_id(Id)),
  ok = index_facility(Id,Name),
  ok = antidote_lib:put(FacilityKey,?MAP,update,Facility,fmk).

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
      PatientKey = list_to_binary(concatenate_patient_id(Id)),
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
  AddOperation = build_binary_tuple(Name,concatenate_patient_id(Id)),
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation).

index_facility(Id,Name) ->
  AddOperation = build_binary_tuple(Name,concatenate_facility_id(Id)),
  ok = antidote_lib:put(?FMK_FACILITY_NAME_INDEX,?ORSET,add,AddOperation).

index_pharmacy(Id,Name) ->
  AddOperation = build_binary_tuple(Name,concatenate_pharmacy_id(Id)),
  ok = antidote_lib:put(?FMK_PHARMACY_NAME_INDEX,?ORSET,add,AddOperation).

reindex_patient(Id,OldName,NewName) ->
  PatientKey = concatenate_patient_id(Id),
  RemoveOperation = {list_to_binary(OldName),PatientKey},
  AddOperation = {list_to_binary(NewName),PatientKey},
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,remove,RemoveOperation),
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation).

concatenate_patient_id(Id) ->
  lists:flatten(io_lib:format("patient~p", [Id])).

concatenate_pharmacy_id(Id) ->
  lists:flatten(io_lib:format("pharmacy~p", [Id])).

concatenate_facility_id(Id) ->
  lists:flatten(io_lib:format("facility~p", [Id])).

build_binary_tuple(List1,List2) ->
  {list_to_binary(List1),list_to_binary(List2)}.