-module (fmk_index).
-include("fmk.hrl").

%% Useful functions for this module
-export ([
    get_patient_name_index/0,
    get_pharmacy_name_index/0,
    get_facility_name_index/0,
    get_staff_name_index/0,
    is_indexed/2,
    search_index/2
  ]).

%% Function exports needed for other modules
-export ([
    index_patient/2,
    index_pharmacy/2,
    index_facility/2,
    index_staff/2,
    reindex_patient/3,
    reindex_pharmacy/3,
    reindex_facility/3,
    reindex_staff/3,
    reindex_orset/3
  ]).

get_patient_name_index() ->
  antidote_lib:get(?FMK_PATIENT_NAME_INDEX,?ORSET).

get_pharmacy_name_index() ->
  antidote_lib:get(?FMK_PHARMACY_NAME_INDEX,?ORSET).

get_facility_name_index() ->
  antidote_lib:get(?FMK_FACILITY_NAME_INDEX,?ORSET).

get_staff_name_index() ->
  antidote_lib:get(?FMK_STAFF_NAME_INDEX,?ORSET).

is_indexed(Key,Index) ->
  case search_index(Key,Index) of
    not_found -> false;
    _Result -> true
  end.

search_index(Key,Index) ->
  case proplists:get_all_values(Key,Index) of
    [] -> not_found;
    Results -> Results
  end.

index_patient(Id,Name) ->
  AddOperation = build_binary_tuple(Name,concatenate_patient_id(Id)),
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation).

index_facility(Id,Name) ->
  AddOperation = build_binary_tuple(Name,concatenate_facility_id(Id)),
  ok = antidote_lib:put(?FMK_FACILITY_NAME_INDEX,?ORSET,add,AddOperation).

index_pharmacy(Id,Name) ->
  AddOperation = build_binary_tuple(Name,concatenate_pharmacy_id(Id)),
  ok = antidote_lib:put(?FMK_PHARMACY_NAME_INDEX,?ORSET,add,AddOperation).

index_staff(Id,Name) ->
  AddOperation = build_binary_tuple(Name,concatenate_staff_id(Id)),
  ok = antidote_lib:put(?FMK_STAFF_NAME_INDEX,?ORSET,add,AddOperation).

reindex_patient(Id,OldName,NewName) ->
  PatientKey = concatenate_patient_id(Id),
  RemoveOperation = build_binary_tuple(OldName,PatientKey),
  AddOperation = build_binary_tuple(NewName,PatientKey),
  reindex_orset(?FMK_PATIENT_NAME_INDEX,RemoveOperation,AddOperation).

reindex_pharmacy(Id,OldName,NewName) ->
  PharmacyKey = concatenate_pharmacy_id(Id),
  RemoveOperation = build_binary_tuple(OldName,PharmacyKey),
  AddOperation = build_binary_tuple(NewName,PharmacyKey),
  reindex_orset(?FMK_PHARMACY_NAME_INDEX,RemoveOperation,AddOperation).

reindex_facility(Id,OldName,NewName) ->
  FacilityKey = concatenate_facility_id(Id),
  RemoveOperation = build_binary_tuple(OldName,FacilityKey),
  AddOperation = build_binary_tuple(NewName,FacilityKey),
  reindex_orset(?FMK_FACILITY_NAME_INDEX,RemoveOperation,AddOperation).

reindex_staff(Id,OldName,NewName) ->
  StaffKey = concatenate_staff_id(Id),
  RemoveOperation = build_binary_tuple(OldName,StaffKey),
  AddOperation = build_binary_tuple(NewName,StaffKey),
  reindex_orset(?FMK_STAFF_NAME_INDEX,RemoveOperation,AddOperation).

reindex_orset(Key,RemoveOperation,AddOperation) ->
  ok = antidote_lib:put(Key,?ORSET,remove,RemoveOperation),
  ok = antidote_lib:put(Key,?ORSET,add,AddOperation).

build_binary_tuple(List1,List2) ->
  {list_to_binary(List1),list_to_binary(List2)}.

concatenate_patient_id(Id) ->
  fmk_core:concatenate_id(patient,Id).

concatenate_pharmacy_id(Id) ->
  fmk_core:concatenate_id(pharmacy,Id).

concatenate_facility_id(Id) ->
  fmk_core:concatenate_id(facility,Id).

concatenate_staff_id(Id) ->
  fmk_core:concatenate_id(staff,Id).