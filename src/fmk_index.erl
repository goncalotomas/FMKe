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

index_patient(Key,Name) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation).

index_facility(Key,Name) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_FACILITY_NAME_INDEX,?ORSET,add,AddOperation).

index_pharmacy(Key,Name) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_PHARMACY_NAME_INDEX,?ORSET,add,AddOperation).

index_staff(Key,Name) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_STAFF_NAME_INDEX,?ORSET,add,AddOperation).

reindex_patient(Key,OldName,NewName) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_PATIENT_NAME_INDEX,RemoveOperation,AddOperation).

reindex_pharmacy(Key,OldName,NewName) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_PHARMACY_NAME_INDEX,RemoveOperation,AddOperation).

reindex_facility(Key,OldName,NewName) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_FACILITY_NAME_INDEX,RemoveOperation,AddOperation).

reindex_staff(Key,OldName,NewName) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_STAFF_NAME_INDEX,RemoveOperation,AddOperation).

reindex_orset(Key,RemoveOperation,AddOperation) ->
  ok = antidote_lib:put(Key,?ORSET,remove,RemoveOperation),
  ok = antidote_lib:put(Key,?ORSET,add,AddOperation).

build_binary_tuple(List1,List2) ->
  {list_to_binary(List1),list_to_binary(List2)}.
