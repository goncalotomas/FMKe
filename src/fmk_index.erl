-module (fmk_index).
-include("fmk.hrl").

%% Useful functions for this module
-export ([
    get_patient_name_index/0,
    get_pharmacy_name_index/0,
    get_facility_name_index/0,
    get_staff_name_index/0,
    get_patient_name_index/1,
    get_pharmacy_name_index/1,
    get_facility_name_index/1,
    get_staff_name_index/1,
    is_indexed/2,
    search_index/2
  ]).

%% Function exports needed for other modules
-export ([
    index_patient/3,
    index_pharmacy/3,
    index_facility/3,
    index_staff/3,
    reindex_patient/4,
    reindex_pharmacy/4,
    reindex_facility/4,
    reindex_staff/4,
    reindex_orset/4
  ]).

get_patient_name_index() ->
  antidote_lib:get(?FMK_PATIENT_NAME_INDEX,?ORSET).

get_pharmacy_name_index() ->
  antidote_lib:get(?FMK_PHARMACY_NAME_INDEX,?ORSET).

get_facility_name_index() ->
  antidote_lib:get(?FMK_FACILITY_NAME_INDEX,?ORSET).

get_staff_name_index() ->
  antidote_lib:get(?FMK_STAFF_NAME_INDEX,?ORSET).

get_patient_name_index(Txn) ->
  antidote_lib:get(?FMK_PATIENT_NAME_INDEX,?ORSET,Txn).

get_pharmacy_name_index(Txn) ->
  antidote_lib:get(?FMK_PHARMACY_NAME_INDEX,?ORSET,Txn).

get_facility_name_index(Txn) ->
  antidote_lib:get(?FMK_FACILITY_NAME_INDEX,?ORSET,Txn).

get_staff_name_index(Txn) ->
  antidote_lib:get(?FMK_STAFF_NAME_INDEX,?ORSET,Txn).

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

index_patient(Key,Name,Txn) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_PATIENT_NAME_INDEX,?ORSET,add,AddOperation,Txn).

index_facility(Key,Name,Txn) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_FACILITY_NAME_INDEX,?ORSET,add,AddOperation,Txn).

index_pharmacy(Key,Name,Txn) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_PHARMACY_NAME_INDEX,?ORSET,add,AddOperation,Txn).

index_staff(Key,Name,Txn) ->
  AddOperation = build_binary_tuple(Name,Key),
  ok = antidote_lib:put(?FMK_STAFF_NAME_INDEX,?ORSET,add,AddOperation,Txn).

reindex_patient(Key,OldName,NewName,Txn) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_PATIENT_NAME_INDEX,RemoveOperation,AddOperation,Txn).

reindex_pharmacy(Key,OldName,NewName,Txn) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_PHARMACY_NAME_INDEX,RemoveOperation,AddOperation,Txn).

reindex_facility(Key,OldName,NewName,Txn) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_FACILITY_NAME_INDEX,RemoveOperation,AddOperation,Txn).

reindex_staff(Key,OldName,NewName,Txn) ->
  RemoveOperation = build_binary_tuple(OldName,Key),
  AddOperation = build_binary_tuple(NewName,Key),
  reindex_orset(?FMK_STAFF_NAME_INDEX,RemoveOperation,AddOperation,Txn).

reindex_orset(Key,RemoveOperation,AddOperation,Txn) ->
  ok = antidote_lib:put(Key,?ORSET,remove,RemoveOperation,Txn),
  ok = antidote_lib:put(Key,?ORSET,add,AddOperation,Txn).

build_binary_tuple(List1,List2) ->
  {list_to_binary(List1),list_to_binary(List2)}.
