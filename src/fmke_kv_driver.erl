%%%-------------------------------------------------------------------
%%% @author goncalotomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2017 15:27
%%%-------------------------------------------------------------------
-module(fmke_kv_driver).
-include("fmk.hrl").
-include("fmk_kv.hrl").
-author("goncalotomas").

-behaviour(gen_fmke_driver).

-export([
  start_transaction/1,
  commit_transaction/1,
  create_patient/4,
  create_pharmacy/4,
  create_facility/5,
  create_staff/5,
  create_prescription/7,
  create_prescription/8,
  create_event/5,
  create_treatment/5,
  create_treatment/6,
  get_event_by_id/1,
  get_facility_by_id/1,
  get_facility_treatments/1,
  get_patient_by_id/2,
  get_pharmacy_by_id/2,
  get_processed_pharmacy_prescriptions/2,
  get_pharmacy_prescriptions/2,
  get_prescription_by_id/2,
  get_prescription_medication/2,
  get_staff_by_id/2,
  get_staff_prescriptions/2,
  get_staff_treatments/1,
  get_treatment_by_id/1,
  process_prescription/2,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  update_staff_details/4,
  update_prescription_medication/3
]).

start_transaction(Context) ->
  ?KV_IMPLEMENTATION:start_transaction(Context).

commit_transaction(Context) ->
  ?KV_IMPLEMENTATION:commit_transaction(Context).

create_patient(Context, Id, Name, Address) ->
  Result = case get_patient_by_id(Context,Id) of
             {{error,not_found},Context1} ->
               PatientKey = gen_patient_key(Id),
               %% get will have to return an erlang record for a known entity
               PatientUpdate = [
                 create_register_op(?PATIENT_ID_KEY,Id),
                 create_register_op(?PATIENT_NAME_KEY,Name),
                 create_register_op(?PATIENT_ADDRESS_KEY,Address)
               ],
               {ok, _Context2} = ?KV_IMPLEMENTATION:update_map(PatientKey,PatientUpdate,Context1);
               %{ok, _Context2} = ?KV_IMPLEMENTATION:put(PatientKey,#patient{id=Id,name=Name,address=Address},Context1);
             {{ok, _Patient}, Context3} ->
               {{error, patient_id_taken}, Context3}
           end,
  Result.

create_pharmacy(Context, Id, Name, Address) ->
  Result = case get_pharmacy_by_id(Context,Id) of
             {{error,not_found},Context1} ->
               PatientKey = gen_patient_key(Id),
               %% get will have to return an erlang record for a known entity
               {ok, _Context2} = ?KV_IMPLEMENTATION:put(PatientKey,{id=Id,name=Name,address=Address},Context1);
             {{ok, _Patient}, Context3} ->
               {{error, patient_id_taken}, Context3}
           end,
  Result.

create_facility(_Context, _Id, _Name, _Address, _Type) ->
  erlang:error(not_implemented).

create_staff(_Context,_Id, _Name, _Address, _Speciality) ->
  erlang:error(not_implemented).

create_prescription(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6) ->
  erlang:error(not_implemented).

create_prescription(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5, _Arg6, _Arg7) ->
  erlang:error(not_implemented).

create_event(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4) ->
  erlang:error(not_implemented).

create_treatment(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4) ->
  erlang:error(not_implemented).

create_treatment(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4, _Arg5) ->
  erlang:error(not_implemented).

get_event_by_id(_Arg0) ->
  erlang:error(not_implemented).

get_facility_by_id(_Arg0) ->
  erlang:error(not_implemented).

get_facility_treatments(_Arg0) ->
  erlang:error(not_implemented).

get_patient_by_id(Context, Id) ->
  ?KV_IMPLEMENTATION:get_key(gen_patient_key(Id),patient,Context).

get_pharmacy_by_id(Context, Id) ->
  ?KV_IMPLEMENTATION:get_key(gen_pharmacy_key(Id),pharmacy,Context).

get_processed_pharmacy_prescriptions(_Context,_Arg0) ->
  erlang:error(not_implemented).

get_pharmacy_prescriptions(_Context,_Arg0) ->
  erlang:error(not_implemented).

get_prescription_by_id(_Context,_Arg0) ->
  erlang:error(not_implemented).

get_prescription_medication(_Context,_Arg0) ->
  erlang:error(not_implemented).

get_staff_by_id(_Context,_Arg0) ->
  erlang:error(not_implemented).

get_staff_prescriptions(_Context,_Arg0) ->
  erlang:error(not_implemented).

get_staff_treatments(_Arg0) ->
  erlang:error(not_implemented).

get_treatment_by_id(_Arg0) ->
  erlang:error(not_implemented).

process_prescription(_Arg0, _Arg1) ->
  erlang:error(not_implemented).

update_patient_details(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

update_pharmacy_details(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

update_facility_details(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).

update_staff_details(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).

update_prescription_medication(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

gen_key(Entity,Id) ->
  list_to_binary(lists:flatten(io_lib:format("~p_~p",[Entity,Id]))).

gen_patient_key(Id) ->
  gen_key(patient,Id).

gen_pharmacy_key(Id) ->
  gen_key(pharmacy,Id).

create_map_op(Key,NestedOps) ->
  {create_map, Key, NestedOps}.

create_register_op(Key,Value) ->
  {create_register, Key, Value}.
