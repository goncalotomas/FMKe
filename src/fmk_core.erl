-module(fmk_core).
-include("fmk.hrl").

-export([
  get_patient/1,
  create_patient/3,
  update_patient/1,
  get_patients/0
  ]).

%% Finds a patient in the Antidote Key-Value store by Patient ID.
-spec get_patient(Id::pos_integer()) -> riak_dt_map:map().
get_patient(Id) ->
  Patients = antidote_lib:get(?FMK_PATIENTS,?MAP),
  case Patients of
    [] -> no_patients;
    ListPatients -> 
      case antidote_lib:find_key(ListPatients,Id,?NESTED_MAP) of
        not_found -> {error,not_found};
        Patient -> Patient
      end
  end.
  

create_patient(Id,Name,Address) ->
  Patient = patient:new(Id,Name,Address),
  ok = antidote_lib:put(?FMK_PATIENTS,?MAP,Patient,fmk).

update_patient(PatientObject) ->
  Txn = antidote_lib:start_txn(),
  ok = antidote_lib:write_object(PatientObject,Txn),
  ok = antidote_lib:commit_txn(Txn).

get_patients() ->
  antidote_lib:get(?FMK_PATIENTS,?MAP).