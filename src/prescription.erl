-module (prescriptions).
-include("fmk.hrl").

% %% Functions to handle single Facility objects
-export ([
    id/1,
    new/5,
    prescriber/1,
    patient/1,
    drugs/1,
    process/1,
    add_drugs/1,
    remove_drugs/1
  ]).


new(Id,PatientName,PrescriberName,DatePrescribed,Drugs) ->
  IdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientOp = antidote_lib:build_map_op(?PRESCRIPTION_PATIENT_NAME,?PRESCRIPTION_PATIENT_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(PatientName))),
  PrescriberOp = antidote_lib:build_map_op(?PRESCRIPTION_PRESCRIBER_NAME,?PRESCRIPTION_PRESCRIBER_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(PrescriberName))),
  DatePrescribedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  IsProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_IS_PROCESSED,?PRESCRIPTION_IS_PROCESSED_CRDT,antidote_lib:lwwreg_assign(<<"0">>)),
  [IdOp,PatientOp,PrescriberOp,DatePrescribedOp,IsProcessedOp]).

% -spec update(Id::pos_integer(), FacilityUpdate::riak_dt_map:map_op()) -> {ok,_Something}.
% update(Id,FacilityUpdate) ->
%   antidote_lib:put(Id,riak_dt_map,{FacilityUpdate}).

prescriber(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_PRESCRIBER_NAME,?PRESCRIPTION_PRESCRIBER_NAME_CRDT) of
    not_found -> "";
    Prescriber -> binary_to_list(Prescriber)
  end.

id(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

patient(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_PATIENT_NAME,?PRESCRIPTION_PATIENT_NAME_CRDT) of
    not_found -> "";
    Patient -> binary_to_list(Patient)
  end.

date_prescribed(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT) of
    not_found -> "";
    Date -> binary_to_list(Date)
  end.

date_processed(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT) of
    not_found -> not_processed;
    Date -> binary_to_list(Date)
  end.

drugs(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_DRUGS,?PRESCRIPTION_DRUGS_CRDT) of
    not_found -> [];
    Drugs -> Drugs
  end.

process(CurrentDate) ->
  IsProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_IS_PROCESSED,?PRESCRIPTION_IS_PROCESSED_CRDT,antidote_lib:lwwreg_assign(<<"1">>)),
  ProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PROCESSED,?PRESCRIPTION_DATE_PROCESSED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(CurrentDate))),
  [IsProcessedOp,ProcessedOp].

add_drugs(Drugs) ->
  [build_map_op(?PRESCRIPTION_DRUGS,?PRESCRIPTION_DRUGS_CRDT,antidote_lib:set_add_elements(Drugs))].

remove_drugs(Drugs) ->
  [build_map_op(?PRESCRIPTION_DRUGS,?PRESCRIPTION_DRUGS_CRDT,antidote_lib:set_remove_elements(Drugs))].

