-module (prescription).
-include("fmk.hrl").

%% Functions to handle single Facility objects
-export ([
    id/1,
    new/7,
    new/8,
    facility_id/1,
    patient_id/1,
    pharmacy_id/1,
    prescriber_id/1,
    drugs/1,
    process/1,
    is_processed/1,
    date_prescribed/1,
    date_processed/1,
    add_drugs/1,
    remove_drugs/1
  ]).


new(Id,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  IdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientOp = antidote_lib:build_map_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PharmacyOp = antidote_lib:build_map_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,antidote_lib:counter_increment(PharmacyId)),
  FacilityOp = antidote_lib:build_map_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  PrescriberOp = antidote_lib:build_map_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  DatePrescribedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  IsProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_IS_PROCESSED,?PRESCRIPTION_IS_PROCESSED_CRDT,antidote_lib:lwwreg_assign(<<"0">>)),
  [DrugsOp] = add_drugs(Drugs),
  [IdOp,PatientOp,PharmacyOp,FacilityOp,PrescriberOp,DatePrescribedOp,IsProcessedOp,DrugsOp].

new(Id,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,DateProcessed,Drugs) ->
  IdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(Id)),
  PatientOp = antidote_lib:build_map_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PharmacyOp = antidote_lib:build_map_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,antidote_lib:counter_increment(PharmacyId)),
  FacilityOp = antidote_lib:build_map_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  PrescriberOp = antidote_lib:build_map_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  DatePrescribedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  DateProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DateProcessed))),
  IsProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_IS_PROCESSED,?PRESCRIPTION_IS_PROCESSED_CRDT,antidote_lib:lwwreg_assign(<<"1">>)),
  [DrugsOp] = add_drugs(Drugs),
  [IdOp,PatientOp,PharmacyOp,FacilityOp,PrescriberOp,DatePrescribedOp,DateProcessedOp,IsProcessedOp,DrugsOp].

facility_id(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT) of
    not_found -> 0;
    PharmacyId -> PharmacyId
  end.

pharmacy_id(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT) of
    not_found -> 0;
    PharmacyId -> PharmacyId
  end.

prescriber_id(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT) of
    not_found -> 0;
    PrescriberId -> PrescriberId
  end.

id(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

patient_id(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT) of
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

is_processed(Prescription) ->
  case antidote_lib:find_key(Prescription,?PRESCRIPTION_IS_PROCESSED,?PRESCRIPTION_IS_PROCESSED_CRDT) of
    not_found -> unknown;
    <<"0">> -> no;
    <<"1">> -> yes
  end.

process(CurrentDate) ->
  IsProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_IS_PROCESSED,?PRESCRIPTION_IS_PROCESSED_CRDT,antidote_lib:lwwreg_assign(<<"1">>)),
  ProcessedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PROCESSED,?PRESCRIPTION_DATE_PROCESSED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(CurrentDate))),
  [IsProcessedOp,ProcessedOp].

add_drugs(Drugs) ->
  [antidote_lib:build_map_op(?PRESCRIPTION_DRUGS,?PRESCRIPTION_DRUGS_CRDT,antidote_lib:set_add_elements(Drugs))].

remove_drugs(Drugs) ->
  [antidote_lib:build_map_op(?PRESCRIPTION_DRUGS,?PRESCRIPTION_DRUGS_CRDT,antidote_lib:set_remove_elements(Drugs))].
