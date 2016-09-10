-module(fmk_core).
-include("fmk.hrl").

%%-----------------------------------------------------------------------------
%% Public API for FMK Core
%%-----------------------------------------------------------------------------
-export([
  create_patient/3,
  create_pharmacy/3,
  create_facility/4,
  create_staff/4,  
  create_prescription/7,
  create_prescription/8,
  create_event/7,
  create_treatment/5,
  get_event_by_id/1,
  get_facility_by_id/1,
  get_facility_by_name/1,
  get_facility_prescriptions/1,
  get_facility_treatments/1,
  get_patient_by_id/1,
  get_patient_by_name/1,
  get_pharmacy_by_id/1,
  get_pharmacy_by_name/1,
  get_pharmacy_prescriptions/1,
  get_prescription_by_id/1,
  get_staff_by_id/1,
  get_staff_by_name/1,
  get_staff_prescriptions/1,
  get_staff_treatments/1,
  get_treatment_by_id/1,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  update_staff_details/4,
  update_prescription_details/5,
  update_treatment_details/5,
  update_event_details/5
  ]).

%% Exports needed for other modules
-export ([
    concatenate_id/2,
    binary_patient_key/1,
    binary_treatment_key/1
  ]).

%% Adds a patient to the FMK system, needing only an ID, Name and Address.
%% A check is done to determine if a patient with the given ID already exists,
%% and if so the operation fails. If there isn't any indexed patient, he/she
%% will be added to the system and indexed by both Name and ID.
create_patient(Id,Name,Address) ->
  case get_patient_by_id(Id) of
    {error,not_found} ->
      Patient = patient:new(Id,Name,Address),
      PatientKey = binary_patient_key(Id),
      ok = fmk_index:index_patient(Id,Name),
      ok = antidote_lib:put(PatientKey,?MAP,update,Patient,fmk);
    _Patient ->
      {error, patient_id_taken}
  end.

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet
%% been seen. If the operation succeeds, the pharmacy will be indexed by both
%% Name and ID.
create_pharmacy(Id,Name,Address) ->
  case get_pharmacy_by_id(Id) of
    {error,not_found} ->
      Pharmacy = pharmacy:new(Id,Name,Address),
      PharmacyKey = binary_pharmacy_key(Id),
      ok = fmk_index:index_pharmacy(Id,Name),
      ok = antidote_lib:put(PharmacyKey,?MAP,update,Pharmacy,fmk);
    _Pharmacy ->
      {error, pharmacy_id_taken}
  end.

create_facility(Id,Name,Address,Type) ->
  case get_facility_by_id(Id) of
    {error,not_found} ->
      Facility = facility:new(Id,Name,Address,Type),
      FacilityKey = binary_facility_key(Id),
      ok = fmk_index:index_facility(Id,Name),
      ok = antidote_lib:put(FacilityKey,?MAP,update,Facility,fmk);
    _Facility ->
      {error, facility_id_taken}
  end.

create_staff(Id,Name,Address,Speciality) ->
  case get_staff_by_id(Id) of
    {error,not_found} ->
      Staff = staff:new(Id,Name,Address,Speciality),
      StaffKey = binary_staff_key(Id),
      ok = fmk_index:index_staff(Id,Name),
      ok = antidote_lib:put(StaffKey,?MAP,update,Staff,fmk);
    _Facility ->
      {error, staff_id_taken}
  end.

get_event_by_id(Id) ->
  Event = antidote_lib:get(binary_event_key(Id),?MAP),
  case Event of
    [] -> {error,not_found};
    EventMap -> EventMap
  end.

get_facility_by_id(Id) ->
  Facility = antidote_lib:get(binary_facility_key(Id),?MAP),
  case Facility of
    [] -> {error,not_found};
    FacilityMap -> FacilityMap
  end.

get_facility_by_name(Name) ->
  case search_facility_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

get_facility_prescriptions(FacilityId) ->
  case get_facility_by_id(FacilityId) of
    {error,not_found} -> {error,no_such_facility};
    Facility -> facility:prescriptions(Facility)
  end.

get_facility_treatments(FacilityId) ->
  case get_facility_by_id(FacilityId) of
    {error,not_found} -> {error,no_such_facility};
    Facility -> facility:treatments(Facility)
  end.

get_pharmacy_by_id(Id) ->
  Pharmacy = antidote_lib:get(binary_pharmacy_key(Id),?MAP),
  case Pharmacy of
    [] -> {error,not_found};
    PharmacyMap -> PharmacyMap
  end.

%% Finds a patient in the Antidote Key-Value store by Patient ID.
get_patient_by_id(Id) ->
  Patient = antidote_lib:get(binary_patient_key(Id),?MAP),
  case Patient of
    [] -> {error,not_found};
    PatientMap -> PatientMap
  end.

get_patient_by_name(Name) ->
  case search_patient_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

get_pharmacy_by_name(Name) ->
  case search_pharmacy_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

get_pharmacy_prescriptions(PharmacyId) ->
  case get_pharmacy_by_id(PharmacyId) of
    {error,not_found} -> {error,no_such_pharmacy};
    Pharmacy -> pharmacy:prescriptions(Pharmacy)
  end.

get_prescription_by_id(Id) ->
  Prescription = antidote_lib:get(binary_prescription_key(Id),?MAP),
  case Prescription of
    [] -> {error,not_found};
    PrescriptionMap -> PrescriptionMap
  end.

get_staff_by_id(Id) ->
  Staff = antidote_lib:get(binary_staff_key(Id),?MAP),
  case Staff of
    [] -> {error,not_found};
    StaffMap -> StaffMap
  end.

get_staff_by_name(Name) ->
  case search_staff_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

get_staff_prescriptions(StaffId) ->
  case get_staff_by_id(StaffId) of
    {error,not_found} -> {error,no_such_facility};
    Staff -> staff:prescriptions(Staff)
  end.

get_staff_treatments(StaffId) ->
  case get_staff_by_id(StaffId) of
    {error,not_found} -> {error,no_such_facility};
    Staff -> staff:treatments(Staff)
  end.

get_treatment_by_id(Id) ->
  Treatment = antidote_lib:get(binary_treatment_key(Id),?MAP),
  case Treatment of
    [] -> {error,not_found};
    TreatmentMap -> TreatmentMap
  end.

update_patient_details(Id,Name,Address) ->
  case get_patient_by_id(Id) of
    {error,not_found} ->
      {error,no_such_patient};
    Patient ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      PatientKey = binary_patient_key(Id),
      PatientName = patient:name(Patient),
      PatientUpdate = patient:update_personal_details(Name,Address),
      antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,fmk),
      case string:equal(PatientName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_patient(Id,PatientName,Name)
      end
  end.

update_pharmacy_details(Id,Name,Address) ->
  case get_pharmacy_by_id(Id) of
    {error,not_found} ->
      {error,no_such_pharmacy};
    Pharmacy ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      PharmacyKey = binary_pharmacy_key(Id),
      PharmacyName = pharmacy:name(Pharmacy),
      PharmacyUpdate = pharmacy:update_details(Name,Address),
      antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,fmk),
      case string:equal(PharmacyName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_pharmacy(Id,PharmacyName,Name)
      end
  end.

update_facility_details(Id,Name,Address,Type) ->
  case get_facility_by_id(Id) of
    {error,not_found} ->
      {error,no_such_facility};
    Facility ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      FacilityKey = binary_pharmacy_key(Id),
      FacilityName = facility:name(Facility),
      FacilityUpdate = facility:update_details(Name,Address,Type),
      antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,fmk),
      case string:equal(FacilityName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_facility(Id,FacilityName,Name)
      end
  end.

update_staff_details(Id,Name,Address,Speciality) ->
  case get_staff_by_id(Id) of
    {error,not_found} ->
      {error,no_such_staff_member};
    Staff ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      StaffKey = binary_pharmacy_key(Id),
      StaffName = staff:name(Staff),
      StaffUpdate = staff:update_personal_details(Name,Address,Speciality),
      antidote_lib:put(StaffKey,?MAP,update,StaffUpdate,fmk),
      case string:equal(StaffName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_staff(Id,StaffName,Name)
      end
  end.

% add_prescription(_PrescriptionId, PatientName, PrescriberName, _DatePrescribed, _Drugs) ->
%   PatientNameIndex = fmk_index:get_patient_name_index(),
%   case fmk_index:is_indexed(PatientName,PatientNameIndex) of
%     false -> {error, no_such_patient};
%     true ->
%       %% Patient is indexed, still need to check the prescriber
%       StaffNameIndex = fmk_index:get_staff_name_index(),
%       PatientNameIndex = fmk_index:get_patient_name_index(),
%       case fmk_index:is_indexed(PrescriberName,StaffNameIndex) of
%         false -> {error, no_such_staff_member};
%         true ->
%           %% Both the patient and the prescriber are indexed, the transaction can take place
%           %% create prescription object (may be different for different purposes)
%           %% It makes sense to perform all operations in a single transaction
%           Txn = antidote_lib:start_txn(),

%           %% update patient's prescriptions
%           %% update prescriber's prescriptions
%           %% update facility's prescriptions
%           %% update pharmacy's prescriptions
%           %% Update treatment's prescriptions
%           %% update everything in a single transaction and commit
          
%           ok = antidote_lib:txn_commit(Txn)
%       end      
%   end.

create_prescription(_PrescriptionId,_PatientId,_PrescriberId,_PharmacyId,_FacilityId,_DatePrescribed,_Drugs) ->
  %% check required pre-conditions
  free = check_prescription_id(_PrescriptionId),
  taken = check_patient_id(_PatientId),
  taken = check_staff_id(_PrescriberId),
  taken = check_pharmacy_id(_PharmacyId),
  taken = check_facility_id(_FacilityId),
  _Txn = antidote_lib:start_txn(),

  ok.

create_prescription(_PrescriptionId,_PatientId,_TreatmentId,_PrescriberId,_PharmacyId,_FacilityId,_DatePrescribed,_Drugs) ->
  %% check required pre-conditions
  free = check_prescription_id(_PrescriptionId),
  taken = check_patient_id(_PatientId),
  taken = check_treatment_id(_TreatmentId),
  taken = check_staff_id(_PrescriberId),
  taken = check_pharmacy_id(_PharmacyId),
  taken = check_facility_id(_FacilityId),
  _Txn = antidote_lib:start_txn(),
  ok.

create_event(_EventId,_PatientId,_TreatmentId,_StaffId,_FacilityId,_TimeStamp,_Description) ->
  %% check required pre-conditions
  free = check_event_id(_EventId),
  taken = check_patient_id(_PatientId),
  taken = check_treatment_id(_TreatmentId),
  taken = check_staff_id(_StaffId),
  taken = check_facility_id(_FacilityId),
  _Txn = antidote_lib:start_txn(),
  ok.

create_treatment(TreatmentId,PatientId,StaffId,FacilityId,TimeStamp) ->
  %% check required pre-conditions
  %% TODO I'm performing a get for each operation below, can it be improved?
  free = check_treatment_id(TreatmentId),
  taken = check_patient_id(PatientId),
  taken = check_staff_id(StaffId),
  taken = check_facility_id(FacilityId),
  %% gather required antidote keys
  TreatmentKey = binary_treatment_key(TreatmentId),
  PatientKey = binary_patient_key(PatientId),
  FacilityKey = binary_facility_key(FacilityId),
  %% build top level update for treatment objects
  TopLevelTreatment = treatment:new(TreatmentId,PatientId,StaffId,FacilityId,TimeStamp),
  %% build nested updates for facilities and patients
  PatientUpdate = patient:add_treatment(TreatmentId,StaffId,FacilityId,TimeStamp),
  FacilityUpdate = facility:add_treatment(TreatmentId,PatientId,StaffId,TimeStamp),
  %% TODO should everything happen in a single transaction? Discuss with Nuno and Joao
  %% add top level domain
  antidote_lib:put(TreatmentKey,?MAP,update,TopLevelTreatment,fmk),
  %% add to patient treatments
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,fmk),
  %% add to facility treatments
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,fmk),
  ok.

check_prescription_id(Id) ->
  case antidote_lib:get_prescription_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_staff_id(Id) ->
  case get_staff_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_patient_id(Id) ->
  case get_patient_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_pharmacy_id(Id) ->
  case get_pharmacy_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_facility_id(Id) ->
  case get_facility_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_treatment_id(Id) ->
  case get_treatment_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_event_id(Id) ->
  case get_event_by_id(Id) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

  update_prescription_details(_1,_2,_3,_4,_5) ->
    %% TODO!
    ok.

  update_treatment_details(_1,_2,_3,_4,_5) ->
    %% TODO!
    ok.
    
  update_event_details(_1,_2,_3,_4,_5) ->
    %% TODO!
    ok.

binary_patient_key(Id) ->
  list_to_binary(concatenate_id(patient,Id)).

binary_pharmacy_key(Id) ->
  list_to_binary(concatenate_id(pharmacy,Id)).

binary_facility_key(Id) ->
  list_to_binary(concatenate_id(facility,Id)).

binary_staff_key(Id) ->
  list_to_binary(concatenate_id(staff,Id)).

binary_prescription_key(Id) ->
  list_to_binary(concatenate_id(prescription,Id)).

binary_treatment_key(Id) ->
  list_to_binary(concatenate_id(treatment,Id)).
  
binary_event_key(Id) ->
  list_to_binary(concatenate_id(event,Id)).

concatenate_list_with_id(List,Id) ->
  lists:flatten(io_lib:format(List, [Id])).

search_patient_index(Name) ->
  search_fmk_index(patient,Name).

search_pharmacy_index(Name) ->
  search_fmk_index(pharmacy,Name).

search_facility_index(Name) ->
  search_fmk_index(facility,Name).

search_staff_index(Name) ->
  search_fmk_index(staff,Name).

concatenate_id(patient,Id) ->
  concatenate_list_with_id("patient_~p",Id);
concatenate_id(pharmacy,Id) ->
  concatenate_list_with_id("pharmacy_~p",Id);
concatenate_id(facility,Id) ->
  concatenate_list_with_id("facility_~p",Id);
concatenate_id(staff,Id) ->
  concatenate_list_with_id("staff_member_~p",Id);
concatenate_id(prescription,Id) ->
  concatenate_list_with_id("prescription_~p",Id);
concatenate_id(treatment,Id) ->
  concatenate_list_with_id("treatment_~p",Id);
concatenate_id(event,Id) ->
  concatenate_list_with_id("event_~p",Id);
concatenate_id(_,_) ->
  undefined.

search_fmk_index(staff,Name) ->
  fmk_index:search_index(Name,fmk_index:get_staff_name_index());
search_fmk_index(facility,Name) ->
  fmk_index:search_index(Name,fmk_index:get_facility_name_index());
search_fmk_index(patient,Name) ->
  fmk_index:search_index(Name,fmk_index:get_patient_name_index());
search_fmk_index(pharmacy, Name) ->
  fmk_index:search_index(Name,fmk_index:get_pharmacy_name_index());
search_fmk_index(_,_) ->
  undefined.
