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
  create_event/5,
  create_treatment/5,
  create_treatment/6,
  get_event_by_id/1,
  get_facility_by_id/1,
  get_facility_by_name/1,
  get_facility_prescriptions/1,
  get_facility_treatments/1,
  get_patient_by_id/1,
  get_patient_by_name/1,
  get_pharmacy_by_id/1,
  get_pharmacy_by_name/1,
  get_processed_pharmacy_prescriptions/1,
  get_pharmacy_prescriptions/1,
  get_prescription_by_id/1,
  get_prescription_medication/1,
  get_staff_by_id/1,
  get_staff_by_name/1,
  get_staff_prescriptions/1,
  get_staff_treatments/1,
  get_treatment_by_id/1,
  process_prescription/1,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  update_staff_details/4,
  update_prescription_medication/3
  ]).

%% Exports needed for other modules
-export ([
    binary_patient_key/1,
    binary_treatment_key/1,
    binary_prescription_key/1,
    binary_event_key/1
  ]).

%% Adds a patient to the FMK system, needing only an ID, Name and Address.
%% A check is done to determine if a patient with the given ID already exists,
%% and if so the operation fails. If there isn't any indexed patient, he/she
%% will be added to the system and indexed by both Name and ID.
-spec create_patient(id(),string(),string()) -> ok | {error, reason()}.
create_patient(Id,Name,Address) ->
  Txn = antidote_lib:txn_start(),
  Result = case get_patient_by_id(Id,Txn) of
    {error,not_found} ->
      Patient = patient:new(Id,Name,Address),
      PatientKey = binary_patient_key(Id),
      ok = fmk_index:index_patient(concatenate_id(patient,Id),Name,Txn),
      ok = antidote_lib:put(PatientKey,?MAP,update,Patient,Txn);
    _Patient ->
      {error, patient_id_taken}
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet
%% been seen. If the operation succeeds, the pharmacy will be indexed by both
%% Name and ID.
-spec create_pharmacy(id(),string(),string()) -> ok | {error, reason()}.
create_pharmacy(Id,Name,Address) ->
  Txn = antidote_lib:txn_start(),
  Result = case get_pharmacy_by_id(Id,Txn) of
    {error,not_found} ->
      Pharmacy = pharmacy:new(Id,Name,Address),
      PharmacyKey = binary_pharmacy_key(Id),
      ok = fmk_index:index_pharmacy(concatenate_id(pharmacy,Id),Name,Txn),
      ok = antidote_lib:put(PharmacyKey,?MAP,update,Pharmacy,Txn);
    _Pharmacy ->
      {error, pharmacy_id_taken}
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Adds a facility to the FMK-- system if the ID for the facility has not yet
%% been seen. If the operation succeeds, the facility will be indexed by both
%% Name and ID.
-spec create_facility(id(),string(),string(),string()) -> ok | {error, reason()}.
create_facility(Id,Name,Address,Type) ->
  Txn = antidote_lib:txn_start(),
  Result = case get_facility_by_id(Id,Txn) of
    {error,not_found} ->
      Facility = facility:new(Id,Name,Address,Type),
      FacilityKey = binary_facility_key(Id),
      ok = fmk_index:index_facility(concatenate_id(facility,Id),Name,Txn),
      ok = antidote_lib:put(FacilityKey,?MAP,update,Facility,Txn);
    _Facility ->
      {error, facility_id_taken}
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Adds a staff member to the FMK-- system if the ID for the member has not yet
%% been seen. If the operation succeeds, the staff member will be indexed by both
%% Name and ID.
-spec create_staff(id(),string(),string(),string()) -> ok | {error, reason()}.
create_staff(Id,Name,Address,Speciality) ->
  Txn = antidote_lib:txn_start(),
  Result = case get_staff_by_id(Id,Txn) of
    {error,not_found} ->
      Staff = staff:new(Id,Name,Address,Speciality),
      StaffKey = binary_staff_key(Id),
      ok = fmk_index:index_staff(concatenate_id(staff,Id),Name,Txn),
      ok = antidote_lib:put(StaffKey,?MAP,update,Staff,Txn);
    _Facility ->
      {error, staff_id_taken}
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Fetches a treatment event by id.
-spec get_event_by_id(id()) -> [crdt()] | {error, reason()}.
get_event_by_id(Id) ->
  process_get_request(binary_event_key(Id),?MAP).

%% Alternative to get_event_by_id/1, which includes a transactional context.
-spec get_event_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_event_by_id(Id,Txn) ->
  process_get_request(binary_event_key(Id),?MAP,Txn).


%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) ->
  process_get_request(binary_facility_key(Id),?MAP).

%% Alternative to get_facility_by_id/1, which includes a transactional context.
-spec get_facility_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id,Txn) ->
  process_get_request(binary_facility_key(Id),?MAP,Txn).

%% Fetches a facility by name.
-spec get_facility_by_name(string()) -> [crdt()] | {error, reason()}.
get_facility_by_name(Name) ->
  Txn = antidote_lib:txn_start(),
  Result = case search_facility_index(list_to_binary(Name),Txn) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP,Txn);
    List -> [antidote_lib:get(Result,?MAP,Txn) || Result <- List]
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Fetches the list of facility prescriptions given a certain facility ID.
-spec get_facility_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_facility_prescriptions(FacilityId) ->
  case get_facility_by_id(FacilityId) of
    {error,not_found} -> {error,no_such_facility};
    Facility -> facility:prescriptions(Facility)
  end.

%% Fetches the list of facility treatments given a certain facility ID.
-spec get_facility_treatments(id()) -> [crdt()] | {error, reason()}.
get_facility_treatments(FacilityId) ->
  case get_facility_by_id(FacilityId) of
    {error,not_found} -> {error,no_such_facility};
    Facility -> facility:treatments(Facility)
  end.

%% Fetches a pharmacy by ID.
-spec get_pharmacy_by_id(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id) ->
  process_get_request(binary_pharmacy_key(Id),?MAP).

%% Alternative to get_pharmacy_by_id/1, which includes a transactional context.
-spec get_pharmacy_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id,Txn) ->
  process_get_request(binary_pharmacy_key(Id),?MAP,Txn).

%% Fetches a patient by ID.
-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) ->
  process_get_request(binary_patient_key(Id),?MAP).

%% Alternative to get_patient_by_id/1, which includes a transactional context.
-spec get_patient_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id,Txn) ->
  process_get_request(binary_patient_key(Id),?MAP,Txn).

%% Fetches a patient by name.
-spec get_patient_by_name(string()) -> [crdt()] | {error, reason()}.
get_patient_by_name(Name) ->
  Txn = antidote_lib:txn_start(),
  Result = case search_patient_index(list_to_binary(Name),Txn) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP,Txn);
    List -> [antidote_lib:get(Result,?MAP,Txn) || Result <- List]
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Fetches a pharmacy by name.
-spec get_pharmacy_by_name(string()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_name(Name) ->
  Txn = antidote_lib:txn_start(),
  Result = case search_pharmacy_index(list_to_binary(Name), Txn) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP,Txn);
    List -> [antidote_lib:get(Result,?MAP,Txn) || Result <- List]
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(PharmacyId) ->
  case get_pharmacy_by_id(PharmacyId) of
    {error,not_found} -> {error,no_such_pharmacy};
    Pharmacy -> pharmacy:prescriptions(Pharmacy)
  end.

-spec get_processed_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_processed_pharmacy_prescriptions(PharmacyId) ->
  case get_pharmacy_by_id(PharmacyId) of
    {error,not_found} -> {error,no_such_pharmacy};
    Pharmacy ->
      PharmacyPrescriptions = pharmacy:prescriptions(Pharmacy),
      filter_processed_prescriptions(PharmacyPrescriptions)
  end.

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
  process_get_request(binary_prescription_key(Id),?MAP).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
get_prescription_medication(Id) ->
  Prescription = process_get_request(binary_prescription_key(Id),?MAP),
  case Prescription of
    {error, _} ->
      {error, no_such_prescription};
    PrescriptionObject ->
      prescription:drugs(PrescriptionObject)
  end.

%% Alternative to get_prescription_by_id/1, which includes a transactional context.
-spec get_prescription_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id,Txn) ->
  process_get_request(binary_prescription_key(Id),?MAP,Txn).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) ->
  process_get_request(binary_staff_key(Id),?MAP).

%% Alternative to get_staff_by_id/1, which includes a transactional context.
-spec get_staff_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id,Txn) ->
  process_get_request(binary_staff_key(Id),?MAP,Txn).

%% Fetches a staff member by name.
-spec get_staff_by_name(string()) -> [crdt()] | {error, reason()}.
get_staff_by_name(Name) ->
  Txn = antidote_lib:txn_start(),
  Result = case search_staff_index(list_to_binary(Name),Txn) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP,Txn);
    List -> [antidote_lib:get(Result,?MAP,Txn) || Result <- List]
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_staff_prescriptions(StaffId) ->
  case get_staff_by_id(StaffId) of
    {error,not_found} -> {error,no_such_facility};
    Staff -> staff:prescriptions(Staff)
  end.

%% Fetches a list of treatments given a certain staff member ID.
-spec get_staff_treatments(id()) -> [crdt()] | {error, reason()}.
get_staff_treatments(StaffId) ->
  case get_staff_by_id(StaffId) of
    {error,not_found} -> {error,no_such_facility};
    Staff -> staff:treatments(Staff)
  end.

%% Fetches a treatment by ID.
-spec get_treatment_by_id(id()) -> [crdt()] | {error, reason()}.
get_treatment_by_id(Id) ->
  process_get_request(binary_treatment_key(Id),?MAP).

%% Fetches a treatment by ID.
-spec get_treatment_by_id(id(),txid()) -> [crdt()] | {error, reason()}.
get_treatment_by_id(Id,Txn) ->
  process_get_request(binary_treatment_key(Id),?MAP,Txn).

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(),string(),string()) -> ok | {error, reason()}.
update_patient_details(Id,Name,Address) ->
  Txn = antidote_lib:txn_start(),
  case get_patient_by_id(Id,Txn) of
    {error,not_found} ->
      {error,no_such_patient};
    Patient ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      PatientKey = binary_patient_key(Id),
      PatientName = patient:name(Patient),
      PatientUpdate = patient:update_details(Name,Address),
      antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
      case string:equal(PatientName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_patient(concatenate_id(patient,Id),PatientName,Name,Txn)
      end,
      ok = antidote_lib:txn_commit(Txn)
  end.

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(),string(),string()) -> ok | {error, reason()}.
update_pharmacy_details(Id,Name,Address) ->
  Txn = antidote_lib:txn_start(),
  case get_pharmacy_by_id(Id,Txn) of
    {error,not_found} ->
      {error,no_such_pharmacy};
    Pharmacy ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      PharmacyKey = binary_pharmacy_key(Id),
      PharmacyName = pharmacy:name(Pharmacy),
      PharmacyUpdate = pharmacy:update_details(Name,Address),
      antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,Txn),
      case string:equal(PharmacyName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_pharmacy(concatenate_id(pharmacy,Id),PharmacyName,Name,Txn)
      end,
      ok = antidote_lib:txn_commit(Txn)
  end.

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(),string(),string(),string()) -> ok | {error, reason()}.
update_facility_details(Id,Name,Address,Type) ->
  Txn = antidote_lib:txn_start(),
  case get_facility_by_id(Id,Txn) of
    {error,not_found} ->
      {error,no_such_facility};
    Facility ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      FacilityKey = binary_pharmacy_key(Id),
      FacilityName = facility:name(Facility),
      FacilityUpdate = facility:update_details(Name,Address,Type),
      antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
      case string:equal(FacilityName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_facility(concatenate_id(facility,Id),FacilityName,Name,Txn)
      end,
      ok = antidote_lib:txn_commit(Txn)
  end.

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(),string(),string(),string()) -> ok | {error, reason()}.
update_staff_details(Id,Name,Address,Speciality) ->
  Txn = antidote_lib:txn_start(),
  case get_staff_by_id(Id,Txn) of
    {error,not_found} ->
      {error,no_such_staff_member};
    Staff ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      StaffKey = binary_pharmacy_key(Id),
      StaffName = staff:name(Staff),
      StaffUpdate = staff:update_details(Name,Address,Speciality),
      antidote_lib:put(StaffKey,?MAP,update,StaffUpdate,Txn),
      case string:equal(StaffName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_staff(concatenate_id(staff,Id),StaffName,Name,Txn)
      end,
      ok = antidote_lib:txn_commit(Txn)
  end.

-spec update_prescription_medication(id(),atom(),[string()]) -> ok | {error, reason()}.
update_prescription_medication(Id,add_drugs,Drugs) ->
  Txn = antidote_lib:txn_start(),
  Result = case get_prescription_by_id(Id,Txn) of
    {error,not_found} ->
      {error,no_such_prescription};
    Prescription ->
      UpdateOperation = prescription:add_drugs(Drugs),
      PatientId = prescription:patient_id(Prescription),
      PharmacyId = prescription:pharmacy_id(Prescription),
      FacilityId = prescription:facility_id(Prescription),
      PrescriberId = prescription:prescriber_id(Prescription),
      %% gather required antidote keys
      PrescriptionKey = binary_prescription_key(Id),
      PatientKey = binary_patient_key(PatientId),
      PharmacyKey = binary_pharmacy_key(PharmacyId),
      FacilityKey = binary_facility_key(FacilityId),
      PrescriberKey = binary_staff_key(PrescriberId),
      %% build nested updates for patients, pharmacies, facilities and the prescriber
      PatientUpdate = patient:add_prescription_drugs(Id,Drugs),
      FacilityUpdate = facility:add_prescription_drugs(Id,Drugs),
      PharmacyUpdate = pharmacy:add_prescription_drugs(Id,Drugs),
      PrescriberUpdate = staff:add_prescription_drugs(Id,Drugs),
      %% update top level prescription
      antidote_lib:put(PrescriptionKey,?MAP,update,UpdateOperation,Txn),
      %% add to patient prescriptions
      antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
      %% add to hospital prescriptions
      antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
      %% add to pharmacy prescriptions
      antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,Txn),
      %% add to the prescriber's prescriptions
      antidote_lib:put(PrescriberKey,?MAP,update,PrescriberUpdate,Txn),
      ok
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result;
update_prescription_medication(_Id,_action,_Drugs) -> {error,undefined}.

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy and treatment facility (hospital). The prescription also includes the prescription date
%% and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  %% check required pre-conditions
  Txn = antidote_lib:txn_start(),
  free = check_prescription_id(PrescriptionId,Txn),
  taken = check_patient_id(PatientId,Txn),
  taken = check_staff_id(PrescriberId,Txn),
  taken = check_pharmacy_id(PharmacyId,Txn),
  taken = check_facility_id(FacilityId,Txn),
  %% gather required antidote keys
  PrescriptionKey = binary_prescription_key(PrescriptionId),
  PatientKey = binary_patient_key(PatientId),
  PharmacyKey = binary_pharmacy_key(PharmacyId),
  FacilityKey = binary_facility_key(FacilityId),
  PrescriberKey = binary_staff_key(PrescriberId),
  %% build top level update for the prescription
  TopLevelPrescription = prescription:new(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  %% build nested updates for patients, pharmacies, facilities and the prescriber
  PatientUpdate = patient:add_prescription(PrescriptionId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  FacilityUpdate = facility:add_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs),
  PharmacyUpdate = pharmacy:add_prescription(PrescriptionId,PatientId,PrescriberId,FacilityId,DatePrescribed,Drugs),
  PrescriberUpdate = staff:add_prescription(PrescriptionId,PatientId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  %% add top level prescription
  antidote_lib:put(PrescriptionKey,?MAP,update,TopLevelPrescription,Txn),
  %% add to pharmaciy prescriptions
  antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,Txn),
  %% add to the prescriber's prescriptions
  antidote_lib:put(PrescriberKey,?MAP,update,PrescriberUpdate,Txn),
  %% add to patient prescriptions
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
  %% add to hospital prescriptions
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
  ok = antidote_lib:txn_commit(Txn),
  ok.

%% Same as create_prescription/7, but includes a reference to the treatment to which the
%% prescription is associated with.
-spec create_prescription(id(), id(), id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId,TreatmentId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  %% check required pre-conditions
  Txn = antidote_lib:txn_start(),
  free = check_prescription_id(PrescriptionId,Txn),
  taken = check_patient_id(PatientId,Txn),
  taken = check_staff_id(PrescriberId,Txn),
  taken = check_pharmacy_id(PharmacyId,Txn),
  taken = check_facility_id(FacilityId,Txn),
  %% gather required antidote keys
  PrescriptionKey = binary_prescription_key(PrescriptionId),
  PatientKey = binary_patient_key(PatientId),
  PharmacyKey = binary_pharmacy_key(PharmacyId),
  FacilityKey = binary_facility_key(FacilityId),
  PrescriberKey = binary_staff_key(PrescriberId),
  TreatmentKey = binary_treatment_key(TreatmentId),
  %% build top level update for the prescription
  TopLevelPrescription = prescription:new(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  %% build nested updates for patients, pharmacies, facilities and the prescriber
  PatientUpdate = patient:add_prescription(PrescriptionId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  FacilityUpdate = facility:add_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,DatePrescribed,Drugs),
  PharmacyUpdate = pharmacy:add_prescription(PrescriptionId,PatientId,PrescriberId,FacilityId,DatePrescribed,Drugs),
  PrescriberUpdate = staff:add_prescription(PrescriptionId,PatientId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  TreatmentUpdate = treatment:add_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs),
  %% add top level prescription
  antidote_lib:put(PrescriptionKey,?MAP,update,TopLevelPrescription,Txn),
  %% add to patient prescriptions
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
  %% add to hospital prescriptions
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
  %% add to pharmaciy prescriptions
  antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,Txn),
  %% add to the prescriber's prescriptions
  antidote_lib:put(PrescriberKey,?MAP,update,PrescriberUpdate,Txn),
  %% add to the treatment's prescriptions
  antidote_lib:put(TreatmentKey,?MAP,update,TreatmentUpdate,Txn),
  ok = antidote_lib:txn_commit(Txn),
  ok.

%% Creates a treatment event, with information about the staff member that registered it,
%% along with a timestamp and description.
-spec create_event(id(), id(), id(), string(), string()) -> ok | {error, reason()}.
create_event(EventId,TreatmentId,StaffMemberId,Timestamp,Description) ->
  %% check required pre-conditions
  Txn = antidote_lib:txn_start(),
  free = check_event_id(EventId,Txn),
  taken = check_treatment_id(TreatmentId,Txn),
  taken = check_staff_id(StaffMemberId,Txn),
  TreatmentObject = get_treatment_by_id(TreatmentId,Txn),
  PatientId = treatment:patient_id(TreatmentObject),
  FacilityId = treatment:facility_id(TreatmentObject),

   %% gather required antidote keys
  PatientKey = binary_patient_key(PatientId),
  FacilityKey = binary_facility_key(FacilityId),
  TreatmentKey = binary_treatment_key(TreatmentId),
  EventKey = binary_event_key(EventId),
  %% build top level update for the event
  TopLevelEvent = event:new(EventId,PatientId,StaffMemberId,Timestamp,Description),
  %% build nested updates for treatments, patients and facilities
  PatientUpdate = patient:add_event(TreatmentId,EventId,StaffMemberId,Timestamp,Description),
  FacilityUpdate = facility:add_event(TreatmentId,EventId,StaffMemberId,Timestamp,Description),
  TreatmentUpdate = treatment:add_event(EventId,StaffMemberId,Timestamp,Description),
  %% add top level event
  antidote_lib:put(EventKey,?MAP,update,TopLevelEvent,Txn),
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
  antidote_lib:put(TreatmentKey,?MAP,update,TreatmentUpdate,Txn),
  ok = antidote_lib:txn_commit(Txn),
  ok.

%% Creates a treatment with information about the patient, the staff member that iniciated it,
%% and also the facility ID and date when the treatment started.
-spec create_treatment(id(), id(), id(), id(), string()) -> ok | {error, reason()}.
create_treatment(TreatmentId,PatientId,StaffId,FacilityId,DateStarted) ->
  %% check required pre-conditions
  Txn = antidote_lib:txn_start(),
  free = check_treatment_id(TreatmentId,Txn),
  taken = check_patient_id(PatientId,Txn),
  taken = check_staff_id(StaffId,Txn),
  taken = check_facility_id(FacilityId,Txn),
  %% gather required antidote keys
  TreatmentKey = binary_treatment_key(TreatmentId),
  PatientKey = binary_patient_key(PatientId),
  FacilityKey = binary_facility_key(FacilityId),
  %% build top level update for treatment objects
  TopLevelTreatment = treatment:new(TreatmentId,PatientId,StaffId,FacilityId,DateStarted),
  %% build nested updates for facilities and patients
  PatientUpdate = patient:add_treatment(TreatmentId,StaffId,FacilityId,DateStarted),
  FacilityUpdate = facility:add_treatment(TreatmentId,PatientId,StaffId,DateStarted),
  %% add top level treatment
  antidote_lib:put(TreatmentKey,?MAP,update,TopLevelTreatment,Txn),
  %% add to patient treatments
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
  %% add to facility treatments
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
  ok = antidote_lib:txn_commit(Txn),
  ok.

%% Same as create_treatment/5, but includes an ending date for the treatment.
-spec create_treatment(id(), id(), id(), id(), string(), string()) -> ok | {error, reason()}.
create_treatment(TreatmentId,PatientId,StaffId,FacilityId,DateStarted,DateEnded) ->
  %% check required pre-conditions
  Txn = antidote_lib:txn_start(),
  free = check_treatment_id(TreatmentId,Txn),
  taken = check_patient_id(PatientId,Txn),
  taken = check_staff_id(StaffId,Txn),
  taken = check_facility_id(FacilityId,Txn),
  %% gather required antidote keys
  TreatmentKey = binary_treatment_key(TreatmentId),
  PatientKey = binary_patient_key(PatientId),
  FacilityKey = binary_facility_key(FacilityId),
  %% build top level update for treatment objects
  TopLevelTreatment = treatment:new(TreatmentId,PatientId,StaffId,FacilityId,DateStarted,DateEnded),
  %% build nested updates for facilities and patients
  PatientUpdate = patient:add_treatment(TreatmentId,StaffId,FacilityId,DateStarted,DateEnded),
  FacilityUpdate = facility:add_treatment(TreatmentId,PatientId,StaffId,DateStarted,DateEnded),
  %% add top level treatment
  antidote_lib:put(TreatmentKey,?MAP,update,TopLevelTreatment,Txn),
  %% add to patient treatments
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
  %% add to facility treatments
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
  ok = antidote_lib:txn_commit(Txn),
  ok.

%% Builds the patient key inside Antidote given the patient ID.
-spec binary_patient_key(id()) -> binary().
binary_patient_key(Id) ->
  list_to_binary(concatenate_id(patient,Id)).

%% Builds the pharmacy key inside Antidote given the pharmacy ID.
-spec binary_pharmacy_key(id()) -> binary().
binary_pharmacy_key(Id) ->
  list_to_binary(concatenate_id(pharmacy,Id)).

%% Builds the facility key inside Antidote given the facility ID.
-spec binary_facility_key(id()) -> binary().
binary_facility_key(Id) ->
  list_to_binary(concatenate_id(facility,Id)).

%% Builds the staff member key inside Antidote given the staff member ID.
-spec binary_staff_key(id()) -> binary().
binary_staff_key(Id) ->
  list_to_binary(concatenate_id(staff,Id)).

%% Builds the prescription key inside Antidote given the prescription ID.
-spec binary_prescription_key(id()) -> binary().
binary_prescription_key(Id) ->
  list_to_binary(concatenate_id(prescription,Id)).

%% Builds the treatment key inside Antidote given the treatment ID.
-spec binary_treatment_key(id()) -> binary().
binary_treatment_key(Id) ->
  list_to_binary(concatenate_id(treatment,Id)).

%% Builds the event key inside Antidote given the event ID.
-spec binary_event_key(id()) -> binary().
binary_event_key(Id) ->
  list_to_binary(concatenate_id(event,Id)).

process_prescription(Id) ->
  Txn = antidote_lib:txn_start(),
  Result = case get_prescription_by_id(Id,Txn) of
    {error,not_found} ->
      {error,no_such_prescription};
    Prescription ->
      case prescription:is_processed(Prescription) of
        ?PRESCRIPTION_PROCESSED ->
          {error,prescription_already_processed};
        ?PRESCRIPTION_NOT_PROCESSED ->
          CurrentDate = "31/12/2016",
          UpdateOperation = prescription:process(CurrentDate),
          PatientId = prescription:patient_id(Prescription),
          PharmacyId = prescription:pharmacy_id(Prescription),
          FacilityId = prescription:facility_id(Prescription),
          PrescriberId = prescription:prescriber_id(Prescription),
          %% gather required antidote keys
          PrescriptionKey = binary_prescription_key(Id),
          PatientKey = binary_patient_key(PatientId),
          PharmacyKey = binary_pharmacy_key(PharmacyId),
          FacilityKey = binary_facility_key(FacilityId),
          PrescriberKey = binary_staff_key(PrescriberId),
          %% build nested updates for patients, pharmacies, facilities and the prescriber
          PatientUpdate = patient:process_prescription(Id,CurrentDate),
          FacilityUpdate = facility:process_prescription(Id,CurrentDate),
          PharmacyUpdate = pharmacy:process_prescription(Id,CurrentDate),
          PrescriberUpdate = staff:process_prescription(Id,CurrentDate),
          %% update top level prescription
          antidote_lib:put(PrescriptionKey,?MAP,update,UpdateOperation,Txn),
          %% add to patient prescriptions
          antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,Txn),
          %% add to hospital prescriptions
          antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,Txn),
          %% add to pharmacy prescriptions
          antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,Txn),
          %% add to the prescriber's prescriptions
          antidote_lib:put(PrescriberKey,?MAP,update,PrescriberUpdate,Txn),
          ok
      end
  end,
  ok = antidote_lib:txn_commit(Txn),
  Result.



%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------

-spec concatenate_id(atom(),integer()) -> list().
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
  concatenate_list_with_id("event_~p",Id).

concatenate_list_with_id(List,Id) ->
  lists:flatten(io_lib:format(List, [Id])).

search_patient_index(Name,Txn) ->
  search_fmk_index(patient,Name,Txn).

search_pharmacy_index(Name,Txn) ->
  search_fmk_index(pharmacy,Name,Txn).

search_facility_index(Name,Txn) ->
  search_fmk_index(facility,Name,Txn).

search_staff_index(Name,Txn) ->
  search_fmk_index(staff,Name,Txn).

search_fmk_index(staff,Name,Txn) ->
  fmk_index:search_index(Name,fmk_index:get_staff_name_index(Txn));
search_fmk_index(facility,Name,Txn) ->
  fmk_index:search_index(Name,fmk_index:get_facility_name_index(Txn));
search_fmk_index(patient,Name,Txn) ->
  fmk_index:search_index(Name,fmk_index:get_patient_name_index(Txn));
search_fmk_index(pharmacy,Name,Txn) ->
  fmk_index:search_index(Name,fmk_index:get_pharmacy_name_index(Txn)).

check_prescription_id(Id,Txn) ->
  case get_prescription_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_staff_id(Id,Txn) ->
  case get_staff_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_patient_id(Id,Txn) ->
  case get_patient_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_pharmacy_id(Id,Txn) ->
  case get_pharmacy_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_facility_id(Id,Txn) ->
  case get_facility_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_treatment_id(Id,Txn) ->
  case get_treatment_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

check_event_id(Id,Txn) ->
  case get_event_by_id(Id,Txn) of
    {error,not_found} -> free;
    _Map  -> taken
  end.

process_get_request(Key,Type) ->
  ReadResult = antidote_lib:get(Key,Type),
  case ReadResult of
    [] -> {error,not_found};
    Object -> Object
  end.

process_get_request(Key,Type,Txn) ->
  ReadResult = antidote_lib:get(Key,Type,Txn),
  case ReadResult of
    [] -> {error,not_found};
    Object -> Object
  end.

filter_processed_prescriptions(PharmacyPrescriptions) ->
  [Prescription || {_PrescriptionHeader,Prescription} <- PharmacyPrescriptions, prescription:is_processed(Prescription)==?PRESCRIPTION_PROCESSED].
