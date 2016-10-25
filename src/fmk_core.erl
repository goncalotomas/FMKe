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
  get_event_by_id/2,
  get_facility_by_id/1,
  get_facility_by_id/2,
  get_facility_by_name/1,
  get_facility_prescriptions/1,
  get_facility_treatments/1,
  get_patient_by_id/1,
  get_patient_by_id/2,
  get_patient_by_name/1,
  get_pharmacy_by_id/1,
  get_pharmacy_by_id/2,
  get_pharmacy_by_name/1,
  get_pharmacy_prescriptions/1,
  get_prescription_by_id/1,
  get_prescription_by_id/2,
  get_staff_by_id/1,
  get_staff_by_id/2,
  get_staff_by_name/1,
  get_staff_prescriptions/1,
  get_staff_treatments/1,
  get_treatment_by_id/1,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  update_staff_details/4
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
-spec create_patient(id(),binary(),binary()) -> ok | {error, reason()}.
create_patient(Id,Name,Address) ->
  Txn = antidote_lib:txn_start(),
  case get_patient_by_id(Id,Txn) of
    {error,not_found} ->
      Patient = patient:new(Id,Name,Address),
      PatientKey = binary_patient_key(Id),
      ok = fmk_index:index_patient(concatenate_id(patient,Id),Name),
      ok = antidote_lib:put_map(PatientKey,?MAP,update,Patient,node(),Txn),
      ok = antidote_lib:txn_commit(Txn);
    _Patient ->
      {error, patient_id_taken}
  end.

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet
%% been seen. If the operation succeeds, the pharmacy will be indexed by both
%% Name and ID.
-spec create_pharmacy(id(),binary(),binary()) -> ok | {error, reason()}.
create_pharmacy(Id,Name,Address) ->
  Txn = antidote_lib:txn_start(),
  case get_pharmacy_by_id(Id,Txn) of
    {error,not_found} ->
      Pharmacy = pharmacy:new(Id,Name,Address),
      PharmacyKey = binary_pharmacy_key(Id),
      ok = fmk_index:index_pharmacy(concatenate_id(pharmacy,Id),Name),
      ok = antidote_lib:put_map(PharmacyKey,?MAP,update,Pharmacy,node(),Txn),
      ok = antidote_lib:txn_commit(Txn);
    _Pharmacy ->
      {error, pharmacy_id_taken}
  end.

%% Adds a facility to the FMK-- system if the ID for the facility has not yet
%% been seen. If the operation succeeds, the facility will be indexed by both
%% Name and ID.
-spec create_facility(id(),binary(),binary(),binary()) -> ok | {error, reason()}.
create_facility(Id,Name,Address,Type) ->
  Txn = antidote_lib:txn_start(),
  case get_facility_by_id(Id) of
    {error,not_found} ->
      Facility = facility:new(Id,Name,Address,Type),
      FacilityKey = binary_facility_key(Id),
      ok = fmk_index:index_facility(concatenate_id(facility,Id),Name),
      ok = antidote_lib:put_map(FacilityKey,?MAP,update,Facility,node(),Txn),
      ok = antidote_lib:txn_commit(Txn);
    _Facility ->
      {error, facility_id_taken}
  end.

%% Adds a staff member to the FMK-- system if the ID for the member has not yet
%% been seen. If the operation succeeds, the staff member will be indexed by both
%% Name and ID.
-spec create_staff(id(),binary(),binary(),binary()) -> ok | {error, reason()}.
create_staff(Id,Name,Address,Speciality) ->
  case get_staff_by_id(Id) of
    {error,not_found} ->
      Staff = staff:new(Id,Name,Address,Speciality),
      StaffKey = binary_staff_key(Id),
      ok = fmk_index:index_staff(concatenate_id(staff,Id),Name),
      ok = antidote_lib:put(StaffKey,?MAP,update,Staff,node());
    _Facility ->
      {error, staff_id_taken}
  end.

%% Fetches a treatment event by id.
-spec get_event_by_id(id()) -> [crdt()] | {error, reason()}.
get_event_by_id(Id) ->
  Event = antidote_lib:get(binary_event_key(Id),?MAP),
  case Event of
    [] -> {error,not_found};
    EventMap -> EventMap
  end.

%% Alternative to get_event_by_id/1, which includes a transactional context.
-spec get_event_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_event_by_id(Id,Txn) ->
  process_get_request(binary_event_key(Id),?MAP,Txn).


%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) ->
  Facility = antidote_lib:get(binary_facility_key(Id),?MAP),
  case Facility of
    [] -> {error,not_found};
    FacilityMap -> FacilityMap
  end.

%% Alternative to get_facility_by_id/1, which includes a transactional context.
-spec get_facility_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id,Txn) ->
  process_get_request(binary_facility_key(Id),?MAP,Txn).

%% Fetches a facility by name.
-spec get_facility_by_name(binary()) -> [crdt()] | {error, reason()}.
get_facility_by_name(Name) ->
  case search_facility_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

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
  Pharmacy = antidote_lib:get(binary_pharmacy_key(Id),?MAP),
  case Pharmacy of
    [] -> {error,not_found};
    PharmacyMap -> PharmacyMap
  end.

%% Alternative to get_pharmacy_by_id/1, which includes a transactional context.
-spec get_pharmacy_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id,Txn) ->
  process_get_request(binary_pharmacy_key(Id),?MAP,Txn).

%% Fetches a patient by ID.
-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) ->
  Patient = antidote_lib:get(binary_patient_key(Id),?MAP),
  case Patient of
    [] -> {error,not_found};
    PatientMap -> PatientMap
  end.

%% Alternative to get_patient_by_id/1, which includes a transactional context.
-spec get_patient_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id,Txn) ->
  process_get_request(binary_patient_key(Id),?MAP,Txn).

%% Fetches a patient by name.
-spec get_patient_by_name(binary()) -> [crdt()] | {error, reason()}.
get_patient_by_name(Name) ->
  case search_patient_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

%% Fetches a pharmacy by name.
-spec get_pharmacy_by_name(binary()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_name(Name) ->
  case search_pharmacy_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(PharmacyId) ->
  case get_pharmacy_by_id(PharmacyId) of
    {error,not_found} -> {error,no_such_pharmacy};
    Pharmacy -> pharmacy:prescriptions(Pharmacy)
  end.

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
  Prescription = antidote_lib:get(binary_prescription_key(Id),?MAP),
  case Prescription of
    [] -> {error,not_found};
    PrescriptionMap -> PrescriptionMap
  end.

%% Alternative to get_prescription_by_id/1, which includes a transactional context.
-spec get_prescription_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id,Txn) ->
  process_get_request(binary_prescription_key(Id),?MAP,Txn).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) ->
  Staff = antidote_lib:get(binary_staff_key(Id),?MAP),
  case Staff of
    [] -> {error,not_found};
    StaffMap -> StaffMap
  end.

%% Alternative to get_staff_by_id/1, which includes a transactional context.
-spec get_staff_by_id(id(),id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id,Txn) ->
  process_get_request(binary_staff_key(Id),?MAP,Txn).

%% Fetches a staff member by name.
-spec get_staff_by_name(binary()) -> [crdt()] | {error, reason()}.
get_staff_by_name(Name) ->
  case search_staff_index(list_to_binary(Name)) of
    not_found -> not_found;
    [H] -> antidote_lib:get(H,?MAP);
    List -> [antidote_lib:get(Result,?MAP) || Result <- List]
  end.

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
  Treatment = antidote_lib:get(binary_treatment_key(Id),?MAP),
  case Treatment of
    [] -> {error,not_found};
    TreatmentMap -> TreatmentMap
  end.

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(),binary(),binary()) -> ok | {error, reason()}.
update_patient_details(Id,Name,Address) ->
  case get_patient_by_id(Id) of
    {error,not_found} ->
      {error,no_such_patient};
    Patient ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      PatientKey = binary_patient_key(Id),
      PatientName = patient:name(Patient),
      PatientUpdate = patient:update_details(Name,Address),
      antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,node()),
      case string:equal(PatientName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_patient(concatenate_id(patient,Id),PatientName,Name)
      end
  end.

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(),binary(),binary()) -> ok | {error, reason()}.
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
      antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,node()),
      case string:equal(PharmacyName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_pharmacy(concatenate_id(pharmacy,Id),PharmacyName,Name)
      end
  end.

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(),binary(),binary(),binary()) -> ok | {error, reason()}.
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
      antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,node()),
      case string:equal(FacilityName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_facility(concatenate_id(facility,Id),FacilityName,Name)
      end
  end.

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(),binary(),binary(),binary()) -> ok | {error, reason()}.
update_staff_details(Id,Name,Address,Speciality) ->
  case get_staff_by_id(Id) of
    {error,not_found} ->
      {error,no_such_staff_member};
    Staff ->
      %% Patient already exists, prepare update operation and check if
      %% we need to re-index him/her.
      StaffKey = binary_pharmacy_key(Id),
      StaffName = staff:name(Staff),
      StaffUpdate = staff:update_details(Name,Address,Speciality),
      antidote_lib:put(StaffKey,?MAP,update,StaffUpdate,node()),
      case string:equal(StaffName,Name) of
        true ->
          ok;
        false ->
          ok = fmk_index:reindex_staff(concatenate_id(staff,Id),StaffName,Name)
      end
  end.

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy and treatment facility (hospital). The prescription also includes the prescription date
%% and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), id(), binary(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  %% check required pre-conditions
  %% TODO I'm performing a get for each operation below, how could it be improved?
  free = check_prescription_id(PrescriptionId),
  taken = check_patient_id(PatientId),
  taken = check_staff_id(PrescriberId),
  taken = check_pharmacy_id(PharmacyId),
  taken = check_facility_id(FacilityId),
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
  antidote_lib:put(PrescriptionKey,?MAP,update,TopLevelPrescription,node()),
  %% add to pharmaciy prescriptions
  antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,node()),
  %% add to the prescriber's prescriptions
  antidote_lib:put(PrescriberKey,?MAP,update,PrescriberUpdate,node()),
  %% add to patient prescriptions
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,node()),
  %% add to hospital prescriptions
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,node()),
  ok.

%% Same as create_prescription/7, but includes a reference to the treatment to which the
%% prescription is associated with.
-spec create_prescription(id(), id(), id(), id(), id(), id(), binary(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId,TreatmentId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  %% check required pre-conditions
  %% TODO I'm performing a get for each operation below, how could it be improved?
  free = check_prescription_id(PrescriptionId),
  taken = check_patient_id(PatientId),
  taken = check_staff_id(PrescriberId),
  taken = check_pharmacy_id(PharmacyId),
  taken = check_facility_id(FacilityId),
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
  antidote_lib:put(PrescriptionKey,?MAP,update,TopLevelPrescription,node()),
  %% add to patient prescriptions
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,node()),
  %% add to hospital prescriptions
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,node()),
  %% add to pharmaciy prescriptions
  antidote_lib:put(PharmacyKey,?MAP,update,PharmacyUpdate,node()),
  %% add to the prescriber's prescriptions
  antidote_lib:put(PrescriberKey,?MAP,update,PrescriberUpdate,node()),
  %% add to the treatment's prescriptions
  antidote_lib:put(TreatmentKey,?MAP,update,TreatmentUpdate,node()),
  ok.

%% Creates a treatment event, with information about the staff member that registered it,
%% along with a timestamp and description.
-spec create_event(id(), id(), id(), binary(), binary()) -> ok | {error, reason()}.
create_event(EventId,TreatmentId,StaffMemberId,Timestamp,Description) ->
  %% check required pre-conditions
  free = check_event_id(EventId),
  taken = check_treatment_id(TreatmentId),
  taken = check_staff_id(StaffMemberId),
  TreatmentObject = get_treatment_by_id(TreatmentId),
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
  antidote_lib:put(EventKey,?MAP,update,TopLevelEvent,node()),
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,node()),
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,node()),
  antidote_lib:put(TreatmentKey,?MAP,update,TreatmentUpdate,node()),
  ok.

%% Creates a treatment with information about the patient, the staff member that iniciated it,
%% and also the facility ID and date when the treatment started.
-spec create_treatment(id(), id(), id(), id(), binary()) -> ok | {error, reason()}.
create_treatment(TreatmentId,PatientId,StaffId,FacilityId,DateStarted) ->
  %% check required pre-conditions
  %% TODO I'm performing a get for each operation below, how could it be improved?
  free = check_treatment_id(TreatmentId),
  taken = check_patient_id(PatientId),
  taken = check_staff_id(StaffId),
  taken = check_facility_id(FacilityId),
  %% gather required antidote keys
  TreatmentKey = binary_treatment_key(TreatmentId),
  PatientKey = binary_patient_key(PatientId),
  FacilityKey = binary_facility_key(FacilityId),
  %% build top level update for treatment objects
  TopLevelTreatment = treatment:new(TreatmentId,PatientId,StaffId,FacilityId,DateStarted),
  %% build nested updates for facilities and patients
  PatientUpdate = patient:add_treatment(TreatmentId,StaffId,FacilityId,DateStarted),
  FacilityUpdate = facility:add_treatment(TreatmentId,PatientId,StaffId,DateStarted),
  %% TODO should everything happen in a single transaction? Discuss with Nuno and Joao
  %% add top level treatment
  antidote_lib:put(TreatmentKey,?MAP,update,TopLevelTreatment,node()),
  %% add to patient treatments
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,node()),
  %% add to facility treatments
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,node()),
  ok.

%% Same as create_treatment/5, but includes an ending date for the treatment.
-spec create_treatment(id(), id(), id(), id(), binary(), binary()) -> ok | {error, reason()}.
create_treatment(TreatmentId,PatientId,StaffId,FacilityId,DateStarted,DateEnded) ->
  %% check required pre-conditions
  %% TODO I'm performing a get for each operation below, how could it be improved?
  free = check_treatment_id(TreatmentId),
  taken = check_patient_id(PatientId),
  taken = check_staff_id(StaffId),
  taken = check_facility_id(FacilityId),
  %% gather required antidote keys
  TreatmentKey = binary_treatment_key(TreatmentId),
  PatientKey = binary_patient_key(PatientId),
  FacilityKey = binary_facility_key(FacilityId),
  %% build top level update for treatment objects
  TopLevelTreatment = treatment:new(TreatmentId,PatientId,StaffId,FacilityId,DateStarted,DateEnded),
  %% build nested updates for facilities and patients
  PatientUpdate = patient:add_treatment(TreatmentId,StaffId,FacilityId,DateStarted,DateEnded),
  FacilityUpdate = facility:add_treatment(TreatmentId,PatientId,StaffId,DateStarted,DateEnded),
  %% TODO should everything happen in a single transaction? Discuss with Nuno and Joao
  %% add top level treatment
  antidote_lib:put(TreatmentKey,?MAP,update,TopLevelTreatment,node()),
  %% add to patient treatments
  antidote_lib:put(PatientKey,?MAP,update,PatientUpdate,node()),
  %% add to facility treatments
  antidote_lib:put(FacilityKey,?MAP,update,FacilityUpdate,node()),
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

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------

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

check_prescription_id(Id) ->
  case get_prescription_by_id(Id) of
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

process_get_request(Key,Type,Txn) ->
  ReadResult = antidote_lib:get(Key,Type,Txn),
  case ReadResult of
    [] -> {error,not_found};
    Object -> Object
  end.
