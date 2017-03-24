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
    get_facility_by_id/1,
    get_patient_by_id/1,
    get_pharmacy_by_id/1,
    get_processed_pharmacy_prescriptions/1,
    get_pharmacy_prescriptions/1,
    get_prescription_by_id/1,
    get_prescription_medication/1,
    get_staff_by_id/1,
    get_staff_prescriptions/1,
    get_staff_treatments/1,
    process_prescription/2,
    update_patient_details/3,
    update_pharmacy_details/3,
    update_facility_details/4,
    update_staff_details/4,
    update_prescription_medication/3,
    error_to_binary/1
  ]).

%% Exports needed for other modules
-export([
    binary_patient_key/1,
    binary_prescription_key/1,
    binary_facility_key/1,
    binary_staff_key/1,
    binary_pharmacy_key/1
]).

%%-----------------------------------------------------------------------------
%% Create functions - no transactional context
%%-----------------------------------------------------------------------------

%% Adds a patient to the FMK system, needing only an ID, Name and Address.
%% A check is done to determine if a patient with the given ID already exists,
%% and if so the operation fails.
-spec create_patient(id(),string(),string()) -> ok | {error, reason()}.
create_patient(Id,Name,Address) ->
    execute_op_no_txn_context(create_patient,[Id,Name,Address]).

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet
%% been seen.
-spec create_pharmacy(id(),string(),string()) -> ok | {error, reason()}.
create_pharmacy(Id,Name,Address) ->
    execute_op_no_txn_context(create_pharmacy,[Id,Name,Address]).

%% Adds a facility to the FMK-- system if the ID for the facility has not yet
%% been seen.
-spec create_facility(id(),string(),string(),string()) -> ok | {error, reason()}.
create_facility(Id,Name,Address,Type) ->
    execute_op_no_txn_context(create_facility,[Id,Name,Address,Type]).

%% Adds a staff member to the FMK-- system if the ID for the member has not yet
%% been seen.
-spec create_staff(id(),string(),string(),string()) -> ok | {error, reason()}.
create_staff(Id,Name,Address,Speciality) ->
    execute_op_no_txn_context(create_staff,[Id,Name,Address,Speciality]).

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy. The prescription also includes the prescription date and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  execute_op_no_txn_context(create_prescription,[
    PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs
  ]).

%%-----------------------------------------------------------------------------
%% Read functions - no transactional context
%%-----------------------------------------------------------------------------

%% Fetches a patient by ID.
-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) ->
    execute_get_op_no_txn_context(get_patient_by_id,[Id]).

%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) ->
    execute_get_op_no_txn_context(get_facility_by_id,[Id]).

%% Fetches a pharmacy by ID.
-spec get_pharmacy_by_id(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id) ->
    execute_get_op_no_txn_context(get_pharmacy_by_id,[Id]).

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
    execute_get_op_no_txn_context(get_prescription_by_id,[Id]).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(Id) ->
    execute_get_op_no_txn_context(get_pharmacy_prescriptions,[Id]).

-spec get_processed_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_processed_pharmacy_prescriptions(Id) ->
    execute_get_op_no_txn_context(get_processed_pharmacy_prescriptions,[Id]).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
get_prescription_medication(Id) ->
    case get_prescription_by_id(Id) of
      {error,not_found} -> {error,no_such_prescriptions};
      {{ok,PrescriptionObject}, DBContext} -> ?DB_DRIVER:get_prescription_medication(DBContext,PrescriptionObject)
    end.

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) ->
    execute_get_op_no_txn_context(get_staff_by_id,[Id]).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_staff_prescriptions(StaffId) ->
    case get_staff_by_id(StaffId) of
      {error,not_found} -> {error,no_such_facility};
      {{ok,StaffObject}, DBContext} -> ?DB_DRIVER:get_staff_prescriptions(DBContext,StaffObject)
    end.

%% Fetches a list of treatments given a certain staff member ID.
-spec get_staff_treatments(id()) -> [crdt()] | {error, reason()}.
get_staff_treatments(StaffId) ->
    case get_staff_by_id(StaffId) of
      {error,not_found} -> {error,no_such_facility};
      {{ok,StaffObject}, DBContext} -> ?DB_DRIVER:get_staff_treatments(DBContext,StaffObject)
    end.


%%-----------------------------------------------------------------------------
%% Update functions - no transactional context
%%-----------------------------------------------------------------------------

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(),string(),string()) -> ok | {error, reason()}.
update_patient_details(Id,Name,Address) ->
    execute_op_no_txn_context(update_patient_details,[Id,Name,Address]).

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(),string(),string()) -> ok | {error, reason()}.
update_pharmacy_details(Id,Name,Address) ->
    execute_op_no_txn_context(update_pharmacy_details,[Id,Name,Address]).

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(),string(),string(),string()) -> ok | {error, reason()}.
update_facility_details(Id,Name,Address,Type) ->
    execute_op_no_txn_context(update_facility_details,[Id,Name,Address,Type]).

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(),string(),string(),string()) -> ok | {error, reason()}.
update_staff_details(Id,Name,Address,Speciality) ->
    execute_op_no_txn_context(update_staff_details,[Id,Name,Address,Speciality]).

-spec update_prescription_medication(id(),atom(),[string()]) -> ok | {error, reason()}.
update_prescription_medication(Id,Operation,Drugs) ->
    execute_op_no_txn_context(update_prescription_medication,[Id,Operation,Drugs]).

%% Builds the patient key inside Antidote given the patient ID.
-spec binary_patient_key(id()) -> binary().
binary_patient_key(Id) when is_integer(Id) ->
    list_to_binary(concatenate_id(patient,Id)).

%% Builds the pharmacy key inside Antidote given the pharmacy ID.
-spec binary_pharmacy_key(id()) -> binary().
binary_pharmacy_key(Id) when is_integer(Id) ->
    list_to_binary(concatenate_id(pharmacy,Id)).

%% Builds the facility key inside Antidote given the facility ID.
-spec binary_facility_key(id()) -> binary().
binary_facility_key(Id) when is_integer(Id) ->
    list_to_binary(concatenate_id(facility,Id)).

%% Builds the staff member key inside Antidote given the staff member ID.
-spec binary_staff_key(id()) -> binary().
binary_staff_key(Id) when is_integer(Id) ->
    list_to_binary(concatenate_id(staff,Id)).

%% Builds the prescription key inside Antidote given the prescription ID.
-spec binary_prescription_key(id()) -> binary().
binary_prescription_key(Id) when is_integer(Id) ->
    list_to_binary(concatenate_id(prescription,Id)).

process_prescription(Id,Date) ->
    execute_op_no_txn_context(process_prescription,[Id,Date]).

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

error_to_binary(Reason) ->
    list_to_binary(lists:flatten(io_lib:format("~p", [Reason]))).

execute_get_op_no_txn_context(Op,Arguments) when is_atom(Op), is_list(Arguments) ->
    case execute_op_no_txn_context(Op,Arguments) of
      {ok,Object} -> Object;
      Error -> Error
    end.

execute_op_no_txn_context(Op,Arguments) when is_atom(Op), is_list(Arguments) ->
    {ok, DBContext} = ?DB_DRIVER:start_transaction({}),
    {Result, DBContext1} = execute_op_with_txn_context(Op,Arguments,DBContext),
    {ok, _DBContext2} = ?DB_DRIVER:commit_transaction(DBContext1), %% assumes TXN commits
    Result.

execute_op_with_txn_context(Op,Arguments,DBContext) when is_atom(Op), is_list(Arguments) ->
    {_Result, _DBContext1} = apply(?DB_DRIVER,Op,[DBContext] ++ Arguments).