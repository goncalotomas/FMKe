-module(fmk_core).
-include("fmk.hrl").

-export([
  create_patient/3,
  create_pharmacy/3,
  create_facility/4,
  create_staff/4,
  get_patient/1,
  get_pharmacy/1,
  get_facility/1,
  get_staff/1,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  %update_pharmacy/3,
  %update_facility/3,
  add_prescription/1
  ]).

%% Export needed for other modules
-export ([
    concatenate_patient_id/1,
    concatenate_pharmacy_id/1,
    concatenate_facility_id/1,
    concatenate_staff_id/1
  ]).

create_patient(Id,Name,Address) ->
  case get_patient(Id) of
    {error,not_found} ->
      Patient = patient:new(Id,Name,Address),
      PatientKey = binary_patient_key(Id),
      ok = fmk_index:index_patient(Id,Name),
      ok = antidote_lib:put(PatientKey,?MAP,update,Patient,fmk);
    _Patient ->
      {error, patient_id_taken}
  end.

create_pharmacy(Id,Name,Address) ->
  case get_pharmacy(Id) of
    {error,not_found} ->
      Pharmacy = pharmacy:new(Id,Name,Address),
      PharmacyKey = binary_pharmacy_key(Id),
      ok = fmk_index:index_pharmacy(Id,Name),
      ok = antidote_lib:put(PharmacyKey,?MAP,update,Pharmacy,fmk);
    _Pharmacy ->
      {error, pharmacy_id_taken}
  end.

create_facility(Id,Name,Address,Type) ->
  case get_facility(Id) of
    {error,not_found} ->
      Facility = facility:new(Id,Name,Address,Type),
      FacilityKey = binary_facility_key(Id),
      ok = fmk_index:index_facility(Id,Name),
      ok = antidote_lib:put(FacilityKey,?MAP,update,Facility,fmk);
    _Facility ->
      {error, facility_id_taken}
  end.

create_staff(Id,Name,Address,Speciality) ->
  case get_staff(Id) of
    {error,not_found} ->
      Staff = staff:new(Id,Name,Address,Speciality),
      StaffKey = binary_staff_key(Id),
      ok = fmk_index:index_staff(Id,Name),
      ok = antidote_lib:put(StaffKey,?MAP,update,Staff,fmk);
    _Facility ->
      {error, staff_id_taken}
  end.

%% Finds a patient in the Antidote Key-Value store by Patient ID.
get_patient(Id) ->
  Patient = antidote_lib:get(binary_patient_key(Id),?MAP),
  case Patient of
    [] -> {error,not_found};
    PatientMap -> PatientMap
  end.

get_pharmacy(Id) ->
  Pharmacy = antidote_lib:get(binary_pharmacy_key(Id),?MAP),
  case Pharmacy of
    [] -> {error,not_found};
    PharmacyMap -> PharmacyMap
  end.

get_facility(Id) ->
  Facility = antidote_lib:get(binary_facility_key(Id),?MAP),
  case Facility of
    [] -> {error,not_found};
    FacilityMap -> FacilityMap
  end.

get_staff(Id) ->
  Staff = antidote_lib:get(binary_staff_key(Id),?MAP),
  case Staff of
    [] -> {error,not_found};
    StaffMap -> StaffMap
  end.

update_patient_details(Id,Name,Address) ->
  case get_patient(Id) of
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
  case get_pharmacy(Id) of
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
  case get_facility(Id) of
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

add_prescription(PatientId) ->
  Patient = get_patient(PatientId),
  PatientId = antidote_lib:find_key(Patient,?PATIENT_ID,?PATIENT_ID_CRDT).
  %% TODO complete

binary_patient_key(Id) ->
  list_to_binary(concatenate_patient_id(Id)).

binary_pharmacy_key(Id) ->
  list_to_binary(concatenate_pharmacy_id(Id)).

binary_facility_key(Id) ->
  list_to_binary(concatenate_facility_id(Id)).

binary_staff_key(Id) ->
  list_to_binary(concatenate_staff_id(Id)).

concatenate_patient_id(Id) ->
  lists:flatten(io_lib:format("patient~p", [Id])).

concatenate_pharmacy_id(Id) ->
  lists:flatten(io_lib:format("pharmacy~p", [Id])).

concatenate_facility_id(Id) ->
  lists:flatten(io_lib:format("facility~p", [Id])).

concatenate_staff_id(Id) ->
  lists:flatten(io_lib:format("staff_member~p", [Id])).