%% This module represents the staff member entity in the FMK system.
%% Staff members are associated with prescriptions.
-module (staff).
-include("fmk.hrl").

%% Functions to handle single Staff objects
-export ([
  new/4,
  update_details/3,
  name/1,
  id/1,
  address/1,
  speciality/1,
  treatments/1,
  prescriptions/1,
  add_prescription/6,
  process_prescription/2,
  add_prescription_drugs/2
  ]).

%% Creates a new staff member object from an ID, Name, Address and Speciality.
%% Returns an update operation ready to insert into Antidote
-spec new(id(),string(),string(),string()) -> [term()].
new(Id,Name,Address,Speciality) ->
  IdOp = build_id_op(?STAFF_ID,?STAFF_ID_CRDT,Id),
  NameOp = build_lwwreg_op(?STAFF_NAME,?STAFF_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?STAFF_ADDRESS,?STAFF_ADDRESS_CRDT,Address),
  SpecialityOp = build_lwwreg_op(?STAFF_SPECIALITY,?STAFF_SPECIALITY_CRDT,Speciality),
  %% initially a staff member does not have any treatments or prescriptions
  [IdOp,NameOp,AddressOp,SpecialityOp].


%% Update operation: updates only the staff member's personal details
-spec update_details(string(),string(),string()) -> [term()].
update_details(Name,Address,Speciality) ->
  NameOp = build_lwwreg_op(?STAFF_NAME,?STAFF_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?STAFF_ADDRESS,?STAFF_ADDRESS_CRDT,Address),
  SpecialityOp = build_lwwreg_op(?STAFF_SPECIALITY,?STAFF_SPECIALITY_CRDT,Speciality),
  [NameOp,AddressOp,SpecialityOp].

%% Returns the name in the form of a list from a staff member object.
-spec name(crdt()) -> string().
name(Staff) ->
  binary_to_list(antidote_lib:find_key(Staff,?STAFF_NAME,?STAFF_NAME_CRDT)).

%% Returns the id in the form of an integer from a staff member object.
-spec id(crdt()) -> id().
id(Staff) ->
  antidote_lib:find_key(Staff,?STAFF_ID,?STAFF_ID_CRDT).

%% Returns the address in the form of a list from a staff member object.
-spec address(crdt()) -> string().
address(Staff) ->
  binary_to_list(antidote_lib:find_key(Staff,?STAFF_ADDRESS,?STAFF_ADDRESS_CRDT)).

%% Returns the members' speciality as a Riak map from a staff member object
-spec speciality(crdt()) -> string().
speciality(Staff) ->
  binary_to_list(antidote_lib:find_key(Staff,?STAFF_SPECIALITY,?STAFF_SPECIALITY_CRDT)).

%% Returns the treatments as a Riak map from a staff member object
-spec treatments(crdt()) -> term().
treatments(Staff) ->
  antidote_lib:find_key(Staff,?STAFF_TREATMENTS,?STAFF_TREATMENTS_CRDT).

%% Returns the prescriptions as a Riak map from a staff member object
-spec prescriptions(crdt()) -> term().
prescriptions(Staff) ->
  antidote_lib:find_key(Staff,?STAFF_PRESCRIPTIONS,?STAFF_PRESCRIPTIONS_CRDT).

%% Returns an update operation for adding a prescription to a specific staff member.
-spec add_prescription(id(), id(), id(), id(), string(), [crdt()]) -> [term()].
add_prescription(PrescriptionId,PatientId,PharmacyId,FacilityId,DatePrescribed,Drugs) ->
  %% nested prescription operations
  PrescriptionIdOp = build_id_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,PrescriptionId),
  PatientIdOp = build_id_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,PatientId),
  PharmacyIdOp = build_id_op(?PRESCRIPTION_PHARMACY_ID,?PRESCRIPTION_PHARMACY_ID_CRDT,PharmacyId),
  FacilityIdOp = build_id_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,FacilityId),
  DateStartedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,DatePrescribed),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PatientIdOp,PharmacyIdOp,FacilityIdOp,DateStartedOp,DrugsOp],
  %% now to insert the nested operations inside the prescriptions map
  StaffPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level staff member update that contains the prescriptions map update

  StaffPrescriptionsOp = {{?STAFF_PRESCRIPTIONS, ?NESTED_RMAP}, {update,{{StaffPrescriptionsKey,?NESTED_RMAP}, {update, ListOps}}}},

  %StaffPrescriptionsOp = antidote_lib:build_nested_map_op(?STAFF_PRESCRIPTIONS,?NESTED_RMAP,StaffPrescriptionsKey,ListOps),
  [StaffPrescriptionsOp].

-spec process_prescription(id(), string()) -> [term()].
process_prescription(PrescriptionId, _CurrentDate) ->
  %% remove the prescription from the prescriptions map
  %PrescriptionRemove = antidote_lib:map_remove_elements([PrescriptionId],?NESTED_RMAP),
  StaffPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update

  StaffPrescriptionsOp = {{?STAFF_PRESCRIPTIONS, ?NESTED_RMAP}, {remove,{StaffPrescriptionsKey,?NESTED_RMAP}}},

  %% StaffPrescriptionsOp = antidote_lib:build_nested_map_op(?STAFF_PRESCRIPTIONS,?NESTED_RMAP,StaffPrescriptionsKey,[PrescriptionRemove]),
  [StaffPrescriptionsOp].

-spec add_prescription_drugs(id(), [string()]) -> [term()].
add_prescription_drugs(PrescriptionId, Drugs) ->
  PrescriptionUpdate = prescription:add_drugs(Drugs),
  %% now to insert the nested operations inside the prescriptions map
  StaffPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  StaffPrescriptionsOp = antidote_lib:build_nested_map_op(?STAFF_PRESCRIPTIONS,?NESTED_RMAP,
  StaffPrescriptionsKey,PrescriptionUpdate),
  [StaffPrescriptionsOp].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key,KeyType,Id) ->
  build_lwwreg_op(Key,KeyType,integer_to_list(Id)).

build_lwwreg_op(Key,KeyType,Value) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:lwwreg_assign(Value)).
