%% This module represents the pharmacy entity in the FMK system.
%% Pharmacies have associated prescriptions.
-module(pharmacy).
-include("fmk.hrl").

%% Functions to handle single Pharmacy objects
-export ([
  new/3,
  name/1,
  id/1,
  address/1,
  prescriptions/1,
  update_details/2,
  add_prescription/6
  ]).

%% Returns a list of operations ready to be inserted into antidote.
%% All Ids must be of type pos_integer() and name and address should be binary()
new(Id,Name,Address) ->
  IdOp = build_id_op(?PHARMACY_ID,?PHARMACY_ID_CRDT,Id),
  NameOp = build_lwwreg_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,Address),
  [IdOp,NameOp,AddressOp].

%% Returns a list of operations ready to be inserted into antidote, with the purpose
%% of updating a specific pharmacy's details.
update_details(Name,Address) ->
  NameOp = build_lwwreg_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,Name),
  AddressOp = build_lwwreg_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,Address),
  [NameOp,AddressOp].

%% Returns the name of the pharmacy in the form of a list.
name(Pharmacy) ->
  antidote_lib:find_key(Pharmacy,?PHARMACY_NAME,?PHARMACY_NAME_CRDT).

%% Returns the ID of the pharmacy in the form of an integer.
id(Pharmacy) ->
  antidote_lib:find_key(Pharmacy,?PHARMACY_ID,?PHARMACY_ID_CRDT).

%% Returns the address of the pharmacy in the form of a list.
address(Pharmacy) ->
  antidote_lib:find_key(Pharmacy,?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT).

%% Returns a orset of prescriptions keys (index) associated with this pharmacy object.
prescriptions(Pharmacy) ->
  antidote_lib:find_key(Pharmacy,?PHARMACY_PRESCRIPTIONS,?PHARMACY_PRESCRIPTIONS_CRDT).

%% Returns an update operation for adding a prescription to a specific pharmacy.
add_prescription(PrescriptionId,PatientId,PrescriberId,FacilityId,DatePrescribed,Drugs) ->
  %% nested prescription operations
  PrescriptionIdOp = build_id_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,PrescriptionId),
  PatientIdOp = build_id_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,PatientId),
  PrescriberIdOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,PrescriberId),
  FacilityIdOp = build_id_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,FacilityId),
  DateStartedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,DatePrescribed),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DateStartedOp,DrugsOp],
  %% now to insert the nested operations inside the prescriptions map
  PharmacyPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level pharmacy update that contains the prescriptions map update
  PharmacyPrescriptionsOp = antidote_lib:build_nested_map_op(?PHARMACY_PRESCRIPTIONS,?NESTED_MAP,PharmacyPrescriptionsKey,ListOps),
  [PharmacyPrescriptionsOp].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key,KeyType,Id) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:counter_increment(Id)).

build_lwwreg_op(Key,KeyType,Value) ->
  antidote_lib:build_map_op(Key,KeyType,antidote_lib:lwwreg_assign(Value)).
