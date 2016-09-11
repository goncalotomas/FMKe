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

new(Id,Name,Address) ->
  IdOp = antidote_lib:build_map_op(?PHARMACY_ID,?PHARMACY_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  [IdOp,NameOp,AddressOp].

update_details(Name,Address) ->
  NameOp = antidote_lib:build_map_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  [NameOp,AddressOp].

%% Returns the name of the pharmacy in the form of a list.
-spec name(Pharmacy::riak_dt_map:map()) -> string().
name(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,?PHARMACY_NAME,?PHARMACY_NAME_CRDT) of
    not_found -> {error,not_found};
    Name -> binary_to_list(Name)
  end.

%% Returns the ID of the pharmacy in the form of an integer.
-spec id(Pharmacy::riak_dt_map:map()) -> pos_integer().
id(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,?PHARMACY_ID,?PHARMACY_ID_CRDT) of
    not_found -> {error,not_found};
    Id -> Id
  end.

%% Returns the address of the pharmacy in the form of a list.
-spec address(Pharmacy::riak_dt_map:map()) -> string().
address(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT) of
    not_found -> {error,not_found};
    Address -> binary_to_list(Address)
  end.

%% Returns a orset of prescriptions keys (index) associated with this pharmacy object.
-spec prescriptions(Pharmacy::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,?PHARMACY_PRESCRIPTIONS,?PHARMACY_PRESCRIPTIONS_CRDT) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

add_prescription(PrescriptionId,PatientId,PrescriberId,FacilityId,DatePrescribed,Drugs) ->
  PrescriptionIdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(PrescriptionId)),
  PatientIdOp = antidote_lib:build_map_op(?PRESCRIPTION_PATIENT_ID,?PRESCRIPTION_PATIENT_ID_CRDT,antidote_lib:counter_increment(PatientId)),
  PrescriberIdOp = antidote_lib:build_map_op(?PRESCRIPTION_PRESCRIBER_ID,?PRESCRIPTION_PRESCRIBER_ID_CRDT,antidote_lib:counter_increment(PrescriberId)),
  FacilityIdOp = antidote_lib:build_map_op(?PRESCRIPTION_FACILITY_ID,?PRESCRIPTION_FACILITY_ID_CRDT,antidote_lib:counter_increment(FacilityId)),
  DateStartedOp = antidote_lib:build_map_op(?PRESCRIPTION_DATE_PRESCRIBED,?PRESCRIPTION_DATE_PRESCRIBED_CRDT,antidote_lib:lwwreg_assign(list_to_binary(DatePrescribed))),
  [DrugsOp] = prescription:add_drugs(Drugs),
  ListOps = [PrescriptionIdOp,PatientIdOp,PrescriberIdOp,FacilityIdOp,DateStartedOp,DrugsOp],
  PharmacyPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  PharmacyPrescriptionsOp = antidote_lib:build_nested_map_op(?PHARMACY_PRESCRIPTIONS,?NESTED_MAP,PharmacyPrescriptionsKey,ListOps),
  [PharmacyPrescriptionsOp].