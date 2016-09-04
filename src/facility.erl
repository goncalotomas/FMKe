-module(facility).
-include("fmk.hrl").

%% Functions to handle single Facility objects
-export ([
  new/4,
  update_details/3,
  name/1,
  id/1,
  address/1,
  type/1,
  prescriptions/1,
  treatments/1,
  add_prescription/1,
  add_event/1
  ]).

new(Id,Name,Address,Type) ->
  IdOp = antidote_lib:build_map_op(?FACILITY_ID,?FACILITY_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?FACILITY_NAME,?FACILITY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  TypeOp = antidote_lib:build_map_op(?FACILITY_TYPE,?FACILITY_TYPE_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Type))),
  [IdOp,NameOp,AddressOp,TypeOp].


update_details(Name,Address,Type) ->
  NameOp = antidote_lib:build_map_op(?FACILITY_NAME,?FACILITY_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  TypeOp = antidote_lib:build_map_op(?FACILITY_TYPE,?FACILITY_TYPE_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Type))),
  [NameOp,AddressOp,TypeOp].

name(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_NAME,?FACILITY_NAME_CRDT) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

type(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_TYPE,?FACILITY_TYPE_CRDT) of
    not_found -> "";
    Type -> binary_to_list(Type)
  end.

id(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_ID,?FACILITY_ID) of
    not_found -> 0;
    Id -> Id
  end.

address(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_ADDRESS,?FACILITY_ADDRESS_CRDT) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

prescriptions(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_PRESCRIPTIONS,?FACILITY_PRESCRIPTIONS_CRDT) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

treatments(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_TREATMENTS,?FACILITY_TREATMENTS_CRDT) of
    not_found -> [];
    Treatments -> Treatments
  end.

add_prescription(Prescription) ->
  Id = prescription:id(Prescription),
  Drugs = prescription:drugs(Prescription),
  %% Make operations to insert a new nested map
  IdOp = antidote_lib:build_map_op(?PRESCRIPTION_ID,?PRESCRIPTION_ID_CRDT,antidote_lib:counter_increment(Id)),
  [DrugsOp] = prescription:add_drugs(Drugs),
  [IdOp,DrugsOp].


add_event(Event) ->
  Id = event:id(Event),
  Description = event:description(Event),
  Timestamp = event:timestamp(Event),
  %% Make operations to insert a new nested map
  IdOp = antidote_lib:build_map_op(?EVENT_ID,?EVENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  DescriptionOp = antidote_lib:build_map_op(?EVENT_DESCRIPTION,?EVENT_DESCRIPTION_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Description))),
  TimestampOp = antidote_lib:build_map_op(?EVENT_TIMESTAMP,?EVENT_TIMESTAMP_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Timestamp))),
  [IdOp,DescriptionOp,TimestampOp].