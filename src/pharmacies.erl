-module(pharmacies).
-include("fmk.hrl").

%% Functions to handle single Pharmacy objects
-export ([
  new/3,
  update/3,
  update/4,
  name/1,
  id/1,
  address/1,
  prescriptions/1
  ]).

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string()) -> riak_dt_map:map_op().
new(Id,Name,Address) ->
  IdOp = antidote_lib:build_map_op(?PHARMACY_ID,?PHARMACY_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,antidote_lib:lwwreg_update(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,antidote_lib:lwwreg_update(list_to_binary(Address))),
  %% build nested map operations
  PrescriptionMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  PrescriptionsOp = antidote_lib:build_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PrescriptionMapOp),
  %% put everything in a big bulky map update and return it
  MapUpdate = antidote_lib:build_map_update([
    IdOp
    ,NameOp
    ,AddressOp
    ,PrescriptionsOp
  ]),
  antidote_lib:build_map_op(Id,?NESTED_MAP,MapUpdate).

-spec update(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string()) -> ?MAP:map_op().
update(Id,Name,Address) ->
  NameOp = antidote_lib:build_map_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,antidote_lib:lwwreg_update(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,antidote_lib:lwwreg_update(list_to_binary(Address))),
  %% build nested map operations
  PrescriptionMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  PrescriptionsOp = antidote_lib:build_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PrescriptionMapOp),
  %% put everything in a big bulky map update and return it
  MapUpdate = antidote_lib:build_map_update([
    NameOp
    ,AddressOp
    ,PrescriptionsOp
  ]),
  antidote_lib:build_map_op(Id,?NESTED_MAP,MapUpdate).

-spec update(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string(), Prescriptions::?NESTED_MAP:map_op()) -> ?MAP:map_op().
update(Id,Name,Address,_Prescriptions) ->
  NameOp = antidote_lib:build_map_op(?PHARMACY_NAME,?PHARMACY_NAME_CRDT,antidote_lib:lwwreg_update(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PHARMACY_ADDRESS,?PHARMACY_ADDRESS_CRDT,antidote_lib:lwwreg_update(list_to_binary(Address))),
  %% build nested map operations
  PrescriptionMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  PrescriptionsOp = antidote_lib:build_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PrescriptionMapOp),
  %% put everything in a big bulky map update and return it
  MapUpdate = antidote_lib:build_map_update([
    NameOp
    ,AddressOp
    ,PrescriptionsOp
  ]),
  antidote_lib:build_map_op(Id,?NESTED_MAP,MapUpdate).

%% Returns the name of the pharmacy in the form of a list.
-spec name(Pharmacy::riak_dt_map:map()) -> string().
name(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,?PHARMACY_NAME,riak_dt_lwwreg) of
    not_found -> {error,not_found};
    Name -> binary_to_list(Name)
  end.

%% Returns the ID of the pharmacy in the form of an integer.
-spec id(Pharmacy::riak_dt_map:map()) -> pos_integer().
id(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,id,riak_dt_gcounter) of
    not_found -> {error,not_found};
    Id -> Id
  end.

%% Returns the address of the pharmacy in the form of a list.
-spec address(Pharmacy::riak_dt_map:map()) -> string().
address(Pharmacy) ->
  case antidote_lib:find_key(Pharmacy,address,riak_dt_lwwreg) of
    not_found -> {error,not_found};
    Address -> binary_to_list(Address)
  end.

%% Returns a orset of prescriptions keys (index) associated with this pharmacy object.
-spec prescriptions(Pharmacy::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Pharmacy) ->
  case antidote_lib:findkey(Pharmacy,prescriptions,riak_dt_map) of
    not_found -> {error,not_found};
    Prescriptions -> Prescriptions
  end.