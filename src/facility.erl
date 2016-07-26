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
  ]).

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string(), Type::nonempty_string()) -> riak_dt_map:map_op().
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

-spec name(Facility::riak_dt_map:map()) -> string().
name(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_NAME,?FACILITY_NAME_CRDT) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

-spec type(Facility::riak_dt_map:map()) -> string().
type(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_TYPE,?FACILITY_TYPE_CRDT) of
    not_found -> "";
    Type -> binary_to_list(Type)
  end.

-spec id(Facility::riak_dt_map:map()) -> pos_integer().
id(Facility) ->
  case antidote_lib:find_key(Facility,?FACILITY_ID,?FACILITY_ID) of
    not_found -> 0;
    Id -> Id
  end.

-spec address(Facility::riak_dt_map:map()) -> string().
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