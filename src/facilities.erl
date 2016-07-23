-module(facilities).
-include("fmk.hrl").

%% Functions to handle single Facility objects
-export ([
  new/4,
  update/2,
  name/1,
  id/1,
  address/1,
  type/1,
  prescriptions/1
  ]).

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string(), Type::nonempty_string()) -> riak_dt_map:map_op().
new(Id,Name,Address,Type) ->
  IdOp = antidote_lib:build_map_op(id,riak_dt_gcounter,{increment,Id}),
  NameOp = antidote_lib:build_map_op(name,riak_dt_lwwreg,{assign, list_to_binary(Name)}),
  AddressOp = antidote_lib:build_map_op(address,riak_dt_lwwreg,{assign, list_to_binary(Address)}),
  TypeOp = antidote_lib:build_map_op(type,riak_dt_lwwreg,{assign, list_to_binary(Type)}),
  %% build nested map operations
  PrescriptionsMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  PrescriptionsOp = antidote_lib:build_map_op(prescriptions,riak_dt_map,PrescriptionsMapOp),
  %% put everything in a big bulky map update and return it
  antidote_lib:build_map_update([IdOp,NameOp,AddressOp,TypeOp,PrescriptionsOp]).

-spec update(Id::pos_integer(), FacilityUpdate::riak_dt_map:map_op()) -> {ok,_Something}.
update(Id,FacilityUpdate) ->
  antidote_lib:put(Id,riak_dt_map,{FacilityUpdate}).

-spec name(Facility::riak_dt_map:map()) -> string().
name(Facility) ->
  case antidote_lib:findkey(Facility,name,riak_dt_lwwreg) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

-spec type(Facility::riak_dt_map:map()) -> string().
type(Facility) ->
  case antidote_lib:findkey(Facility,type,riak_dt_lwwreg) of
    not_found -> "";
    Type -> binary_to_list(Type)
  end.

-spec id(Facility::riak_dt_map:map()) -> pos_integer().
id(Facility) ->
  case antidote_lib:findkey(Facility,id,riak_dt_gcounter) of
    not_found -> 0;
    Id -> Id
  end.

-spec address(Facility::riak_dt_map:map()) -> string().
address(Facility) ->
  case antidote_lib:findkey(Facility,address,riak_dt_lwwreg) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

-spec prescriptions(Facility::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Facility) ->
  case antidote_lib:findkey(Facility,prescriptions,riak_dt_map) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.