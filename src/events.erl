-module(events).
-include("fmk.hrl").

%% Functions to handle single Pharmacy objects
-export ([
  new/7,
  update/2,
  name/1,
  id/1,
  address/1,
  prescriptions/1
  ]).

-spec new(Id::pos_integer(), Patient::riak_dt_map:map(), Patient::riak_dt_map:map(),
  Patient::riak_dt_map:map(), Patient::riak_dt_map:map(), Timestamp::pos_integer(), 
  Description::nonempty_string()) -> riak_dt_map:map_op().
new(Id,_Patient,_Treatment,_Prescription,_Creator,_Timestamp,_Description) ->
  IdOp = antidote_lib:build_map_op(id,riak_dt_gcounter,{increment,Id}),
  %NameOp = antidote_lib:build_map_op(name,riak_dt_lwwreg,{assign, list_to_binary(Name)}),
  %AddressOp = antidote_lib:build_map_op(address,riak_dt_lwwreg,{assign, list_to_binary(Address)}),
  %% build nested map operations
  PrescriptionsMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  _PrescriptionsOp = antidote_lib:build_map_op(prescriptions,riak_dt_map,PrescriptionsMapOp),
  %% put everything in a big bulky map update and return it
  antidote_lib:build_map_update([IdOp]).%,NameOp,AddressOp,TypeOp,PrescriptionsOp]).

-spec update(Id::pos_integer(), PharmacyUpdate::riak_dt_map:map_op()) -> {ok,_Something}.
update(Id,PharmacyUpdate) ->
  antidote_lib:put(Id,riak_dt_map,{PharmacyUpdate}).

-spec name(Pharmacy::riak_dt_map:map()) -> string().
name(Pharmacy) ->
  case antidote_lib:findkey(Pharmacy,name,riak_dt_lwwreg) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

-spec id(Pharmacy::riak_dt_map:map()) -> pos_integer().
id(Pharmacy) ->
  case antidote_lib:findkey(Pharmacy,id,riak_dt_gcounter) of
    not_found -> 0;
    Id -> Id
  end.

-spec address(Pharmacy::riak_dt_map:map()) -> string().
address(Pharmacy) ->
  case antidote_lib:findkey(Pharmacy,address,riak_dt_lwwreg) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

-spec prescriptions(Pharmacy::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Pharmacy) ->
  case antidote_lib:findkey(Pharmacy,prescriptions,riak_dt_map) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.