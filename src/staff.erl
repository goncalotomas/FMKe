-module (staff).

%% Functions to handle single Staff objects
-export ([
  new/4,
  update/2,
  name/1,
  id/1,
  address/1,
  speciality/1,
  treatments/1,
  prescriptions/1,
  events/1
  ]).

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string(), Speciality::nonempty_string()) -> riak_dt_map:map_op().
new(Id,Name,Address,Speciality) ->
  IdOp = antidote_lib:build_map_op(id,riak_dt_gcounter,{increment,Id}),
  NameOp = antidote_lib:build_map_op(name,riak_dt_lwwreg,{assign, list_to_binary(Name)}),
  AddressOp = antidote_lib:build_map_op(address,riak_dt_lwwreg,{assign, list_to_binary(Address)}),
  SpecialityOp = antidote_lib:build_map_op(speciality,riak_dt_lwwreg,{assign, list_to_binary(Speciality)}),
  %% build nested map operations
  PrescriptionsMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  TreatmentsMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_treatments,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  PrescriptionsOp = antidote_lib:build_map_op(prescriptions,riak_dt_map,PrescriptionMapOp),
  TreatmentsOp = antidote_lib:build_map_op(treatments,riak_dt_map,TreatmentMapOp),
  %% put everything in a big bulky map update and return it
  antidote_lib:build_map_update([IdOp,NameOp,AddressOp,SpecialityOp,TreatmentsOp,PrescriptionsOp]).

-spec update(Id::pos_integer(), StaffUpdate::riak_dt_map:map_op()) -> {ok,_Something}.
update(Id,StaffUpdate) ->
  antidote_lib:put(Id,riak_dt_map,{StaffUpdate}).

% create_Staff_bucket(StaffId) ->
%   antidote_lib:create_bucket(StaffId,riak_dt_map).

% update_Staff(StaffObject) ->
%   Txn = antidote_lib:start_txn(),
%   ok = antidote_lib:write_object(StaffObject,Txn),
%   ok = antidote_lib:commit_txn(Txn).

% read_Staff(StaffId) ->
%   Txn = antidote_lib:start_txn(),
%   StaffBucket = create_Staff_bucket(StaffId),
%   Value = antidote_lib:read_object(StaffBucket,Txn),
%   _CommitTime = antidote_lib:commit_txn(Txn),
%   Value.

-spec name(Staff::riak_dt_map:map()) -> string().
name(Staff) ->
  case antidote_lib:findkey(Staff,name,riak_dt_lwwreg) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

-spec id(Staff::riak_dt_map:map()) -> pos_integer().
id(Staff) ->
  case antidote_lib:findkey(Staff,id,riak_dt_gcounter) of
    not_found -> 0;
    Id -> Id
  end.

-spec address(Staff::riak_dt_map:map()) -> string().
address(Staff) ->
  case antidote_lib:findkey(Staff,address,riak_dt_lwwreg) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

-spec speciality(Staff::riak_dt_map:map()) -> string().
name(Staff) ->
  case antidote_lib:findkey(Staff,speciality,riak_dt_lwwreg) of
    not_found -> "";
    Speciality -> binary_to_list(Speciality)
  end.

-spec treatments(Staff::riak_dt_map:map()) -> riak_dt_map:map().
treatments(Staff) ->
  case antidote_lib:findkey(Staff,treatments,riak_dt_map) of
    not_found -> [];
    Treatments -> Treatments
  end.

-spec prescriptions(Staff::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Staff) ->
  case antidote_lib:findkey(Staff,prescriptions,riak_dt_map) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.