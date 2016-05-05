-module(patient).
-include("fmk.hrl").


%% Functions to handle single patient objects
-export ([
  new/3,
  update_patient/2,
  name/1,
  id/1,
  address/1,
  treatments/1,
  prescriptions/1,
  events/1
  ]).

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string()) -> {ok,_Something}.
new(Id,Name,Address) ->
  IdOp = antidote_lib:build_map_op(patient_id,riak_dt_gcounter,{increment,Id}),
  NameOp = antidote_lib:build_map_op(patient_name,riak_dt_lwwreg,{assign, list_to_binary(Name)}),
  AddressOp = antidote_lib:build_map_op(patient_address,riak_dt_lwwreg,{assign, list_to_binary(Address)}),
  %% build nested map operations
  EventMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_events,riak_dt_pncounter,{increment,0})]),
  PrescriptionMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  TreatmentMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_treatments,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  EventsOp = antidote_lib:build_map_op(patient_events,riak_dt_map,EventMapOp),
  PrescriptionsOp = antidote_lib:build_map_op(patient_prescriptions,riak_dt_map,PrescriptionMapOp),
  TreatmentsOp = antidote_lib:build_map_op(patient_treatments,riak_dt_map,TreatmentMapOp),
  %% put everything in a big bulky map update
  MapUpdate = antidote_lib:build_map_update([IdOp,NameOp,AddressOp,EventsOp,TreatmentsOp,PrescriptionsOp]),
  {ok,_Something} = antidote_lib:put(Id,riak_dt_map,MapUpdate). %% TODO SWITCH THIS TO THE NEW API

-spec update_patient(Id::pos_integer(), PatientUpdate::riak_dt_map:map_op()) -> {ok,_Something}.
update_patient(Id,PatientUpdate) ->
  antidote_lib:put(Id,riak_dt_map,{PatientUpdate}).

% create_patient_bucket(PatientId) ->
%   antidote_lib:create_bucket(PatientId,riak_dt_map).

% update_patient(PatientObject) ->
%   Txn = antidote_lib:start_txn(),
%   ok = antidote_lib:write_object(PatientObject,Txn),
%   ok = antidote_lib:commit_txn(Txn).

% read_patient(PatientId) ->
%   Txn = antidote_lib:start_txn(),
%   PatientBucket = create_patient_bucket(PatientId),
%   Value = antidote_lib:read_object(PatientBucket,Txn),
%   _CommitTime = antidote_lib:commit_txn(Txn),
%   Value.

-spec name(Patient::riak_dt_map:map()) -> string().
name(Patient) ->
  case lists:keyfind({patient_name,riak_dt_lwwreg},1,Patient) of
    false -> "";
    {{patient_name,riak_dt_lwwreg},Name} -> binary_to_list(Name)
  end.

-spec id(Patient::riak_dt_map:map()) -> pos_integer().
id(Patient) ->
  case lists:keyfind({patient_id,riak_dt_gcounter},1,Patient) of
    false -> 0;
    {{patient_id,riak_dt_gcounter},Id} -> Id
  end.

-spec address(Patient::riak_dt_map:map()) -> string().
address(Patient) ->
  case lists:keyfind({patient_address,riak_dt_lwwreg},1,Patient) of
    false -> "";
    {{patient_address,riak_dt_lwwreg},Address} -> binary_to_list(Address)
  end.

-spec treatments(Patient::riak_dt_map:map()) -> riak_dt_map:map().
treatments(Patient) ->
  case lists:keyfind({patient_treatments,riak_dt_map},1,Patient) of
    false -> [];
    {{patient_treatments,riak_dt_map},Treatments} -> Treatments
  end.

-spec prescriptions(Patient::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Patient) ->
  case lists:keyfind({patient_prescriptions,riak_dt_map},1,Patient) of
    false -> [];
    {{patient_prescriptions,riak_dt_map},Prescriptions} -> Prescriptions
  end.

-spec events(Patient::riak_dt_map:map()) -> riak_dt_map:map().
events(Patient) ->
  case lists:keyfind({patient_events,riak_dt_map},1,Patient) of
    false -> [];
    {{patient_events,riak_dt_map},Events} -> Events
  end.