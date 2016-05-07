-module(patient).
-include("fmk.hrl").


%% Functions to handle single patient objects
-export ([
  new/3,
  update/2,
  name/1,
  id/1,
  address/1,
  treatments/1,
  prescriptions/1,
  events/1
  ]).

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string()) -> riak_dt_map:map_op().
new(Id,Name,Address) ->
  IdOp = antidote_lib:build_map_op(id,riak_dt_gcounter,{increment,Id}),
  NameOp = antidote_lib:build_map_op(name,riak_dt_lwwreg,{assign, list_to_binary(Name)}),
  AddressOp = antidote_lib:build_map_op(address,riak_dt_lwwreg,{assign, list_to_binary(Address)}),
  %% build nested map operations
  EventMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_events,riak_dt_pncounter,{increment,0})]),
  PrescriptionMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  TreatmentMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_treatments,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  EventsOp = antidote_lib:build_map_op(events,riak_dt_map,EventMapOp),
  PrescriptionsOp = antidote_lib:build_map_op(prescriptions,riak_dt_map,PrescriptionMapOp),
  TreatmentsOp = antidote_lib:build_map_op(treatments,riak_dt_map,TreatmentMapOp),
  %% put everything in a big bulky map update and return it
  antidote_lib:build_map_update([IdOp,NameOp,AddressOp,EventsOp,TreatmentsOp,PrescriptionsOp]).

-spec update(Id::pos_integer(), PatientUpdate::riak_dt_map:map_op()) -> {ok,_Something}.
update(Id,PatientUpdate) ->
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
  case antidote_lib:findkey(Patient,patient_name,riak_dt_lwwreg) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

-spec id(Patient::riak_dt_map:map()) -> pos_integer().
id(Patient) ->
  case antidote_lib:findkey(Patient,id,riak_dt_gcounter) of
    not_found -> 0;
    Id -> Id
  end.

-spec address(Patient::riak_dt_map:map()) -> string().
address(Patient) ->
  case antidote_lib:findkey(Patient,address,riak_dt_lwwreg) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

-spec treatments(Patient::riak_dt_map:map()) -> riak_dt_map:map().
treatments(Patient) ->
  case antidote_lib:findkey(Patient,treatments,riak_dt_map) of
    not_found -> [];
    Treatments -> Treatments
  end.

-spec prescriptions(Patient::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Patient) ->
  case antidote_lib:findkey(Patient,prescriptions,riak_dt_map) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

-spec events(Patient::riak_dt_map:map()) -> riak_dt_map:map().
events(Patient) ->
  case antidote_lib:findkey(Patient,events,riak_dt_map) of
    not_found -> [];
    Events -> Events
  end.