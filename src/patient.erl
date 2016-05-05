-module(patient).
-include("fmk.hrl").

%% Functions to handle patients at data-center level
-export ([
  findpatient/1
  ]).

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

%% Finds a patient in the Antidote Key-Value store by Patient ID.
-spec findpatient(Id::pos_integer()) -> riak_dt_map:map().
findpatient(Id) ->
  Patients = antidote_lib:read_from_antidote(patients,riak_dt_map),
  case lists:keyfind({Id,riak_dt_map},1,Patients) of
    false -> not_found;
    {{Id,riak_dt_map},Patient} -> Patient
  end.

-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string()) -> {ok,_Something}.
new(Id,Name,Address) ->
  IdOp = {update,{patient_id, riak_dt_gcounter},{increment, Id}},
  NameOp = {update,{patient_name, riak_dt_lwwreg},{assign, list_to_binary(Name)}},
  AddressOp = {update,{patient_address, riak_dt_lwwreg},{assign, list_to_binary(Address)}},
  EventsOp = {update,{patient_events, riak_dt_map},{update,{num_events,riak_dt_pncounter},{increment,0}}},
  _TreatmentsOp = {update,{key, riak_dt_lwwreg},{assign, <<"patient_treatments">>}},
  _PrescriptionsKey = {update,{key, riak_dt_lwwreg},{assign, <<"patient_prescriptions">>}},
  OpList = [IdOp,NameOp,AddressOp,EventsOp],
  {ok,_Something} = antidote_lib:write_to_antidote(Id,riak_dt_map,{update,OpList}). %% TODO SWITCH THIS TO THE NEW API

-spec new(Id::pos_integer(), PatientUpdate::riak_dt_map:map_op() -> {ok,_Something}.
update_patient(Id,PatientUpdate) ->
  antidote_lib:write_to_antidote(Id,riak_dt_map,{PatientUpdate}).

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

-spec new(Patient::riak_dt_map:map()) -> string().
name(Patient) ->
  case lists:keyfind({patient_name,riak_dt_lwwreg},1,Patient) of
    false -> "";
    {{patient_name,riak_dt_lwwreg},Name} -> binary_to_list(Name)
  end.

-spec new(Patient::riak_dt_map:map()) -> pos_integer().
id(Patient) ->
  case lists:keyfind({patient_id,riak_dt_gcounter},1,Patient) of
    false -> 0;
    {{patient_id,riak_dt_gcounter},Id} -> Id
  end.

-spec new(Patient::riak_dt_map:map()) -> string().
address(Patient) ->
  case lists:keyfind({patient_address,riak_dt_lwwreg},1,Patient) of
    false -> "";
    {{patient_address,riak_dt_lwwreg},Address} -> binary_to_list(Address)
  end.

-spec new(Patient::riak_dt_map:map()) -> riak_dt_map:map().
treatments(Patient) ->
  case lists:keyfind({patient_treatments,riak_dt_map},1,Patient) of
    false -> [];
    {{patient_treatments,riak_dt_map},Treatments} -> Treatments
  end.

-spec new(Patient::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Patient) ->
  case lists:keyfind({patient_prescriptions,riak_dt_map},1,Patient) of
    false -> [];
    {{patient_prescriptions,riak_dt_map},Prescriptions} -> Prescriptions
  end.

-spec new(Patient::riak_dt_map:map()) -> riak_dt_map:map().
events(Patient) ->
  case lists:keyfind({patient_events,riak_dt_map},1,Patient) of
    false -> [];
    {{patient_events,riak_dt_map},Events} -> Events
  end.