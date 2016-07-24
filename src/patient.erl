-module(patient).
-include("fmk.hrl").


%% Functions to handle single patient objects
-export ([
  new/3,
  update_personal_details/2,
  name/1,
  id/1,
  address/1,
  treatments/1,
  prescriptions/1,
  events/1
  ]).

%% Creates a new patient object from an ID, Name and Address. Returns an update operation ready to insert into Antidote
new(Id,Name,Address) ->
  IdOp = antidote_lib:build_map_op(?PATIENT_ID,?PATIENT_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?PATIENT_NAME,?PATIENT_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  %% build nested map operations
  EventMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_events,riak_dt_pncounter,{increment,0})]),
  PrescriptionMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  TreatmentMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_treatments,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  EventsOp = antidote_lib:build_map_op(?PATIENT_EVENTS,?NESTED_MAP,EventMapOp),
  PrescriptionsOp = antidote_lib:build_map_op(?PATIENT_PRESCRIPTIONS,?NESTED_MAP,PrescriptionMapOp),
  TreatmentsOp = antidote_lib:build_map_op(?PATIENT_TREATMENTS,?NESTED_MAP,TreatmentMapOp),
  %% put everything in a big bulky map update and return it
  [
    IdOp
    ,NameOp
    ,AddressOp
    ,EventsOp
    ,TreatmentsOp
    ,PrescriptionsOp
  ].

update_personal_details(Name,Address) ->
  NameOp = antidote_lib:build_map_op(?PATIENT_NAME,?PATIENT_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  [NameOp,AddressOp].

%% Returns the patient name as a list from a patient object
-spec name(Patient::?NESTED_MAP:map()) -> string().
name(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_NAME,?PATIENT_NAME_CRDT) of
    not_found -> {error,not_found};
    Name -> binary_to_list(Name)
  end.

%% Returns the patient id as an integer from a patient object
-spec id(Patient::?NESTED_MAP:map()) -> pos_integer().
id(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_ID,?PATIENT_ID_CRDT) of
    not_found -> {error,not_found};
    Id -> Id
  end.

%% Returns the patient address as a list from a patient object
-spec address(Patient::?NESTED_MAP:map()) -> string().
address(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_ADDRESS,?PATIENT_ADDRESS_CRDT) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

%% Returns the patient treatments as a Riak map from a patient object
-spec treatments(Patient::?NESTED_MAP:map()) -> ?NESTED_MAP:map().
treatments(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_TREATMENTS,?NESTED_MAP) of
    not_found -> [];
    Treatments -> Treatments
  end.

%% Returns the patient prescriptions as a Riak map from a patient object
-spec prescriptions(Patient::?NESTED_MAP:map()) -> ?NESTED_MAP:map().
prescriptions(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_PRESCRIPTIONS,?NESTED_MAP) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.

%% Returns the patient events as a Riak map from a patient object
-spec events(Patient::?NESTED_MAP:map()) -> ?NESTED_MAP:map().
events(Patient) ->
  case antidote_lib:find_key(Patient,?PATIENT_EVENTS,?NESTED_MAP) of
    not_found -> [];
    Events -> Events
  end.