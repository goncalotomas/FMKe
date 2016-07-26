-module (staff).
-include("fmk.hrl").

%% Functions to handle single Staff objects
-export ([
  new/4,
  update_personal_details/3,
  name/1,
  id/1,
  address/1,
  speciality/1,
  treatments/1,
  prescriptions/1
  ]).

%% Creates a new staff member object from an ID, Name, Address and Speciality. Returns an update operation ready to insert into Antidote
-spec new(Id::pos_integer(), Name::nonempty_string(), Address::nonempty_string(), Speciality::nonempty_string()) -> riak_dt_map:map_op().
new(Id,Name,Address,Speciality) ->
  IdOp = antidote_lib:build_map_op(?STAFF_ID,?STAFF_ID_CRDT,antidote_lib:counter_increment(Id)),
  NameOp = antidote_lib:build_map_op(?STAFF_NAME,?STAFF_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?STAFF_ADDRESS,?STAFF_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  SpecialityOp = antidote_lib:build_map_op(?STAFF_SPECIALITY,?STAFF_SPECIALITY_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Speciality))),
  %% build nested map operations
  PrescriptionsMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_prescriptions,riak_dt_pncounter,{increment,0})]),
  TreatmentsMapOp = antidote_lib:build_map_update([antidote_lib:build_map_op(num_treatments,riak_dt_pncounter,{increment,0})]),
  %% build top level map operations
  PrescriptionsOp = antidote_lib:build_map_op(prescriptions,riak_dt_map,PrescriptionsMapOp),
  TreatmentsOp = antidote_lib:build_map_op(treatments,riak_dt_map,TreatmentsMapOp),
  %% put everything in a big bulky map update and return it
  [IdOp,NameOp,AddressOp,SpecialityOp,TreatmentsOp,PrescriptionsOp].


%% Update operation: updates only the staff member's personal details, including prescriptions and treatments
update_personal_details(Name,Address,Speciality) ->
  NameOp = antidote_lib:build_map_op(?STAFF_NAME,?STAFF_NAME_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Name))),
  AddressOp = antidote_lib:build_map_op(?STAFF_ADDRESS,?STAFF_ADDRESS_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Address))),
  SpecialityOp = antidote_lib:build_map_op(?STAFF_SPECIALITY,?STAFF_SPECIALITY_CRDT,antidote_lib:lwwreg_assign(list_to_binary(Speciality))),
  [NameOp,AddressOp,SpecialityOp].

%% Returns the name in the form of a list from a staff member object.
-spec name(Staff::riak_dt_map:map()) -> string().
name(Staff) ->
  case antidote_lib:find_key(Staff,?STAFF_NAME,?STAFF_NAME_CRDT) of
    not_found -> "";
    Name -> binary_to_list(Name)
  end.

%% Returns the id in the form of an integer from a staff member object.
-spec id(Staff::riak_dt_map:map()) -> pos_integer().
id(Staff) ->
  case antidote_lib:find_key(Staff,?STAFF_ID,?STAFF_ID_CRDT) of
    not_found -> 0;
    Id -> Id
  end.

%% Returns the address in the form of a list from a staff member object.
-spec address(Staff::riak_dt_map:map()) -> string().
address(Staff) ->
  case antidote_lib:find_key(Staff,?STAFF_ADDRESS,?STAFF_ADDRESS_CRDT) of
    not_found -> "";
    Address -> binary_to_list(Address)
  end.

%% Returns the members' speciality as a Riak map from a staff member object
-spec speciality(Staff::riak_dt_map:map()) -> string().
speciality(Staff) ->
  case antidote_lib:find_key(Staff,?STAFF_SPECIALITY,?STAFF_SPECIALITY_CRDT) of
    not_found -> "";
    Speciality -> binary_to_list(Speciality)
  end.

%% Returns the treatments as a Riak map from a staff member object
-spec treatments(Staff::riak_dt_map:map()) -> riak_dt_map:map().
treatments(Staff) ->
  case antidote_lib:find_key(Staff,?STAFF_TREATMENTS,?NESTED_MAP) of
    not_found -> [];
    Treatments -> Treatments
  end.

%% Returns the prescriptions as a Riak map from a staff member object
-spec prescriptions(Staff::riak_dt_map:map()) -> riak_dt_map:map().
prescriptions(Staff) ->
  case antidote_lib:find_key(Staff,?STAFF_PRESCRIPTIONS,?NESTED_MAP) of
    not_found -> [];
    Prescriptions -> Prescriptions
  end.