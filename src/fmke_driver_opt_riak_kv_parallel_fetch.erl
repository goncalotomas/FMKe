%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for Riak KV, featuring a normalized data model (no CRDT nesting).
%% ---------------------------------------------------------------------------------------------------------------------
-module (fmke_driver_opt_riak_kv_parallel_fetch).

-behaviour (fmke_gen_driver).

-include ("fmke.hrl").
-include ("fmke_kv.hrl").

-export([
  start/1,
  stop/1,
  create_patient/3,
  create_pharmacy/3,
  create_facility/4,
  create_staff/4,
  create_prescription/6,
  create_event/5,
  create_treatment/5,
  create_treatment/6,
  get_event_by_id/1,
  get_facility_by_id/1,
  get_patient_by_id/1,
  get_patient_by_id/2,
  get_pharmacy_by_id/1,
  get_pharmacy_by_id/2,
  get_processed_pharmacy_prescriptions/1,
  get_pharmacy_prescriptions/1,
  get_prescription_by_id/1,
  get_prescription_by_id/2,
  get_prescription_medication/1,
  get_staff_by_id/1,
  get_staff_by_id/2,
  get_staff_prescriptions/1,
  get_staff_treatments/1,
  get_treatment_by_id/1,
  process_prescription/2,
  update_patient_details/3,
  update_pharmacy_details/3,
  update_facility_details/4,
  update_staff_details/4,
  update_prescription_medication/3
]).

start(Params) ->
    case fmke_sup:start_link() of
       {ok, Pid} -> start_conn_pool(Pid, Params);
       _Error -> _Error
    end.

start_conn_pool(Pid, Params) ->
    Hostnames = proplists:get_value(db_conn_hostnames, Params),
    Ports = proplists:get_value(db_conn_ports, Params),
    ConnPoolSize = proplists:get_value(db_conn_pool_size, Params),
    {ok,_} = fmke_db_conn_pool:start([
        {db_conn_hostnames, Hostnames},
        {db_conn_ports, Ports},
        {db_conn_module, riakc_pb_socket},
        {db_conn_pool_size, ConnPoolSize}
    ]),
    {ok,Pid}.

stop(_) ->
  ok.

get_riak_props(Entity, Id) when is_atom(Entity), is_integer(Id) ->
    BucketType = get_bucket_type(Entity),
    BucketName = get_bucket(Entity),
    Key = get_key(Entity, Id),
    {Key, {BucketType, BucketName}}.

get_bucket_type(_Entity) -> <<"maps">>.

get_bucket(event) -> <<"events">>;
get_bucket(facility) -> <<"facilities">>;
get_bucket(patient) -> <<"patients">>;
get_bucket(pharmacy) -> <<"pharmacies">>;
get_bucket(prescription) -> <<"prescriptions">>;
get_bucket(staff) -> <<"staff">>;
get_bucket(treatment) -> <<"treatments">>.

create_if_not_exists(Entity, Fields) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    Result = create_if_not_exists(Pid, Entity, Fields),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result.

create_if_not_exists(Pid, Entity, Fields) ->
    Id = hd(Fields),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    case check_key(Pid, Key, BucketType, BucketName) of
        taken ->
            {error, list_to_atom(lists:flatten(io_lib:format("~p_id_taken",[Entity])))};
        free ->
            LocalObject = gen_entity(Entity, Fields),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(LocalObject))
    end.

%% Does kind of the opposite of create_if_not_exists/2
update_if_already_exists(Entity, Fields) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    Result = update_if_already_exists(Pid, Entity, Fields),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result.

update_if_already_exists(Pid, Entity, Fields) ->
    Id = hd(Fields),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    Result =
      case check_key(Pid, Key, BucketType, BucketName) of
        free ->
            {error, list_to_atom(lists:flatten(io_lib:format("no_such_~p",[Entity])))};
        taken ->
            EntityUpdate = gen_entity(Entity,Fields),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(EntityUpdate))
      end,
    Result.

%% Checks if an entity exists
check_key(Pid, Key, BucketType, BucketName) ->
    case riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key) of
        {error, {notfound, _Type}} -> free;
        {ok, _Map} -> taken
    end.

get_key(Entity,Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p",[Entity,Id]))).

gen_entity(patient, [Id, Name, Address]) ->
    Map = riakc_map:new(),
    Map1 = riakc_map:update({?PATIENT_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(Id)), R) end,
                        Map),
    Map2 = riakc_map:update({?PATIENT_NAME_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Name), R) end,
                        Map1),
    Map3 = riakc_map:update({?PATIENT_ADDRESS_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Address), R) end,
                        Map2),
    Map3;
gen_entity(pharmacy, [Id, Name, Address]) ->
    Map = riakc_map:new(),
    Map1 = riakc_map:update({?PHARMACY_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(Id)), R) end,
                        Map),
    Map2 = riakc_map:update({?PHARMACY_NAME_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Name), R) end,
                        Map1),
    Map3 = riakc_map:update({?PHARMACY_ADDRESS_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Address), R) end,
                        Map2),
    Map3;
gen_entity(facility, [Id, Name, Address, Type]) ->
    Map = riakc_map:new(),
    Map1 = riakc_map:update({?FACILITY_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(Id)), R) end,
                        Map),
    Map2 = riakc_map:update({?FACILITY_NAME_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Name), R) end,
                        Map1),
    Map3 = riakc_map:update({?FACILITY_ADDRESS_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Address), R) end,
                        Map2),
    Map4 = riakc_map:update({?FACILITY_TYPE_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Type), R) end,
                        Map3),
    Map4;
gen_entity(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]) ->
    Map = riakc_map:new(),
    Map1 = riakc_map:update({?PRESCRIPTION_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(Id)), R) end,
                        Map),
    Map2 = riakc_map:update({?PRESCRIPTION_PATIENT_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(PatientId)), R) end,
                        Map1),
    Map3 = riakc_map:update({?PRESCRIPTION_PHARMACY_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(PharmacyId)), R) end,
                        Map2),
    Map4 = riakc_map:update({?PRESCRIPTION_PRESCRIBER_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(PrescriberId)), R) end,
                        Map3),
    Map5 = riakc_map:update({?PRESCRIPTION_DATE_PRESCRIBED_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(DatePrescribed), R) end,
                        Map4),
    Map6 = riakc_map:update({?PRESCRIPTION_DRUGS_KEY, set},
                        fun(S) -> riakc_set:add_elements(lists:map(fun list_to_binary/1, Drugs), S) end,
                        Map5),
    Map7 = riakc_map:update({?PRESCRIPTION_IS_PROCESSED_KEY, register},
                        fun(R) -> riakc_register:set(?PRESCRIPTION_NOT_PROCESSED_VALUE, R) end,
                        Map6),
    Map7;
gen_entity(staff, [Id, Name, Address, Speciality]) ->
    Map = riakc_map:new(),
    Map1 = riakc_map:update({?STAFF_ID_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(integer_to_list(Id)), R) end,
                        Map),
    Map2 = riakc_map:update({?STAFF_NAME_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Name), R) end,
                        Map1),
    Map3 = riakc_map:update({?STAFF_ADDRESS_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Address), R) end,
                        Map2),
    Map4 = riakc_map:update({?STAFF_SPECIALITY_KEY, register},
                        fun(R) -> riakc_register:set(list_to_binary(Speciality), R) end,
                        Map3),
    Map4.

%% Searches for a Value within a map that is associated with a specific key.
%% All riakc_map entries are of type {{key_name,key_type},Value}. This function is only used when fake updates are
%% produced locally.
find_key({map, Values, _, _, _}, Key, KeyType, FallbackValue) ->
  try orddict:fetch({Key, KeyType}, Values) of
    false -> FallbackValue;
    [Value] -> Value
  catch
    _:_ -> FallbackValue
  end.

tryfetch(Key,KeyType,Map,DefaultVal) ->
    try
        riakc_map:fetch({Key, KeyType}, Map)
    catch
        _:_ -> DefaultVal
    end.

build_app_record(_, {error, not_found}) ->
    {error, not_found};
build_app_record(facility,Object) ->
    Id = riakc_map:fetch({?FACILITY_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?FACILITY_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?FACILITY_ADDRESS_KEY, register}, Object),
    Type = riakc_map:fetch({?FACILITY_TYPE_KEY, register}, Object),
    #facility{id=Id,name=Name,address=Address,type=Type};
build_app_record(patient,Object) ->
    Id = riakc_map:fetch({?PATIENT_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?PATIENT_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?PATIENT_ADDRESS_KEY, register}, Object),
    Prescriptions = find_key(Object, ?PATIENT_PRESCRIPTIONS_KEY, set, []),
    #patient{
        id=Id
        ,name=Name
        ,address=Address
        ,prescriptions=Prescriptions
    };
build_app_record(pharmacy,Object) ->
    Id = riakc_map:fetch({?PHARMACY_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?PHARMACY_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?PHARMACY_ADDRESS_KEY, register}, Object),
    Prescriptions = find_key(Object, ?PHARMACY_PRESCRIPTIONS_KEY, set, []),
    #pharmacy{
        id=Id
        ,name=Name
        ,address=Address
        ,prescriptions=Prescriptions
    };
build_app_record(prescription,Object) ->
    Id = tryfetch(?PRESCRIPTION_ID_KEY, register, Object, -1),
    PatientId = tryfetch(?PRESCRIPTION_PATIENT_ID_KEY,register,Object,-1),
    PrescriberId = tryfetch(?PRESCRIPTION_PRESCRIBER_ID_KEY,register,Object,-1),
    PharmacyId = tryfetch(?PRESCRIPTION_PHARMACY_ID_KEY,register,Object,-1),
    DatePrescribed = riakc_map:fetch({?PRESCRIPTION_DATE_PRESCRIBED_KEY, register}, Object),
    IsProcessed = tryfetch(?PRESCRIPTION_IS_PROCESSED_KEY,register,Object,?PRESCRIPTION_NOT_PROCESSED_VALUE),
    DateProcessed = tryfetch(?PRESCRIPTION_DATE_PROCESSED_KEY,register,Object,<<"undefined">>),
    Drugs = riakc_map:fetch({?PRESCRIPTION_DRUGS_KEY, set}, Object),
    #prescription{
        id=Id
        ,patient_id=PatientId
        ,pharmacy_id=PharmacyId
        ,prescriber_id=PrescriberId
        ,date_prescribed=DatePrescribed
        ,date_processed=DateProcessed
        ,drugs=Drugs
        ,is_processed=IsProcessed
    };
build_app_record(staff,Object) ->
    Id = riakc_map:fetch({?STAFF_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?STAFF_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?STAFF_ADDRESS_KEY, register}, Object),
    Speciality = riakc_map:fetch({?STAFF_SPECIALITY_KEY, register}, Object),
    Prescriptions = find_key(Object, ?STAFF_PRESCRIPTIONS_KEY, set, []),
    #staff{
        id=Id
        ,name=Name
        ,address=Address
        ,speciality=Speciality
        ,prescriptions=Prescriptions
        }.

get_entity_with_prescriptions(Entity, Id) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    Result = get_entity_with_prescriptions(Pid, Entity, Id),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result.

get_entity_with_prescriptions(Pid, Entity, Id) ->
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    [EntityObject, Prescriptions, ProcessedPrescriptions] = multi_read(Pid, [
      {Key, BucketType, BucketName},
      {Key, <<"sets">>, <<"prescriptions">>},
      {Key, <<"sets">>, <<"processed_prescriptions">>}
    ]),
    case get_prescs_from_sets(Prescriptions, ProcessedPrescriptions) of
        [] -> build_app_record(Entity, EntityObject);
        Prescs ->
            {map, Values, Updates, Removals, Context} = EntityObject,
            NewValues = orddict:append({get_prescriptions_key(Entity), set}, Prescs, Values),
            build_app_record(Entity, {map, NewValues, Updates, Removals, Context})
    end.

get_prescriptions_key(patient) -> ?PATIENT_PRESCRIPTIONS_KEY;
get_prescriptions_key(pharmacy) -> ?PHARMACY_PRESCRIPTIONS_KEY;
get_prescriptions_key(staff) -> ?STAFF_PRESCRIPTIONS_KEY.

get_prescs_from_sets({error, not_found}, {error, not_found}) -> [];
get_prescs_from_sets({error, not_found}, Keys) -> riakc_set:value(Keys);
get_prescs_from_sets(Keys, {error, not_found}) -> riakc_set:value(Keys);
get_prescs_from_sets(KeysL, KeysR) -> lists:flatten(riakc_set:value(KeysL), riakc_set:value(KeysR)).

multi_read(Pid, Objects) ->
    parallel_fetch(Pid, Objects).

-spec create_patient(id(), string(), string()) -> ok | {error, reason()}.
create_patient(Id, Name, Address) -> create_if_not_exists(patient, [Id, Name, Address]).

-spec create_pharmacy(id(), string(), string()) -> ok | {error, reason()}.
create_pharmacy(Id, Name, Address) -> create_if_not_exists(pharmacy, [Id, Name, Address]).

-spec create_facility(id(), string(), string(), string()) -> ok | {error, reason()}.
create_facility(Id, Name, Address, Type) -> create_if_not_exists(facility, [Id, Name, Address, Type]).

-spec create_staff(id(), string(), string(), string()) -> ok | {error, reason()}.
create_staff(Id, Name, Address, Speciality) -> create_if_not_exists(staff, [Id, Name, Address, Speciality]).

-spec get_event_by_id(id()) -> [crdt()] | {error, reason()}.
get_event_by_id(_Id) -> erlang:error(not_implemented).

-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) -> build_app_record(facility, process_get_request(facility, Id)).

-spec get_pharmacy_by_id(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id) -> get_entity_with_prescriptions(pharmacy, Id).

-spec get_pharmacy_by_id(id(), any()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id, Txn) -> get_entity_with_prescriptions(pharmacy, Id, Txn).

-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) -> get_entity_with_prescriptions(patient, Id).

-spec get_patient_by_id(id(), any()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id, Txn) -> get_entity_with_prescriptions(patient, Id, Txn).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(Id) ->
    Key = get_key(pharmacy, Id),
    Pid = poolboy:checkout(fmke_db_connection_pool),
    [Prescriptions, ProcessedPrescriptions] = multi_read(Pid, [
        {Key, <<"sets">>, <<"prescriptions">>},
        {Key, <<"sets">>, <<"processed_prescriptions">>}
    ]),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    case {Prescriptions, ProcessedPrescriptions} of
        {{error, not_found}, {error, not_found}} -> [];
        {{error, not_found}, PrescriptionKeys2} ->  PrescriptionKeys2;
        {PrescriptionKeys1, {error, not_found}} ->  PrescriptionKeys1;
        {PrescriptionKeys1, PrescriptionKeys2} ->   lists:append(PrescriptionKeys1, PrescriptionKeys2)
    end.

-spec get_processed_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_processed_pharmacy_prescriptions(Id) ->
    PharmacyKey = get_key(pharmacy, Id),
    Pid = poolboy:checkout(fmke_db_connection_pool),
    [ProcessedPrescriptions] = multi_read(Pid, [
        {PharmacyKey, <<"sets">>, <<"processed_prescriptions">>}
    ]),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    case ProcessedPrescriptions of
        {error, not_found} -> [];
        PrescriptionKeys ->  PrescriptionKeys
    end.

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
    build_app_record(prescription, process_get_request(prescription, Id)).

%% Fetches a prescription by ID.
-spec get_prescription_by_id(pid(), id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Pid, Id) ->
    build_app_record(prescription, process_get_request(Pid, prescription, Id)).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
get_prescription_medication(Id) ->
    Prescription = get_prescription_by_id(Id),
    case Prescription of
      {error, _} -> {error, no_such_prescription};
      _ -> Prescription#prescription.drugs
    end.

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) -> get_entity_with_prescriptions(staff, Id).

%% Alternative to get_staff_by_id/1, which includes a transactional context.
-spec get_staff_by_id(id(), any()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id, Txn) -> get_entity_with_prescriptions(staff, Id, Txn).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_staff_prescriptions(Id) ->
    Key = get_key(staff, Id),
    Pid = poolboy:checkout(fmke_db_connection_pool),
    [Prescriptions, ProcessedPrescriptions] = multi_read(Pid, [
        {Key, <<"sets">>, <<"prescriptions">>},
        {Key, <<"sets">>, <<"processed_prescriptions">>}
    ]),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    case {Prescriptions, ProcessedPrescriptions} of
        {{error, not_found}, {error, not_found}} -> [];
        {{error, not_found}, PrescriptionKeys2} ->  PrescriptionKeys2;
        {PrescriptionKeys1, {error, not_found}} ->  PrescriptionKeys1;
        {PrescriptionKeys1, PrescriptionKeys2} ->   lists:append(PrescriptionKeys1, PrescriptionKeys2)
    end.

%% Fetches a list of treatments given a certain staff member ID.
-spec get_staff_treatments(id()) -> [crdt()] | {error, reason()}.
get_staff_treatments(_StaffId) ->
  erlang:error(not_implemented).

%% Fetches a treatment by ID.
-spec get_treatment_by_id(id()) -> [crdt()] | {error, reason()}.
get_treatment_by_id(_Id) ->
  erlang:error(not_implemented).

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(), string(), string()) -> ok | {error, reason()}.
update_patient_details(Id, Name, Address) ->
  update_if_already_exists(patient, [Id, Name, Address]).

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(), string(), string()) -> ok | {error, reason()}.
update_pharmacy_details(Id, Name, Address) ->
  update_if_already_exists(pharmacy, [Id, Name, Address]).

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_facility_details(Id, Name, Address, Type) ->
  update_if_already_exists(facility, [Id, Name, Address, Type]).

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_staff_details(Id, Name, Address, Speciality) ->
  update_if_already_exists(staff, [Id, Name, Address, Speciality]).

-spec update_prescription_medication(id(), atom(), [string()]) -> ok | {error, reason()}.
update_prescription_medication(Id, add_drugs, Drugs) ->
    {Key, {BucketType, BucketName}} = get_riak_props(prescription, Id),
    Pid = poolboy:checkout(fmke_db_connection_pool),
    Result = case get_prescription_by_id(Id) of
        {error, not_found} ->
            {error, no_such_prescription};
        Prescription ->
            case Prescription#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {error, prescription_already_processed};
                _Other ->
                    Map = riakc_map:new(),
                    Map1 = riakc_map:update({?PRESCRIPTION_DRUGS_KEY, set},
                                        fun(S) -> riakc_set:add_elements(lists:map(fun list_to_binary/1, Drugs), S) end,
                                        Map),
                    ok = riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(Map1))
            end
      end,
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result;

update_prescription_medication(_Id, _Op, _Drugs) -> {error, invalid_update_operation}.

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy and treatment facility (hospital). The prescription also includes the prescription date
%% and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    %% gather required keys
    PrescriptionKey = get_key(prescription, PrescriptionId),
    PatientKey = get_key(patient, PatientId),
    PharmacyKey = get_key(pharmacy, PharmacyId),
    PrescriberKey = get_key(staff, PrescriberId),
    Pid = poolboy:checkout(fmke_db_connection_pool),
    %% check required pre-conditions
    [ {taken, PatientKey}, {taken, PharmacyKey}, {taken, PrescriberKey}, {Status, PrescriptionKey}]
      = check_keys(Pid, [
          {PatientKey, get_bucket_type(patient), get_bucket(patient)},
          {PharmacyKey, get_bucket_type(pharmacy), get_bucket(pharmacy)},
          {PrescriberKey, get_bucket_type(staff), get_bucket(staff)},
          {PrescriptionKey, get_bucket_type(prescription), get_bucket(prescription)}
      ]),

    CreateOpResult = case Status of
        taken -> {error, prescription_id_taken};
        free ->
            PrescriptionFields = [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
            LocalObject = riakc_map:to_op(gen_entity(prescription, PrescriptionFields)),
            BucketType1 = get_bucket_type(prescription),
            BucketName1 = get_bucket(prescription),
            riakc_pb_socket:update_type(Pid, {BucketType1, BucketName1}, PrescriptionKey, LocalObject)
    end,

    Result = case CreateOpResult of
        ok ->
            Prescriptions = riakc_set:new(),
            Prescriptions1 = riakc_set:to_op(riakc_set:add_element(PrescriptionKey, Prescriptions)),
            BucketType = <<"sets">>,
            BucketName = <<"prescriptions">>,
            parallel_update(Pid, [
                {PatientKey, BucketType, BucketName, Prescriptions1},
                {PharmacyKey, BucketType, BucketName, Prescriptions1},
                {PrescriberKey, BucketType, BucketName, Prescriptions1}
            ]),
            ok;
        ErrorMessage -> ErrorMessage
    end,
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result.

%% Creates a treatment event, with information about the staff member that registered it,
%% along with a timestamp and description.
-spec create_event(id(), id(), id(), string(), string()) -> ok | {error, reason()}.
create_event(_EventId, _TreatmentId, _StaffMemberId, _Timestamp, _Description) ->
  erlang:error(not_implemented).

%% Creates a treatment with information about the patient, the staff member that iniciated it,
%% and also the facility ID and date when the treatment started.
-spec create_treatment(id(), id(), id(), id(), string()) -> ok | {error, reason()}.
create_treatment(_TreatmentId, _PatientId, _StaffId, _FacilityId, _DateStarted) ->
  erlang:error(not_implemented).

%% Same as create_treatment/5, but includes an ending date for the treatment.
-spec create_treatment(id(), id(), id(), id(), string(), string()) -> ok | {error, reason()}.
create_treatment(_TreatmentId, _PatientId, _StaffId, _FacilityId, _DateStarted, _DateEnded) ->
  erlang:error(not_implemented).

process_prescription(Id, Date) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    Result = process_prescription(Pid, Id, Date),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result.

process_prescription(Pid, Id, Date) ->
    case process_get_request(Pid, prescription, Id) of
        {error, not_found} ->
            {error, no_such_prescription};
        Prescription ->
            case riakc_map:fetch({?PRESCRIPTION_IS_PROCESSED_KEY, register}, Prescription) of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {error, prescription_already_processed};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    Object = build_app_record(prescription, Prescription),
                    PharmacyKey =      get_key(pharmacy, binary_to_integer(Object#prescription.pharmacy_id)),
                    PatientKey =       get_key(patient, binary_to_integer(Object#prescription.patient_id)),
                    StaffKey =         get_key(staff, binary_to_integer(Object#prescription.prescriber_id)),
                    PrescriptionKey =  get_key(prescription, Id),

                    NewMap1 = riakc_map:update({?PRESCRIPTION_IS_PROCESSED_KEY, register},
                                        fun(R) -> riakc_register:set(?PRESCRIPTION_PROCESSED_VALUE, R) end,
                                    Prescription),
                    NewMap = riakc_map:update({?PRESCRIPTION_DATE_PROCESSED_KEY, register},
                                        fun(R) -> riakc_register:set(list_to_binary(Date), R) end,
                                    NewMap1),

                    [OpenPatPrescs, OpenPharmPrescs, OpenStaffPrescs] = multi_read(Pid, [
                        {PatientKey, <<"sets">>, <<"prescriptions">>},
                        {PharmacyKey, <<"sets">>, <<"prescriptions">>},
                        {StaffKey, <<"sets">>, <<"prescriptions">>}
                    ]),

                    RemFromSetPat = riakc_set:del_element(PrescriptionKey, OpenPatPrescs),
                    RemFromSetPharm = riakc_set:del_element(PrescriptionKey, OpenPharmPrescs),
                    RemFromSetStaff = riakc_set:del_element(PrescriptionKey, OpenStaffPrescs),

                    ProcPrescsLocal = riakc_set:add_element(PrescriptionKey, riakc_set:new()),

                    DelBuckName = <<"prescriptions">>,
                    AddBuckName = <<"processed_prescriptions">>,
                    BucketType = <<"sets">>,

                    [ok, ok, ok, ok, ok, ok, ok] = parallel_update(Pid, [
                        {PrescriptionKey, <<"maps">>, <<"prescriptions">>, riakc_map:to_op(NewMap)},
                        {PatientKey, BucketType, DelBuckName, riakc_set:to_op(RemFromSetPat)},
                        {PharmacyKey, BucketType, DelBuckName, riakc_set:to_op(RemFromSetPharm)},
                        {StaffKey, BucketType, DelBuckName, riakc_set:to_op(RemFromSetStaff)},
                        {PatientKey, BucketType, AddBuckName, riakc_set:to_op(ProcPrescsLocal)},
                        {PharmacyKey, BucketType, AddBuckName, riakc_set:to_op(ProcPrescsLocal)},
                        {StaffKey, BucketType, AddBuckName, riakc_set:to_op(ProcPrescsLocal)}
                    ]),
                    ok
           end
   end.

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
parallel_fetch(Pid, Keys) ->
    Self = self(),
    % Spawn a list of processed that will each fetch the value of an individual key
    Procs = lists:map(
                fun({Key, BucketType, BucketName}) ->
                    spawn(fun() ->
                            Result = riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key),
                            Self ! {self(), Result}
                    end)
                end, Keys),
    % For each spawned process, receive its message and get the result
    lists:map(
        fun(Proc) ->
            receive
                {Proc, {error, {notfound, _Type}}} -> {error, not_found};
                {Proc, {ok, Object}} -> Object;
                {Proc, _SomethingElse} -> _SomethingElse
            end
        end, Procs).

parallel_update(Pid, Updates) ->
    Self = self(),
    % Spawn a list of processed that will each fetch the value of an individual key
    Procs = lists:map(fun({Key, BucketType, BucketName, Update}) ->
                spawn(fun() ->
                        Result = riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, Update),
                        Self ! {self(), Result}
                end)
            end, Updates),
    % For each spawned process, receive its message and get the result
    lists:map(fun(Proc) ->
        receive
            {Proc, Result} -> Result
        end
    end, Procs).

check_keys(Pid, Keys) ->
    Results = lists:map(
                    fun(Result) ->
                        case Result of
                            {error, not_found} -> free;
                            _Object -> taken
                        end
                    end,
                    parallel_fetch(Pid, Keys)),
    lists:zip(Results, lists:map(fun({Key, _, _}) -> Key end, Keys)).

process_get_request(Entity, Id) ->
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    ReadResult = get(Key, BucketType, BucketName),
    case ReadResult of
        {error, {notfound, _Type}} -> {error, not_found};
        {ok, Object} -> Object;
        _SomethingElse -> _SomethingElse
    end.

process_get_request(Pid, Entity, Id) ->
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    ReadResult = get(Pid, Key, BucketType, BucketName),
    case ReadResult of
        {error, {notfound, _Type}} -> {error, not_found};
        {ok, Object} -> Object;
        _SomethingElse -> _SomethingElse
    end.

get(Key, BucketType, BucketName) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    Result = riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key),
    poolboy:checkin(fmke_db_connection_pool, Pid),
    Result.

get(Pid, Key, BucketType, BucketName) ->
    riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key).
