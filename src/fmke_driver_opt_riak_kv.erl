%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for Riak KV, featuring a normalized data model (no CRDT nesting).
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_riak_kv).

-behaviour(fmke_gen_driver).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% API

-export([
  start/1
  ,stop/1
  ,create_patient/3
  ,create_pharmacy/3
  ,create_facility/4
  ,create_staff/4
  ,create_prescription/6
  ,get_facility_by_id/1
  ,get_patient_by_id/1
  ,get_pharmacy_by_id/1
  ,get_processed_pharmacy_prescriptions/1
  ,get_pharmacy_prescriptions/1
  ,get_prescription_by_id/1
  ,get_prescription_medication/1
  ,get_staff_by_id/1
  ,get_staff_prescriptions/1
  ,process_prescription/2
  ,update_patient_details/3
  ,update_pharmacy_details/3
  ,update_facility_details/4
  ,update_staff_details/4
  ,update_prescription_medication/3
]).

%% gen_server exports
-export ([
    init/1
    ,handle_cast/2
    ,handle_call/3
]).

-define(SERVER, ?MODULE).

start(_) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(_) ->
    gen_server:call(?MODULE, stop).

init(_) ->
    {ok, {}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

create_patient(Id, Name, Address) ->
    gen_server:call(?MODULE, {create, patient, [Id, Name, Address]}).

create_pharmacy(Id, Name, Address) ->
    gen_server:call(?MODULE, {create, pharmacy, [Id, Name, Address]}).

create_facility(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {create, facility, [Id, Name, Address, Type]}).

create_staff(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {create, staff, [Id, Name, Address, Speciality]}).

create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    gen_server:call(?MODULE,
        {create, prescription, [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}
    ).

get_facility_by_id(Id) ->
    gen_server:call(?MODULE, {read, facility, Id}).

get_patient_by_id(Id) ->
    gen_server:call(?MODULE, {read, patient, Id}).

get_pharmacy_by_id(Id) ->
    gen_server:call(?MODULE, {read, pharmacy, Id}).

get_processed_pharmacy_prescriptions(Id) ->
    F = fun(P) -> P#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE end,
    gen_server:call(?MODULE, {read, pharmacy, Id, [prescriptions], F}).

get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {read, pharmacy, Id, [prescriptions]}).

get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {read, prescription, Id}).

get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {read, prescription, Id, [drugs]}).

get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {read, staff, Id}).

get_staff_prescriptions(Id) ->
  gen_server:call(?MODULE, {read, staff, Id, [prescriptions]}).

process_prescription(Id, DateProcessed) ->
    gen_server:call(?MODULE, {update, prescription, Id, {date_processed, DateProcessed}}).

update_patient_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update, patient, [Id, Name, Address]}).

update_pharmacy_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update, pharmacy, [Id, Name, Address]}).

update_facility_details(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {update, facility, [Id, Name, Address, Type]}).

update_staff_details(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {update, staff, [Id, Name, Address, Speciality]}).

update_prescription_medication(Id, add_drugs, Drugs) ->
    gen_server:call(?MODULE, {update, prescription, Id, {drugs, add, Drugs}}).

% handle_call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}, _From, State) ->
%     %% gather required antidote keys
%     PrescriptionKey = gen_key(prescription, Id),
%     PatientKey = gen_key(patient, PatientId),
%     PharmacyKey = gen_key(pharmacy, PharmacyId),
%     PrescriberKey = gen_key(staff, PrescriberId),
%     Txn = txn_start(),
%     %% check required pre-conditions
%     [ {taken, {patient, PatientId}}, {taken, {pharmacy, PharmacyId}}, {taken, {staff, PrescriberId}} ]
%         = check_keys(Txn, [{patient, PatientId}, {pharmacy, PharmacyId}, {staff, PrescriberId}]),
%
%     PrescriptionFields = [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
%     {HandleCreateOpResult, Txn2} = create_if_not_exists(prescription, PrescriptionFields, Txn),
%
%     {Result, Txn3} = case HandleCreateOpResult of
%         ok ->
%             %% build updates for patients, pharmacies, facilities and the prescriber
%             %% these are already generated as buckets
%             PatientUpdate = gen_entity_update(add_entity_prescription, [PatientKey, PrescriptionKey]),
%             PharmacyUpdate = gen_entity_update(add_entity_prescription, [PharmacyKey, PrescriptionKey]),
%             PrescriberUpdate = gen_entity_update(add_entity_prescription, [PrescriberKey, PrescriptionKey]),
%             txn_update_objects([ PharmacyUpdate, PrescriberUpdate, PatientUpdate ], Txn2),
%             {ok, Txn2};
%         ErrorMessage -> {ErrorMessage, Txn2}
%     end,
%     txn_commit(Txn3),
%     {reply, Result, State};
%
% handle_call({create, Entity, Fields}, _From, State) ->
%     {reply, create_if_not_exists(Entity, Fields), State};

handle_call({read, Entity, Id, prescriptions}, _From, State) ->
    {Key, {_BucketType, _BucketName}} = get_riak_props(Entity, Id),
    Pid = fmke_db_conn_manager:checkout(),
    [Prescriptions, ProcessedPrescriptions] = multi_read(Pid, [
      {Key, <<"sets">>, <<"prescriptions">>},
      {Key, <<"sets">>, <<"processed_prescriptions">>}
    ]),
    Result = case get_prescs_from_sets(Prescriptions, ProcessedPrescriptions) of
        [] -> [];
        Prescs ->
            lists:map(fun(P) -> build_app_record(prescription, P) end, Prescs)
    end,
    fmke_db_conn_manager:checkin(Pid),
    {reply, Result, State};

handle_call({read, Entity, Id, processed_prescriptions}, _From, State) ->
    {Key, {_BucketType, _BucketName}} = get_riak_props(Entity, Id),
    Pid = fmke_db_conn_manager:checkout(),
    [ProcessedPrescriptions] = multi_read(Pid, [
      {Key, <<"sets">>, <<"processed_prescriptions">>}
    ]),
    Result = lists:map(fun(P) -> build_app_record(prescription, P) end, ProcessedPrescriptions),
    fmke_db_conn_manager:checkin(Pid),
    {reply, Result, State};

handle_call({read, prescription, Id, drugs}, _From, State) ->
    Prescription = get_prescription_by_id(Id),
    Result = case Prescription of
        {error, _} -> {error, no_such_prescription};
        _ -> Prescription#prescription.drugs
    end,
    {reply, Result, State};

handle_call({read, Entity, Id}, _From, State) when Entity =:= patient; Entity =:= pharmacy; Entity =:= staff ->
    {reply, get_entity_with_prescriptions(Entity, Id), State};

handle_call({read, Entity, Id}, _From, State) ->
    {reply, build_app_record(Entity, process_get_request(Entity, Id)), State};

handle_call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}, _From, State) ->
    %% gather required keys
    PrescriptionKey = get_key(prescription, Id),
    PatientKey = get_key(patient, PatientId),
    PharmacyKey = get_key(pharmacy, PharmacyId),
    PrescriberKey = get_key(staff, PrescriberId),
    Pid = fmke_db_conn_manager:checkout(),
    %% check required pre-conditions
    [ {taken, {patient, PatientId}}, {taken, {pharmacy, PharmacyId}}, {taken, {staff, PrescriberId}} ]
      = check_keys(Pid, [{patient, PatientId}, {pharmacy, PharmacyId}, {staff, PrescriberId}]),

    PrescriptionFields = [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
    HandleCreateOpResult = create_if_not_exists(Pid, prescription, PrescriptionFields),

    Result = case HandleCreateOpResult of
        ok ->
            Prescriptions = riakc_set:new(),
            Prescriptions1 = riakc_set:add_element(PrescriptionKey, Prescriptions),
            BucketType = <<"sets">>,
            BucketName = <<"prescriptions">>,
            lists:map(
                fun(Key) ->
                    riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_set:to_op(Prescriptions1))
                end, [PatientKey, PharmacyKey, PrescriberKey]
            ),
            ok;
        ErrorMessage -> ErrorMessage
    end,
    fmke_db_conn_manager:checkin(Pid),
    {reply, Result, State};

%% create (for facility, patient, pharmacy, staff)
handle_call({create, Entity, [Id | _T] = Fields}, _From, State) ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    Result = case check_key(Pid, Key, BucketType, BucketName) of
        taken ->
            {error, id_taken(Entity)};
        free ->
            LocalObject = gen_entity(Entity, Fields),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(LocalObject))
    end,
    fmke_db_conn_manager:checkin(Pid),
    {reply, Result, State};

handle_call({update, prescription, Id, Action}, _From, State) ->
    {Key, {BucketType, BucketName}} = get_riak_props(prescription, Id),
    Pid = fmke_db_conn_manager:checkout(),
    Result = case get_prescription_by_id(Id) of
        {error, not_found} ->
            {error, no_such_prescription};
        #prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE} ->
            {error, prescription_already_processed};
        P when is_record(P, prescription) ->
            Prescription = chg_presc(P, Action),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key,
                riakc_map:to_op(gen_entity(prescription, Prescription)))
    end,
    fmke_db_conn_manager:checkin(Pid),
    {reply, Result, State};

%% updates entity if it exists (for facility, patient, pharmacy and staff)
handle_call({update, Entity, [Id | _T] = Fields}, _From, {Driver, DataModel}) ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    Result = case check_key(Pid, Key, BucketType, BucketName) of
        free ->
            {error, no_such_entity(Entity)};
        taken ->
            EntityUpdate = gen_entity(Entity, Fields),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(EntityUpdate))
    end,
    fmke_db_conn_manager:checkin(Pid),
    {reply, Result, {Driver, DataModel}}.

get_riak_props(Entity, Id) when is_atom(Entity), is_integer(Id) ->
    BucketType = get_bucket_type(Entity),
    BucketName = get_bucket(Entity),
    Key = get_key(Entity, Id),
    {Key, {BucketType, BucketName}}.

get_bucket_type(_Entity) -> <<"maps">>.

get_bucket(facility) ->         <<"facilities">>;
get_bucket(patient) ->          <<"patients">>;
get_bucket(pharmacy) ->         <<"pharmacies">>;
get_bucket(prescription) ->     <<"prescriptions">>;
get_bucket(staff) ->            <<"staff">>.

chg_presc(#prescription{} = P, {date_processed, Date}) ->
    P#prescription{date_processed = Date, is_processed = ?PRESCRIPTION_PROCESSED_VALUE};

chg_presc(#prescription{} = P, {drugs, add, Drugs}) ->
    P#prescription{drugs = lists:append(P#prescription.drugs, Drugs)}.

create_if_not_exists(Pid, Entity, Fields) ->
    Id = hd(Fields),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    case check_key(Pid, Key, BucketType, BucketName) of
        taken ->
            {error, list_to_atom(lists:flatten(io_lib:format("~p_id_taken", [Entity])))};
        free ->
            LocalObject = gen_entity(Entity, Fields),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(LocalObject))
    end.

%% Checks if an entity exists
check_key(Pid, Key, BucketType, BucketName) ->
    case riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key) of
        {error, {notfound, _Type}} -> free;
        {ok, _Map} -> taken
    end.

get_key(Entity, Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p", [Entity, Id]))).

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

tryfetch(Key, KeyType, Map, DefaultVal) ->
    try
        riakc_map:fetch({Key, KeyType}, Map)
    catch
        _:_ -> DefaultVal
    end.

build_app_record(_, {error, not_found}) ->
    {error, not_found};
build_app_record(facility, Object) ->
    Id = riakc_map:fetch({?FACILITY_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?FACILITY_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?FACILITY_ADDRESS_KEY, register}, Object),
    Type = riakc_map:fetch({?FACILITY_TYPE_KEY, register}, Object),
    #facility{
        id = Id,
        name = Name,
        address = Address,
        type = Type
    };
build_app_record(patient, Object) ->
    Id = riakc_map:fetch({?PATIENT_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?PATIENT_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?PATIENT_ADDRESS_KEY, register}, Object),
    Prescriptions = find_key(Object, ?PATIENT_PRESCRIPTIONS_KEY, set, []),
    #patient{
        id = Id,
        name = Name,
        address = Address,
        prescriptions = Prescriptions
    };
build_app_record(pharmacy, Object) ->
    Id = riakc_map:fetch({?PHARMACY_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?PHARMACY_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?PHARMACY_ADDRESS_KEY, register}, Object),
    Prescriptions = find_key(Object, ?PHARMACY_PRESCRIPTIONS_KEY, set, []),
    #pharmacy{
        id = Id,
        name = Name,
        address = Address,
        prescriptions = Prescriptions
    };
build_app_record(prescription, Object) ->
    Id = tryfetch(?PRESCRIPTION_ID_KEY, register, Object, -1),
    PatientId = tryfetch(?PRESCRIPTION_PATIENT_ID_KEY, register, Object, -1),
    PrescriberId = tryfetch(?PRESCRIPTION_PRESCRIBER_ID_KEY, register, Object, -1),
    PharmacyId = tryfetch(?PRESCRIPTION_PHARMACY_ID_KEY, register, Object, -1),
    DatePrescribed = riakc_map:fetch({?PRESCRIPTION_DATE_PRESCRIBED_KEY, register}, Object),
    IsProcessed = tryfetch(?PRESCRIPTION_IS_PROCESSED_KEY, register, Object, ?PRESCRIPTION_NOT_PROCESSED_VALUE),
    DateProcessed = tryfetch(?PRESCRIPTION_DATE_PROCESSED_KEY, register, Object, <<"undefined">>),
    Drugs = riakc_map:fetch({?PRESCRIPTION_DRUGS_KEY, set}, Object),
    #prescription{
        id = Id,
        patient_id = PatientId,
        pharmacy_id = PharmacyId,
        prescriber_id = PrescriberId,
        date_prescribed = DatePrescribed,
        date_processed = DateProcessed,
        drugs = Drugs,
        is_processed = IsProcessed
    };
build_app_record(staff, Object) ->
    Id = riakc_map:fetch({?STAFF_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?STAFF_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?STAFF_ADDRESS_KEY, register}, Object),
    Speciality = riakc_map:fetch({?STAFF_SPECIALITY_KEY, register}, Object),
    Prescriptions = find_key(Object, ?STAFF_PRESCRIPTIONS_KEY, set, []),
    #staff{
        id = Id,
        name = Name,
        address = Address,
        speciality = Speciality,
        prescriptions = Prescriptions
    }.

get_entity_with_prescriptions(Entity, Id) ->
    Pid = fmke_db_conn_manager:checkout(),
    Result = get_entity_with_prescriptions(Pid, Entity, Id),
    fmke_db_conn_manager:checkin(Pid),
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
    Results = lists:map(
                fun({Key, BucketType, BucketName}) ->
                    riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key)
                end, Objects),
    lists:map(fun parse_read_result/1, Results).

% get_pharmacy_prescriptions(Id) ->
%     Key = get_key(pharmacy, Id),
%     Pid = fmke_db_conn_manager:checkout(),
%     [Prescriptions, ProcessedPrescriptions] = get_prescriptions(Pid, Key),
%     fmke_db_conn_manager:checkin(Pid),
%     case {Prescriptions, ProcessedPrefscriptions} of
%         {{error, not_found}, {error, not_found}} -> [];
%         {{error, not_found}, PrescriptionKeys2} ->  PrescriptionKeys2;
%         {PrescriptionKeys1, {error, not_found}} ->  PrescriptionKeys1;
%         {PrescriptionKeys1, PrescriptionKeys2} ->   lists:append(PrescriptionKeys1, PrescriptionKeys2)
%     end.
%
% get_processed_pharmacy_prescriptions(Id) ->
%     PharmacyKey = get_key(pharmacy, Id),
%     Pid = fmke_db_conn_manager:checkout(),
%     [ProcessedPrescriptions] = multi_read(Pid, [
%         {PharmacyKey, <<"sets">>, <<"processed_prescriptions">>}
%     ]),
%     fmke_db_conn_manager:checkin(Pid),
%     case ProcessedPrescriptions of
%         {error, not_found} -> [];
%         PrescriptionKeys ->  PrescriptionKeys
%     end.
%
% %% Fetches a prescription by ID.
% -spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
% get_prescription_by_id(Id) ->
%     build_app_record(prescription, process_get_request(prescription, Id)).
%
% %% Fetches prescription medication by ID.
% -spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
% get_prescription_medication(Id) ->
%     Prescription = get_prescription_by_id(Id),
%     case Prescription of
%       {error, _} -> {error, no_such_prescription};
%       _ -> Prescription#prescription.drugs
%     end.
%
% get_staff_by_id(Id) -> get_entity_with_prescriptions(staff, Id).
%
%
% get_staff_prescriptions(Id) ->
%     Key = get_key(staff, Id),
%     Pid = fmke_db_conn_manager:checkout(),
%     [Prescriptions, ProcessedPrescriptions] = get_prescriptions(Pid, Key),
%     fmke_db_conn_manager:checkin(Pid),
%     case {Prescriptions, ProcessedPrescriptions} of
%         {{error, not_found}, {error, not_found}} -> [];
%         {{error, not_found}, PrescriptionKeys2} ->  PrescriptionKeys2;
%         {PrescriptionKeys1, {error, not_found}} ->  PrescriptionKeys1;
%         {PrescriptionKeys1, PrescriptionKeys2} ->   lists:append(PrescriptionKeys1, PrescriptionKeys2)
%     end.

check_keys(_Pid, []) ->
    [];
check_keys(Pid, [H|T]) ->
    {Entity, Id} = H,
    case process_get_request(Pid, Entity, Id) of
        {error, not_found} -> [{free, H}] ++ check_keys(Pid, T);
        _Object -> [{taken, H}] ++ check_keys(Pid, T)
    end.

% process_prescription(Id, Date) ->
%     Pid = fmke_db_conn_manager:checkout(),
%     Result = process_prescription(Pid, Id, Date),
%     fmke_db_conn_manager:checkin(Pid),
%     Result.

% process_prescription(Pid, Id, Date) ->
%     RiakObject = process_get_request(Pid, prescription, Id),
%     Prescription = build_app_record(prescription, RiakObject),
%     case can_process_prescription(Prescription) of
%         {false, Reason} ->
%             {error, Reason};
%         true ->
%             PharmacyKey =      get_key(pharmacy, binary_to_integer(Prescription#prescription.pharmacy_id)),
%             PatientKey =       get_key(patient, binary_to_integer(Prescription#prescription.patient_id)),
%             StaffKey =         get_key(staff, binary_to_integer(Prescription#prescription.prescriber_id)),
%             PrescriptionKey =  get_key(prescription, Id),
%
%             NewMap1 = riakc_map:update({?PRESCRIPTION_IS_PROCESSED_KEY, register},
%                                 fun(R) -> riakc_register:set(?PRESCRIPTION_PROCESSED_VALUE, R) end,
%                             RiakObject),
%             NewMap = riakc_map:update({?PRESCRIPTION_DATE_PROCESSED_KEY, register},
%                                 fun(R) -> riakc_register:set(list_to_binary(Date), R) end,
%                             NewMap1),
%
%             [OpenPatPrescs, OpenPharmPrescs, OpenStaffPrescs] = multi_read(Pid, [
%                 {PatientKey, <<"sets">>, <<"prescriptions">>},
%                 {PharmacyKey, <<"sets">>, <<"prescriptions">>},
%                 {StaffKey, <<"sets">>, <<"prescriptions">>}
%             ]),
%
%             RemFromSetPat = riakc_set:del_element(PrescriptionKey, OpenPatPrescs),
%             RemFromSetPharm = riakc_set:del_element(PrescriptionKey, OpenPharmPrescs),
%             RemFromSetStaff = riakc_set:del_element(PrescriptionKey, OpenStaffPrescs),
%
%             ProcPrescsLocal = riakc_set:add_element(PrescriptionKey, riakc_set:new()),
%
%             BucketType = <<"sets">>,
%             PrscBucketType = {<<"maps">>, <<"prescriptions">>},
%             DelBucketType = {BucketType, <<"prescriptions">>},
%             AddBucketType = {BucketType, <<"processed_prescriptions">>},
%
%             riakc_pb_socket:update_type(Pid, PrscBucketType, PrescriptionKey, riakc_map:to_op(NewMap)),
%             riakc_pb_socket:update_type(Pid, DelBucketType, PatientKey, riakc_set:to_op(RemFromSetPat)),
%             riakc_pb_socket:update_type(Pid, DelBucketType, PharmacyKey, riakc_set:to_op(RemFromSetPharm)),
%             riakc_pb_socket:update_type(Pid, DelBucketType, StaffKey, riakc_set:to_op(RemFromSetStaff)),
%             riakc_pb_socket:update_type(Pid, AddBucketType, PatientKey, riakc_set:to_op(ProcPrescsLocal)),
%             riakc_pb_socket:update_type(Pid, AddBucketType, PharmacyKey, riakc_set:to_op(ProcPrescsLocal)),
%             riakc_pb_socket:update_type(Pid, AddBucketType, StaffKey, riakc_set:to_op(ProcPrescsLocal)),
%             ok
%    end.

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------

%% Uses a Pid connected to Riak to fetch the open prescriptions and processed prescriptions
%% Returns [OpenPrescriptions, ProcessedPrescriptions]
% get_prescriptions(Pid, Key) ->
%     multi_read(Pid, [
%         {Key, <<"sets">>, <<"prescriptions">>},
%         {Key, <<"sets">>, <<"processed_prescriptions">>}
%     ]).
%
% can_process_prescription({error, not_found}) ->
%     {false, no_such_prescription};
% can_process_prescription({error, Reason}) ->
%     {false, Reason};
% can_process_prescription(#prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE}) ->
%     {false, prescription_already_processed};
% can_process_prescription(#prescription{is_processed=?PRESCRIPTION_NOT_PROCESSED_VALUE}) ->
%     true.

process_get_request(Entity, Id) ->
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    parse_read_result(get(Key, BucketType, BucketName)).

process_get_request(Pid, Entity, Id) ->
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    parse_read_result(get(Pid, Key, BucketType, BucketName)).

parse_read_result({error, {notfound, _Type}}) -> {error, not_found};
parse_read_result({ok, Object}) -> Object.

get(Key, BucketType, BucketName) ->
    Pid = fmke_db_conn_manager:checkout(),
    Result = riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key),
    fmke_db_conn_manager:checkin(Pid),
    Result.

get(Pid, Key, BucketType, BucketName) ->
    riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key).

id_taken(facility) ->           facility_id_taken;
id_taken(patient) ->            patient_id_taken;
id_taken(pharmacy) ->           pharmacy_id_taken;
id_taken(prescription) ->       prescription_id_taken;
id_taken(staff) ->              staff_id_taken.

no_such_entity(facility) ->           no_such_facility;
no_such_entity(patient) ->            no_such_patient;
no_such_entity(pharmacy) ->           no_such_pharmacy;
no_such_entity(prescription) ->       no_such_prescription;
no_such_entity(staff) ->              no_such_staff.
