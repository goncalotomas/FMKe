%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for Riak KV, featuring a normalized data model (no CRDT nesting).
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_riak_kv).

% -behaviour(fmke_gen_driver).
-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server exports
-export ([
    start_link/1
    ,stop/1
    ,init/1
    ,handle_cast/2
    ,handle_call/3
]).

-define(SERVER, ?MODULE).
-define(PRESC_BUCKET, <<"prescriptions">>).
-define(BUCKET_TYPE, <<"maps">>).
-define(REF_BUCKET_TYPE, <<"sets">>).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init(_) ->
    {ok, _Started} = application:ensure_all_started(riak_client),
    {ok, {}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, State) ->
    Reply = call(Op),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, State}.

call({read, Entity, Id, prescriptions}) ->
    {Key, {_BucketType, _BucketName}} = get_riak_props(Entity, Id),
    Pid = fmke_db_conn_manager:checkout(),
    Obj = process_get_request(Pid, Entity, Id),
    Rec = build_app_record(Entity, Obj),
    Result = case Rec of
        {error, not_found} ->
            {error, no_such_entity(Entity)};
        _ ->
            [Prescriptions] = multi_read(Pid, [{Key, <<"sets">>, <<"prescriptions">>}]),
            case Prescriptions of
                {error, not_found} -> [];
                Set -> riakc_set:value(Set)
            end
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, Entity, Id, processed_prescriptions}) ->
    {Key, {_BucketType, _BucketName}} = get_riak_props(Entity, Id),
    Pid = fmke_db_conn_manager:checkout(),
    Obj = process_get_request(Pid, Entity, Id),
    Rec = build_app_record(Entity, Obj),
    Result = case Rec of
        {error, not_found} ->
            {error, no_such_entity(Entity)};
        _ ->
            [Prescriptions] = multi_read(Pid, [{Key, <<"sets">>, <<"prescriptions">>}]),
            case Prescriptions of
                {error, not_found} -> [];
                Set ->
                    Keys = riakc_set:value(Set),
                    BucketType = ?BUCKET_TYPE,
                    BucketName = get_bucket(prescription),
                    PrescObjs = lists:map(fun(PRef) ->
                        build_app_record(prescription, parse_read_result(get(Pid, PRef, BucketType, BucketName)))
                    end, Keys),
                    lists:filter(fun(P) -> P#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE end, PrescObjs)
            end
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, prescription, Id, drugs}) ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(prescription, Id),
    Prescription = build_app_record(prescription, parse_read_result(get(Pid, Key, BucketType, BucketName))),
    Result = case Prescription of
        {error, _} -> {error, no_such_prescription};
        _ -> Prescription#prescription.drugs
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, Entity, Id}) when Entity =:= patient; Entity =:= pharmacy; Entity =:= staff ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    [EntityObject, Prescriptions] = multi_read(Pid, [
      {Key, BucketType, BucketName},
      {Key, <<"sets">>, <<"prescriptions">>}
    ]),
    Result = case Prescriptions of
        {error, not_found} -> build_app_record(Entity, EntityObject);
        PrescsSet ->
            {map, Values, Updates, Removals, Context} = EntityObject,
            NewValues = orddict:append({get_prescriptions_key(Entity), set}, riakc_set:value(PrescsSet), Values),
            build_app_record(Entity, {map, NewValues, Updates, Removals, Context})
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, Entity, Id}) ->
    build_app_record(Entity, process_get_request(Entity, Id));

call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}) ->
    Pid = fmke_db_conn_manager:checkout(),
    %% gather required keys
    Key = get_key(prescription, Id),
    PatientKey = get_key(patient, PatientId),
    PharmacyKey = get_key(pharmacy, PharmacyId),
    PrescriberKey = get_key(staff, PrescriberId),

    Res = case missing_keys(check_keys(Pid, [{patient, PatientId}, {pharmacy, PharmacyId}, {staff, PrescriberId}])) of
        {true, patient} -> {error, no_such_patient};
        {true, pharmacy} -> {error, no_such_pharmacy};
        {true, staff} -> {error, no_such_staff};
        false ->
            PrescriptionFields = [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs],
            HandleCreateOpResult = create_if_not_exists(Pid, prescription, PrescriptionFields),

            case HandleCreateOpResult of
                ok ->
                    add_presc_ref(Pid, ?PRESC_BUCKET, PatientKey, Key),
                    add_presc_ref(Pid, ?PRESC_BUCKET, PharmacyKey, Key),
                    add_presc_ref(Pid, ?PRESC_BUCKET, PrescriberKey, Key);
                ErrorMessage ->
                    ErrorMessage
            end
    end,
    fmke_db_conn_manager:checkin(Pid),
    Res;

%% create (for facility, patient, pharmacy, staff)
call({create, Entity, [Id | _T] = Fields}) ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    Result = case check_key(Pid, Key, BucketType, BucketName) of
        taken ->
            {error, id_taken(Entity)};
        free ->
            LocalObject = gen_entity(Entity, Fields),
            ok = riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(LocalObject))
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({update, prescription, Id, Action}) ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(prescription, Id),
    PrescObj = process_get_request(Pid, prescription, Id),
    PrescRec = build_app_record(prescription, PrescObj),
    Result = case PrescRec of
        {error, not_found} ->
            {error, no_such_prescription};
        #prescription{is_processed=?PRESCRIPTION_PROCESSED_VALUE} ->
            {error, prescription_already_processed};
        P when is_record(P, prescription) ->
            case Action of
                {drugs, add, Drugs} ->
                    PrescObj1 = riakc_map:update({?PRESCRIPTION_DRUGS_KEY, set}, fun(S) ->
                            riakc_set:add_elements(lists:map(fun list_to_binary/1, Drugs), S)
                    end, PrescObj),
                    riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(PrescObj1));
                {date_processed, DateProcessed} ->
                    PrescObj1 = riakc_map:update({?PRESCRIPTION_DATE_PROCESSED_KEY, register},
                                        fun(R) -> riakc_register:set(list_to_binary(DateProcessed), R) end,
                                        PrescObj),
                    PrescObj2 = riakc_map:update({?PRESCRIPTION_IS_PROCESSED_KEY, register},
                                        fun(R) -> riakc_register:set(?PRESCRIPTION_PROCESSED_VALUE, R) end,
                                        PrescObj1),

                    ok = riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(PrescObj2))
            end
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

%% updates entity if it exists (for facility, patient, pharmacy and staff)
call({update, Entity, [Id | _T] = Fields}) ->
    Pid = fmke_db_conn_manager:checkout(),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    Obj = process_get_request(Pid, Entity, Id),
    Rec = build_app_record(Entity, Obj),
    Result = case Rec of
        {error, not_found} ->
            {error, no_such_entity(Entity)};
        _ ->
            EntityUpdate = gen_entity(Entity, Fields, Obj),
            riakc_pb_socket:update_type(Pid, {BucketType, BucketName}, Key, riakc_map:to_op(EntityUpdate))
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result.

missing_keys([]) ->
    false;
missing_keys([{taken, _Key} | T]) ->
    missing_keys(T);
missing_keys([{free, {Entity, _Key}} | _T]) ->
    {true, Entity}.

add_presc_ref(Pid, Bucket, Key, PKey) ->
    RefList = parse_read_result(riakc_pb_socket:fetch_type(Pid, {?REF_BUCKET_TYPE, Bucket}, Key)),
    case RefList of
        {error, not_found} ->
            ok = riakc_pb_socket:update_type(Pid, {?REF_BUCKET_TYPE, Bucket}, Key,
                                             riakc_set:to_op(riakc_set:add_element(PKey, riakc_set:new())));
        _ ->
            ok = riakc_pb_socket:update_type(Pid, {?REF_BUCKET_TYPE, Bucket}, Key,
                                 riakc_set:to_op(riakc_set:add_element(PKey, RefList)))
    end.

get_riak_props(Entity, Id) when is_atom(Entity), is_integer(Id) ->
    BucketName = get_bucket(Entity),
    Key = get_key(Entity, Id),
    {Key, {?BUCKET_TYPE, BucketName}}.

get_bucket(facility) ->         <<"facilities">>;
get_bucket(patient) ->          <<"patients">>;
get_bucket(pharmacy) ->         <<"pharmacies">>;
get_bucket(prescription) ->     <<"prescriptions">>;
get_bucket(staff) ->            <<"staff">>.

create_if_not_exists(Pid, Entity, Fields) ->
    Id = hd(Fields),
    {Key, {BucketType, BucketName}} = get_riak_props(Entity, Id),
    case check_key(Pid, Key, BucketType, BucketName) of
        taken ->
            {error, id_taken(Entity)};
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

get_key(facility, Id) ->        binary_str_w_int("facility_", Id);
get_key(patient, Id) ->         binary_str_w_int("patient_", Id);
get_key(pharmacy, Id) ->        binary_str_w_int("pharmacy_", Id);
get_key(prescription, Id) ->    binary_str_w_int("prescription_", Id);
get_key(staff, Id) ->           binary_str_w_int("staff_", Id).

binary_str_w_int(Str, Int) when is_integer(Int) ->
    list_to_binary(unicode:characters_to_list([Str, integer_to_list(Int)])).

gen_entity(Entity, Fields) ->
    gen_entity(Entity, Fields, riakc_map:new()).

gen_entity(patient, [Id, Name, Address], Map) ->
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
gen_entity(pharmacy, [Id, Name, Address], Map) ->
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
gen_entity(facility, [Id, Name, Address, Type], Map) ->
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
gen_entity(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs], Map) ->
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
gen_entity(staff, [Id, Name, Address, Speciality], Map) ->
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

get_prescriptions_key(patient) -> ?PATIENT_PRESCRIPTIONS_KEY;
get_prescriptions_key(pharmacy) -> ?PHARMACY_PRESCRIPTIONS_KEY;
get_prescriptions_key(staff) -> ?STAFF_PRESCRIPTIONS_KEY.

multi_read(Pid, Objects) ->
    Results = lists:map(
                fun({Key, BucketType, BucketName}) ->
                    riakc_pb_socket:fetch_type(Pid, {BucketType, BucketName}, Key)
                end, Objects),
    lists:map(fun parse_read_result/1, Results).

check_keys(_Pid, []) ->
    [];
check_keys(Pid, [H|T]) ->
    {Entity, Id} = H,
    case process_get_request(Pid, Entity, Id) of
        {error, not_found} -> [{free, H}] ++ check_keys(Pid, T);
        _Object -> [{taken, H}] ++ check_keys(Pid, T)
    end.

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------

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
