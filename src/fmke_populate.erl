-module(fmke_populate).

-include("fmke.hrl").
-include("fmke_kv.hrl").

-export([start/0, stop/0]).

-export ([init/1, handle_call/3, handle_cast/2]).

-export([
    supported/0,
    create_facility/4,
    create_patient/3,
    create_pharmacy/3,
    create_prescription/6,
    create_staff/4
]).

-define(SERVER, ?MODULE).

supported() ->
    [redis_cluster, redis_crdb, riak].

start() ->
    {ok, Database} = application:get_env(?APP, target_database),
    {ok, DataModel} = application:get_env(?APP, data_model),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Database, DataModel], []).

stop() ->
    gen_server:stop(?SERVER).

init([Database, DataModel]) ->
    {ok, {Database, DataModel}}.

create_facility(Id, Name, Address, Type) ->
    gen_server:call(?SERVER, {facility, [Id, Name, Address, Type]}).

create_patient(Id, Name, Address) ->
    gen_server:call(?SERVER, {patient, [Id, Name, Address]}).

create_pharmacy(Id, Name, Address) ->
    gen_server:call(?SERVER, {pharmacy, [Id, Name, Address]}).

create_staff(Id, Name, Address, Speciality) ->
    gen_server:call(?SERVER, {staff, [Id, Name, Address, Speciality]}).

create_prescription(Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    gen_server:call(?SERVER, {prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}).

handle_call({Entity, Fields}, _From, {Database, DataModel} = State) ->
    {reply, do_add(Database, DataModel, Entity, Fields), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

do_add(redis_cluster, _, Entity, Fields) ->
    Updates = gen_riak_updates(Entity, Fields),
    eredis_cluster:qmn(Updates),
    ok;

do_add(redis_crdb, _, Entity, Fields) ->
    Pid = fmke_db_conn_manager:checkout(),
    Updates = gen_riak_updates(Entity, Fields),
    eredis:qp(Pid, Updates),
    fmke_db_conn_manager:checkin(Pid),
    ok;

do_add(riak, _, Entity, Fields) ->
    Pid = fmke_db_conn_manager:checkout(),
    Updates = gen_riak_updates(Entity, Fields),
    lists:map(fun({Key, Bucket, Value})->
            ok = riakc_pb_socket:update_type(Pid, Bucket, Key, Value)
        end, Updates),
    fmke_db_conn_manager:checkin(Pid),
    ok.

gen_redis_updates(facility, [Id, Name, Address, Type]) ->
    Key = "facility_" ++ bin_or_int_to_list(Id),
    [["HMSET", Key, ?FACILITY_ID_KEY, Id, ?FACILITY_NAME_KEY, Name,
                   ?FACILITY_ADDRESS_KEY, Address, ?FACILITY_TYPE_KEY, Type]];
gen_redis_updates(patient, [Id, Name, Address]) ->
    Key = "patient_" ++ bin_or_int_to_list(Id),
    [["HMSET", Key, ?PATIENT_ID_KEY, Id, ?PATIENT_NAME_KEY, Name, ?PATIENT_ADDRESS_KEY, Address]];
gen_redis_updates(pharmacy, [Id, Name, Address]) ->
    Key = "pharmacy_" ++ bin_or_int_to_list(Id),
    [["HMSET", Key, ?PHARMACY_ID_KEY, Id, ?PHARMACY_NAME_KEY, Name, ?PHARMACY_ADDRESS_KEY, Address]];
gen_redis_updates(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]) ->
    Key = "prescription_" ++ bin_or_int_to_list(Id),
    [
        ["HMSET", Key, ?PRESCRIPTION_ID_KEY, Id, ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
         ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId, ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
         ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed, ?PRESCRIPTION_DRUGS_KEY, encode_drugs_redis(Drugs)],
         ["SADD", gen_redis_prescriptions_key(patient, PatId), Key],
         ["SADD", gen_redis_prescriptions_key(pharmacy, PharmId), Key],
         ["SADD", gen_redis_prescriptions_key(staff, DocId), Key]
    ];
gen_redis_updates(staff, [Id, Name, Address, Speciality]) ->
    Key = "staff_" ++ integer_to_list(Id),
    [["HMSET", Key, ?STAFF_ID_KEY, Id, ?STAFF_NAME_KEY, Name,
      ?STAFF_ADDRESS_KEY, Address, ?STAFF_SPECIALITY_KEY, Speciality]].

encode_drugs_redis([]) -> "";
encode_drugs_redis([H]) -> H;
encode_drugs_redis([_H|_T] = L) -> string:join(L, ";;").

gen_redis_prescriptions_key(Entity, Id) ->
      atom_to_list(Entity) ++ "_" ++ bin_or_int_to_list(Id) ++ "_prescriptions".

gen_riak_updates(Entity, Fields) ->
    gen_riak_updates(Entity, Fields, riakc_map:new()).

gen_riak_updates(patient, [Id, Name, Address], Map) ->
    {Key, Bucket} = get_riak_props(patient, Id),
    Map1 = riakc_map:update({?PATIENT_ID_KEY, register}, riak_reg_set_int(Id), Map),
    Map2 = riakc_map:update({?PATIENT_NAME_KEY, register}, riak_reg_set_str(Name), Map1),
    Map3 = riakc_map:update({?PATIENT_ADDRESS_KEY, register}, riak_reg_set_str(Address), Map2),
    [{Key, Bucket, riakc_map:to_op(Map3)}];
gen_riak_updates(pharmacy, [Id, Name, Address], Map) ->
    {Key, Bucket} = get_riak_props(pharmacy, Id),
    Map1 = riakc_map:update({?PHARMACY_ID_KEY, register}, riak_reg_set_int(Id), Map),
    Map2 = riakc_map:update({?PHARMACY_NAME_KEY, register}, riak_reg_set_str(Name), Map1),
    Map3 = riakc_map:update({?PHARMACY_ADDRESS_KEY, register}, riak_reg_set_str(Address), Map2),
    [{Key, Bucket, riakc_map:to_op(Map3)}];
gen_riak_updates(facility, [Id, Name, Address, Type], Map) ->
    {Key, Bucket} = get_riak_props(facility, Id),
    Map1 = riakc_map:update({?FACILITY_ID_KEY, register}, riak_reg_set_int(Id), Map),
    Map2 = riakc_map:update({?FACILITY_NAME_KEY, register}, riak_reg_set_str(Name), Map1),
    Map3 = riakc_map:update({?FACILITY_ADDRESS_KEY, register}, riak_reg_set_str(Address), Map2),
    Map4 = riakc_map:update({?FACILITY_TYPE_KEY, register}, riak_reg_set_str(Type), Map3),
    [{Key, Bucket, riakc_map:to_op(Map4)}];
gen_riak_updates(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs], Map) ->
    {Key, Bucket} = get_riak_props(prescription, Id),
    Map1 = riakc_map:update({?PRESCRIPTION_ID_KEY, register}, riak_reg_set_int(Id), Map),
    Map2 = riakc_map:update({?PRESCRIPTION_PATIENT_ID_KEY, register}, riak_reg_set_int(PatientId), Map1),
    Map3 = riakc_map:update({?PRESCRIPTION_PHARMACY_ID_KEY, register}, riak_reg_set_int(PharmacyId), Map2),
    Map4 = riakc_map:update({?PRESCRIPTION_PRESCRIBER_ID_KEY, register}, riak_reg_set_int(PrescriberId), Map3),
    Map5 = riakc_map:update({?PRESCRIPTION_DATE_PRESCRIBED_KEY, register}, riak_reg_set_str(DatePrescribed), Map4),
    Map6 = riakc_map:update({?PRESCRIPTION_DRUGS_KEY, set}, riak_set_add_elems(Drugs), Map5),
    Map7 = riakc_map:update({?PRESCRIPTION_IS_PROCESSED_KEY, register},
                            riak_reg_set_bin(?PRESCRIPTION_NOT_PROCESSED_VALUE), Map6),

    RefBucket = {<<"sets">>, <<"prescriptions">>},
    PatKey = binary_key(patient, PatientId),
    PharmKey = binary_key(pharmacy, PharmacyId),
    StaffKey = binary_key(staff, PrescriberId),
    [
        {Key, Bucket, riakc_map:to_op(Map7)},
        {PatKey, RefBucket, riakc_set:to_op(riakc_set:add_element(Key, riakc_set:new()))},
        {PharmKey, RefBucket, riakc_set:to_op(riakc_set:add_element(Key, riakc_set:new()))},
        {StaffKey, RefBucket, riakc_set:to_op(riakc_set:add_element(Key, riakc_set:new()))}
    ];
gen_riak_updates(staff, [Id, Name, Address, Speciality], Map) ->
    {Key, Bucket} = get_riak_props(staff, Id),
    Map1 = riakc_map:update({?STAFF_ID_KEY, register}, riak_reg_set_str(integer_to_list(Id)), Map),
    Map2 = riakc_map:update({?STAFF_NAME_KEY, register}, riak_reg_set_str(Name), Map1),
    Map3 = riakc_map:update({?STAFF_ADDRESS_KEY, register}, riak_reg_set_str(Address), Map2),
    Map4 = riakc_map:update({?STAFF_SPECIALITY_KEY, register}, riak_reg_set_str(Speciality), Map3),
    [{Key, Bucket, riakc_map:to_op(Map4)}].

get_riak_props(Entity, Id) when is_atom(Entity), is_integer(Id) ->
    BucketType = riak_bucket_type(Entity),
    BucketName = riak_bucket(Entity),
    Key = binary_key(Entity, Id),
    {Key, {BucketType, BucketName}}.

riak_reg_set_bin(Val) ->
    fun(R) -> riakc_register:set(Val, R) end.

riak_reg_set_str(Val) ->
    fun(R) -> riakc_register:set(list_to_binary(Val), R) end.

riak_reg_set_int(Val) ->
    riak_reg_set_str(integer_to_list(Val)).

riak_set_add_elems(Vals) ->
    fun(S) -> riakc_set:add_elements(lists:map(fun list_to_binary/1, Vals), S) end.

riak_bucket_type(_Entity) -> <<"maps">>.

riak_bucket(facility) ->         <<"facilities">>;
riak_bucket(patient) ->          <<"patients">>;
riak_bucket(pharmacy) ->         <<"pharmacies">>;
riak_bucket(prescription) ->     <<"prescriptions">>;
riak_bucket(staff) ->            <<"staff">>.

binary_key(Entity, Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p", [Entity, Id]))).

bin_or_int_to_list(Id) when is_integer(Id) ->
    integer_to_list(Id);
bin_or_int_to_list(Id) when is_binary(Id) ->
    binary_to_list(Id).
