%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for Redis (cluster version)
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_redis_cluster).
%% NOTE: these dialyzer flags are here solely because of ERRORS in the type spec of eredis_cluster. In order to keep the
%% errors contained and to avoid hindering the purpose of dialyzer for other parts of FMKe, a minimal set of dialyzer
%% warnings were disabled only for this module.
-dialyzer([no_match, no_unused, no_return]).

-behaviour(fmke_gen_driver).
-behaviour(gen_server).

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
    {ok, Hosts} = application:get_env(?APP, hosts),
    {ok, Ports} = application:get_env(?APP, ports),
    {ok, PoolSize} = application:get_env(?APP, connection_pool_size),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Hosts, Ports, PoolSize], []).

stop(_) ->
    gen_server:stop(?SERVER).

init([Hosts, Ports, PoolSize]) ->
    InitNodes = lists:zip(Hosts, Ports),
    {ok, _Started} = application:ensure_all_started(eredis_cluster),
    application:set_env(eredis_cluster, pool_size, PoolSize),
    application:set_env(eredis_cluster, pool_max_overflow, 2*PoolSize),
    eredis_cluster:connect(InitNodes),
    {ok, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({update, prescription, Id, Action}, _From, State) ->
    %% facility, patient, pharmacy, staff
    PKey = gen_key(prescription, Id),
    {ok, ListFields} = eredis_cluster:q(["HGETALL", PKey]),
    Result = case ListFields of
        [] ->
            {error, no_such_prescription};
        _ ->
            P = parse_fields(prescription, ListFields),
            case P#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {error, prescription_already_processed};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    case Action of
                        {date_processed, Date} ->
                            [{ok, <<"OK">>}, {ok, <<"1">>}, {ok, <<"1">>},
                             {ok, <<"1">>}, {ok, <<"1">>}, {ok, <<"1">>},
                             {ok, <<"1">>}] = eredis_cluster:qmn([gen_update_op(prescription, [
                                P#prescription.id,
                                P#prescription.patient_id,
                                P#prescription.prescriber_id,
                                P#prescription.pharmacy_id,
                                P#prescription.date_prescribed,
                                P#prescription.drugs,
                                Date
                            ]),
                            ["SREM", gen_prescriptions_key(patient, P#prescription.patient_id), PKey],
                            ["SREM", gen_prescriptions_key(pharmacy, P#prescription.pharmacy_id), PKey],
                            ["SREM", gen_prescriptions_key(staff, P#prescription.prescriber_id), PKey],
                            ["SADD", gen_processed_prescriptions_key(patient, P#prescription.patient_id), PKey],
                            ["SADD", gen_processed_prescriptions_key(pharmacy, P#prescription.pharmacy_id), PKey],
                            ["SADD", gen_processed_prescriptions_key(staff, P#prescription.prescriber_id), PKey]
                            ]),
                            ok;
                        {drugs, add, Drugs} ->
                            NewDrugs = P#prescription.drugs ++ Drugs,
                            {ok, [<<"OK">>]} = eredis_cluster:transaction([gen_update_op(prescription, [
                                P#prescription.id,
                                P#prescription.patient_id,
                                P#prescription.prescriber_id,
                                P#prescription.pharmacy_id,
                                P#prescription.date_prescribed,
                                NewDrugs
                            ])]),
                            ok
                    end
            end
    end,
    {reply, Result, State};

handle_call({update, Entity, [Id | _Fs] = Fields}, _From, State) ->
    %% facility, patient, pharmacy, staff
    {ok, ListFields} = eredis_cluster:q(["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, no_such_entity(Entity)};
        _ ->
            {ok, [<<"OK">>]} = eredis_cluster:transaction([gen_update_op(Entity, Fields)]),
            ok
    end,
    {reply, Result, State};

handle_call({read, Entity, Id}, _From, State) ->
    %% facility, patient, pharmacy, staff
    {ok, ListFields} = eredis_cluster:q(["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, not_found};
        [_H| _T] when Entity == facility; Entity == prescription ->
            parse_fields(Entity, ListFields);
        [_H| _T] when Entity == patient; Entity == pharmacy; Entity == staff ->
            %% these entities have prescriptions associated with them
            [{ok, Prescs}, {ok, ProcPrescs}] = eredis_cluster:qmn([["SMEMBERS", gen_prescriptions_key(Entity, Id)],
                                                            ["SMEMBERS", gen_processed_prescriptions_key(Entity, Id)]]),
            parse_fields(Entity, ListFields ++ [entity_prescriptions_key(Entity), Prescs ++ ProcPrescs])
    end,
    {reply, Result, State};

handle_call({read, Entity, Id, prescriptions}, _From, State) ->
    %% patient, pharmacy, staff
    {ok, ListFields} = eredis_cluster:q(["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, no_such_entity(Entity)};
        [_H| _T] when Entity == patient; Entity == pharmacy; Entity == staff ->
            [{ok, Prescs}, {ok, ProcPrescs}] = eredis_cluster:qmn([["SMEMBERS", gen_prescriptions_key(Entity, Id)],
                                                            ["SMEMBERS", gen_processed_prescriptions_key(Entity, Id)]]),
            Prescs ++ ProcPrescs
    end,
    {reply, Result, State};

handle_call({read, Entity, Id, processed_prescriptions}, _From, State) ->
    %% patient, pharmacy, staff
    {ok, ListFields} = eredis_cluster:q(["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, no_such_entity(Entity)};
        [_H|_T] when Entity == patient; Entity == pharmacy; Entity == staff ->
            [{ok, ProcPrescs}] = eredis_cluster:qmn([["SMEMBERS", gen_processed_prescriptions_key(Entity, Id)]]),
            ProcPrescs
    end,
    {reply, Result, State};

handle_call({create, prescription, [Id, PatId, DocId, PharmId | _Fs] = Fields}, _From, State) ->
    %% facility, patient, pharmacy, staff
    [{ok, Presc}, {ok, Pat}, {ok, Doc}, {ok, Pharm}] = eredis_cluster:qmn([
        ["HGETALL", gen_key(prescription, Id)]
        ,["HGETALL", gen_key(patient, PatId)]
        ,["HGETALL", gen_key(staff, DocId)]
        ,["HGETALL", gen_key(pharmacy, PharmId)]
    ]),
    Result = case Presc of
        [_H|_T] ->
            {error, prescription_id_taken};
        [] when Pat == [] ->
            {error, no_such_patient};
        [] when Doc == [] ->
            {error, no_such_staff};
        [] when Pharm == [] ->
            {error, no_such_pharmacy};
        _ ->
            PKey = gen_key(prescription, Id),
            [{ok, <<"OK">>}, {ok, <<"1">>}, {ok, <<"1">>}, {ok, <<"1">>}] = eredis_cluster:qmn([
                gen_update_op(prescription, Fields),
                ["SADD", gen_prescriptions_key(patient, PatId), PKey],
                ["SADD", gen_prescriptions_key(pharmacy, PharmId), PKey],
                ["SADD", gen_prescriptions_key(staff, DocId), PKey]
            ]),
            ok
    end,
    {reply, Result, State};

handle_call({create, Entity, [Id | _Fs] = Fields}, _From, State) ->
    %% facility, patient, pharmacy, staff
    {ok, ListFields} = eredis_cluster:q(["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {ok, [<<"OK">>]} = eredis_cluster:transaction([gen_update_op(Entity, Fields)]),
            ok;
        _ ->
            {error, id_taken(Entity)}
    end,
    {reply, Result, State}.

new_rec(facility) ->        #facility{};
new_rec(patient) ->         #patient{};
new_rec(pharmacy) ->        #pharmacy{};
new_rec(prescription) ->    #prescription{};
new_rec(staff) ->           #staff{}.

parse_fields(Entity, Fields) ->
    parse_fields(Entity, Fields, new_rec(Entity)).

parse_fields(_Entity, [], Accum) ->
    Accum;

parse_fields(facility, [?FACILITY_ID_KEY, Id | Rest], Fac) ->
    parse_fields(facility, Rest, Fac#facility{id = Id});
parse_fields(facility, [?FACILITY_NAME_KEY, Name | Rest], Fac) ->
    parse_fields(facility, Rest, Fac#facility{name = Name});
parse_fields(facility, [?FACILITY_ADDRESS_KEY, Address | Rest], Fac) ->
    parse_fields(facility, Rest, Fac#facility{address = Address});
parse_fields(facility, [?FACILITY_TYPE_KEY, Type | Rest], Fac) ->
    parse_fields(facility, Rest, Fac#facility{type = Type});

parse_fields(patient, [?PATIENT_ID_KEY, Id | Rest], Pat) ->
    parse_fields(patient, Rest, Pat#patient{id = Id});
parse_fields(patient, [?PATIENT_NAME_KEY, Name | Rest], Pat) ->
    parse_fields(patient, Rest, Pat#patient{name = Name});
parse_fields(patient, [?PATIENT_ADDRESS_KEY, Address | Rest], Pat) ->
    parse_fields(patient, Rest, Pat#patient{address = Address});
parse_fields(patient, [?PATIENT_PRESCRIPTIONS_KEY, Prescriptions | Rest], Pat) ->
    parse_fields(patient, Rest, Pat#patient{prescriptions = Prescriptions});

parse_fields(pharmacy, [?PHARMACY_ID_KEY, Id | Rest], Pharm) ->
    parse_fields(pharmacy, Rest, Pharm#pharmacy{id = Id});
parse_fields(pharmacy, [?PHARMACY_NAME_KEY, Name | Rest], Pharm) ->
    parse_fields(pharmacy, Rest, Pharm#pharmacy{name = Name});
parse_fields(pharmacy, [?PHARMACY_ADDRESS_KEY, Address | Rest], Pharm) ->
    parse_fields(pharmacy, Rest, Pharm#pharmacy{address = Address});
parse_fields(pharmacy, [?PHARMACY_PRESCRIPTIONS_KEY, Prescriptions | Rest], Pharm) ->
    parse_fields(pharmacy, Rest, Pharm#pharmacy{prescriptions = Prescriptions});

parse_fields(prescription, [?PRESCRIPTION_ID_KEY, Id | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{id = Id});
parse_fields(prescription, [?PRESCRIPTION_PATIENT_ID_KEY, PatId | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{patient_id = PatId});
parse_fields(prescription, [?PRESCRIPTION_PRESCRIBER_ID_KEY, DocId | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{prescriber_id = DocId});
parse_fields(prescription, [?PRESCRIPTION_PHARMACY_ID_KEY, PharmId | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{pharmacy_id = PharmId});
parse_fields(prescription, [?PRESCRIPTION_DATE_PRESCRIBED_KEY, Date | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{date_prescribed = Date});
parse_fields(prescription, [?PRESCRIPTION_DATE_PROCESSED_KEY, Date | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{date_processed = Date});
parse_fields(prescription, [?PRESCRIPTION_IS_PROCESSED_KEY, Flag | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{is_processed = Flag});
parse_fields(prescription, [?PRESCRIPTION_DRUGS_KEY, Drugs | Rest], P) ->
    parse_fields(prescription, Rest, P#prescription{drugs = unpack_drugs(binary_to_list(Drugs))});

parse_fields(staff, [?STAFF_ID_KEY, Id | Rest], Staff) ->
    parse_fields(staff, Rest, Staff#staff{id = Id});
parse_fields(staff, [?STAFF_NAME_KEY, Name | Rest], Staff) ->
    parse_fields(staff, Rest, Staff#staff{name = Name});
parse_fields(staff, [?STAFF_ADDRESS_KEY, Address | Rest], Staff) ->
    parse_fields(staff, Rest, Staff#staff{address = Address});
parse_fields(staff, [?STAFF_SPECIALITY_KEY, Speciality | Rest], Staff) ->
    parse_fields(staff, Rest, Staff#staff{speciality = Speciality});
parse_fields(staff, [?STAFF_PRESCRIPTIONS_KEY, Prescriptions | Rest], Staff) ->
    parse_fields(staff, Rest, Staff#staff{prescriptions = Prescriptions}).

gen_update_op(facility, [Id, Name, Address, Type]) ->
    Key = "facility_" ++ bin_or_int_to_list(Id),
    ["HMSET", Key, ?FACILITY_ID_KEY, Id, ?FACILITY_NAME_KEY, Name,
                   ?FACILITY_ADDRESS_KEY, Address, ?FACILITY_TYPE_KEY, Type];
gen_update_op(patient, [Id, Name, Address]) ->
    Key = "patient_" ++ bin_or_int_to_list(Id),
    ["HMSET", Key, ?PATIENT_ID_KEY, Id, ?PATIENT_NAME_KEY, Name, ?PATIENT_ADDRESS_KEY, Address];
gen_update_op(pharmacy, [Id, Name, Address]) ->
    Key = "pharmacy_" ++ bin_or_int_to_list(Id),
    ["HMSET", Key, ?PHARMACY_ID_KEY, Id, ?PHARMACY_NAME_KEY, Name, ?PHARMACY_ADDRESS_KEY, Address];
gen_update_op(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]) ->
    Key = "prescription_" ++ bin_or_int_to_list(Id),
    ["HMSET", Key, ?PRESCRIPTION_ID_KEY, Id, ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
    ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId, ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
    ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed, ?PRESCRIPTION_DRUGS_KEY, pack_drugs(Drugs)];
gen_update_op(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs, DateProcessed]) ->
   Key = "prescription_" ++ bin_or_int_to_list(Id),
   [
       "HMSET", Key,
       ?PRESCRIPTION_ID_KEY, Id,
       ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
       ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId,
       ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
       ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed,
       ?PRESCRIPTION_DRUGS_KEY, pack_drugs(Drugs),
       ?PRESCRIPTION_DATE_PROCESSED_KEY, DateProcessed,
       ?PRESCRIPTION_IS_PROCESSED_KEY, ?PRESCRIPTION_PROCESSED_VALUE
   ];
gen_update_op(staff, [Id, Name, Address, Speciality]) ->
    Key = "staff_" ++ integer_to_list(Id),
    [
        "HMSET", Key, ?STAFF_ID_KEY, Id, ?STAFF_NAME_KEY, Name, ?STAFF_ADDRESS_KEY, Address,
        ?STAFF_SPECIALITY_KEY, Speciality
    ].

pack_drugs([]) -> "";
pack_drugs([H]) -> H;
pack_drugs([_H|_T] = L) -> string:join(L, ";;").

unpack_drugs(Drugs) ->
    string:tokens(Drugs, ";;").

bin_or_int_to_list(Id) when is_integer(Id) ->
    integer_to_list(Id);
bin_or_int_to_list(Id) when is_binary(Id) ->
    binary_to_list(Id).

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
    gen_server:call(?MODULE, {read, pharmacy, Id, processed_prescriptions}).

get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {read, pharmacy, Id, prescriptions}).

get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {read, prescription, Id}).

get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {read, prescription, Id, [drugs]}).

get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {read, staff, Id}).

get_staff_prescriptions(Id) ->
    gen_server:call(?MODULE, {read, staff, Id, prescriptions}).

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

gen_key(Entity, Id) ->
    atom_to_list(Entity) ++ "_" ++ bin_or_int_to_list(Id).

gen_prescriptions_key(Entity, Id) ->
    atom_to_list(Entity) ++ "_" ++ bin_or_int_to_list(Id) ++ "_prescriptions".

gen_processed_prescriptions_key(Entity, Id) ->
    atom_to_list(Entity) ++ "_" ++ bin_or_int_to_list(Id) ++ "_processed_prescriptions".

entity_prescriptions_key(patient) ->
    ?PATIENT_PRESCRIPTIONS_KEY;
entity_prescriptions_key(pharmacy) ->
    ?PHARMACY_PRESCRIPTIONS_KEY;
entity_prescriptions_key(staff) ->
    ?STAFF_PRESCRIPTIONS_KEY.

no_such_entity(facility) -> no_such_facility;
no_such_entity(patient) -> no_such_patient;
no_such_entity(pharmacy) -> no_such_pharmacy;
no_such_entity(prescription) -> no_such_prescription;
no_such_entity(staff) -> no_such_staff.

id_taken(facility) -> facility_id_taken;
id_taken(patient) -> patient_id_taken;
id_taken(pharmacy) -> pharmacy_id_taken;
id_taken(prescription) -> prescription_id_taken;
id_taken(staff) -> staff_id_taken.
