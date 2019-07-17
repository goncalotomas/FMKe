%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for Redis Enterprise CRDB
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_redis_crdb).
%% NOTE: these dialyzer flags are here solely because of ERRORS in the type spec of eredis. In order to keep the
%% errors contained and to avoid hindering the purpose of dialyzer for other parts of FMKe, a minimal set of dialyzer
%% warnings were disabled only for this module.
-dialyzer([no_match, no_unused, no_return]).

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

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, State) ->
    Reply = call(Op),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, State}.

call({update, prescription, Id, Action}) ->
    %% facility, patient, pharmacy, staff
    PKey = gen_key(prescription, Id),
    Pid = fmke_db_conn_manager:checkout(),
    {ok, ListFields} = eredis:q(Pid, ["HGETALL", PKey]),
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
                            {ok, <<"OK">>} = eredis:q(Pid, gen_update_op(prescription, [
                                P#prescription.id,
                                P#prescription.patient_id,
                                P#prescription.prescriber_id,
                                P#prescription.pharmacy_id,
                                P#prescription.date_prescribed,
                                P#prescription.drugs,
                                Date
                            ])),
                            ok;
                        {drugs, add, Drugs} ->
                            NewDrugs = P#prescription.drugs ++ Drugs,
                            eredis:q(Pid, gen_update_op(prescription, [
                                P#prescription.id,
                                P#prescription.patient_id,
                                P#prescription.prescriber_id,
                                P#prescription.pharmacy_id,
                                P#prescription.date_prescribed,
                                NewDrugs
                            ])),
                            ok
                    end
            end
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({update, Entity, [Id | _Fs] = Fields}) ->
    %% facility, patient, pharmacy, staff
    Pid = fmke_db_conn_manager:checkout(),
    {ok, ListFields} = eredis:q(Pid, ["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, no_such_entity(Entity)};
        _ ->
            eredis:q(Pid, gen_update_op(Entity, Fields)),
            ok
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, Entity, Id}) ->
    %% facility, patient, pharmacy, staff
    Pid = fmke_db_conn_manager:checkout(),
    {ok, ListFields} = eredis:q(Pid, ["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, not_found};
        [_H| _T] when Entity == facility; Entity == prescription ->
            parse_fields(Entity, ListFields);
        [_H| _T] when Entity == patient; Entity == pharmacy; Entity == staff ->
            %% these entities have prescriptions associated with them
            {ok, Prescs} = eredis:q(Pid, ["SMEMBERS", gen_prescriptions_key(Entity, Id)]),
            parse_fields(Entity, ListFields ++ [entity_prescriptions_key(Entity), Prescs])
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, Entity, Id, prescriptions}) ->
    %% patient, pharmacy, staff
    Pid = fmke_db_conn_manager:checkout(),
    {ok, ListFields} = eredis:q(Pid, ["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, no_such_entity(Entity)};
        [_H| _T] when Entity == patient; Entity == pharmacy; Entity == staff ->
            {ok, Prescs} = eredis:q(Pid, ["SMEMBERS", gen_prescriptions_key(Entity, Id)]),
            Prescs
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({read, Entity, Id, processed_prescriptions}) ->
    %% patient, pharmacy, staff
    Pid = fmke_db_conn_manager:checkout(),
    {ok, ListFields} = eredis:q(Pid, ["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            {error, no_such_entity(Entity)};
        [_H|_T] when Entity == patient; Entity == pharmacy; Entity == staff ->
            {ok, Prescs} = eredis:q(Pid, ["SMEMBERS", gen_prescriptions_key(Entity, Id)]),
            case Prescs of
                [] -> [];
                [_PrescsH | _PrescsT] ->
                    Queries = lists:map(fun(PrescKey) -> ["HGETALL", binary_to_list(PrescKey)] end, Prescs),
                    QResults = eredis:qp(Pid, Queries),
                    ProcPrescs = lists:filter(fun({ok, PObj}) ->
                                        is_processed(PObj) == true
                                    end, QResults),
                    lists:map(fun({ok, ProcP}) -> parse_fields(prescription, ProcP) end, ProcPrescs)
            end
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({create, prescription, [Id, PatId, DocId, PharmId | _Fs] = Fields}) ->
    %% facility, patient, pharmacy, staff
    Pid = fmke_db_conn_manager:checkout(),
    [{ok, Presc}, {ok, Pat}, {ok, Doc}, {ok, Pharm}] = eredis:qp(Pid, [
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
            [{ok, <<"OK">>}, {ok, <<"1">>}, {ok, <<"1">>}, {ok, <<"1">>}] = eredis:qp(Pid, [
                gen_update_op(prescription, Fields),
                ["SADD", gen_prescriptions_key(patient, PatId), PKey],
                ["SADD", gen_prescriptions_key(pharmacy, PharmId), PKey],
                ["SADD", gen_prescriptions_key(staff, DocId), PKey]
            ]),
            ok
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result;

call({create, Entity, [Id | _Fs] = Fields}) ->
    %% facility, patient, pharmacy, staff
    Pid = fmke_db_conn_manager:checkout(),
    {ok, ListFields} = eredis:q(Pid, ["HGETALL", gen_key(Entity, Id)]),
    Result = case ListFields of
        [] ->
            eredis:q(Pid, gen_update_op(Entity, Fields)),
            ok;
        _ ->
            {error, id_taken(Entity)}
    end,
    fmke_db_conn_manager:checkin(Pid),
    Result.

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

gen_key(Entity, Id) ->
    atom_to_list(Entity) ++ "_" ++ bin_or_int_to_list(Id).

gen_prescriptions_key(Entity, Id) ->
    atom_to_list(Entity) ++ "_" ++ bin_or_int_to_list(Id) ++ "_prescriptions".

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

is_processed(PObj) ->
    search_field(PObj, ?PRESCRIPTION_IS_PROCESSED_KEY) == ?PRESCRIPTION_PROCESSED_VALUE.

search_field([], _Key) ->
    {error, no_such_field};
search_field([Key, Value | _T], Key) ->
    Value;
search_field([_K, _V | T], Key) ->
    search_field(T, Key).
