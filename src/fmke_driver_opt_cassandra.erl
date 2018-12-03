%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for Riak KV, featuring a normalized data model (no CRDT nesting).
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_cassandra).

% -behaviour(fmke_gen_driver).
-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

-include_lib("erlcass/include/erlcass.hrl").

%% gen_server exports
-export ([
    start_link/1
    ,stop/1
    ,init/1
    ,handle_cast/2
    ,handle_call/3
]).

-define(SERVER, ?MODULE).

%% Queries
-define(Q_READ_PAT, <<"select id,name,address from fmke.patients where ID = ? LIMIT 1">>).
-define(Q_READ_PAT_PRESC_IDS, <<"select PrescriptionID from fmke.patient_prescriptions where PatientID = ?">>).
-define(Q_READ_PHARM, <<"select id,name,address from fmke.pharmacies where ID = ? LIMIT 1">>).
-define(Q_READ_PHARM_PRESC_IDS, <<"select PrescriptionID from fmke.pharmacy_prescriptions where PharmacyID = ?">>).
-define(Q_READ_FAC, <<"select id,name,address,type from fmke.treatment_facilities where ID = ? LIMIT 1">>).
-define(Q_READ_PRESC,
        <<"select id,patid,docid,pharmid,dateprescribed,dateprocessed from fmke.prescriptions where ID = ? LIMIT 1">>).
-define(Q_READ_PRESC_DRUGS, <<"select Drug from fmke.prescription_drugs where PrescriptionID = ?">>).
-define(Q_READ_STAFF, <<"select id,name,address,speciality from fmke.medical_staff where ID = ? LIMIT 1">>).
-define(Q_READ_STAFF_PRESC_IDS, <<"select PrescriptionID from fmke.staff_prescriptions where StaffID = ?">>).


%% Statements
-define(S_CREATE_FAC, <<"insert into fmke.treatment_facilities (id,name,address,type) VALUES (?,?,?,?)">>).
-define(S_CREATE_PAT, <<"insert into fmke.patients (id,name,address) VALUES (?,?,?)">>).
-define(S_CREATE_PHARM, <<"insert into fmke.pharmacies (id,name,address) VALUES (?,?,?)">>).
-define(S_CREATE_STAFF, <<"insert into fmke.medical_staff (id,name,address,speciality) VALUES (?,?,?,?)">>).
-define(S_CREATE_PRESC,
        <<"insert into fmke.prescriptions (ID,PatID,DocID,PharmID,DatePrescribed) VALUES (?, ?, ?, ?, ?)">>).
-define(S_CREATE_PRESC_DRUGS, <<"insert into fmke.prescription_drugs (PrescriptionID,Drug) VALUES (?, ?)">>).
-define(S_CREATE_PAT_PRESC, <<"insert into fmke.patient_prescriptions (PatientID,PrescriptionID) VALUES (?, ?)">>).
-define(S_CREATE_PHARM_PRESC, <<"insert into fmke.pharmacy_prescriptions (PharmacyID,PrescriptionID) VALUES (?, ?)">>).
-define(S_CREATE_STAFF_PRESC, <<"insert into fmke.staff_prescriptions (StaffID,PrescriptionID) VALUES (?, ?)">>).
-define(S_UPDATE_FAC, <<"update fmke.treatment_facilities set name = ?, address = ?, type = ? where id = ?">>).
-define(S_UPDATE_PAT, <<"update fmke.patients set name = ?, address = ? where id = ?">>).
-define(S_UPDATE_PHARM, <<"update fmke.pharmacies set name = ?, address = ? where id = ?">>).
-define(S_UPDATE_STAFF, <<"update fmke.medical_staff set name = ?, address = ?, speciality = ? where id = ?">>).
-define(S_PROCESS_PRESC, <<"update fmke.prescriptions set dateprocessed = ? where id = ?">>).

%% column descriptions
-define(FAC_COLUMNS, [{<<"id">>, int}, {<<"name">>, text}, {<<"address">>, text}, {<<"type">>, text}]).
-define(PAT_COLUMNS, [{<<"id">>, int}, {<<"name">>, text}, {<<"address">>, text}]).
-define(PHARM_COLUMNS, [{<<"id">>, int}, {<<"name">>, text}, {<<"address">>, text}]).
-define(STAFF_COLUMNS, [{<<"id">>, int}, {<<"name">>, text}, {<<"address">>, text}, {<<"speciality">>, text}]).
-define(STAFF_PRESCS_COLUMNS, [{<<"prescriptionid">>, int}]).
-define(PRESC_DRUGS_COLUMNS, [{<<"drug">>,text}]).
-define(PRESC_COLUMNS, [{<<"id">>, int}, {<<"patid">>, int}, {<<"docid">>, int},
                        {<<"pharmid">>, int}, {<<"dateprescribed">>, timestamp}, {<<"dateprocessed">>, timestamp}]).


start_link(_) ->
    {ok, Hosts} = application:get_env(?APP, database_addresses),
    {ok, Ports} = application:get_env(?APP, database_ports),
    {ok, PoolSize} = application:get_env(?APP, connection_pool_size),
    gen_server:start_link(?MODULE, [Hosts, Ports, PoolSize], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([Hosts, Ports, PoolSize]) ->
    %% TODO remove load_balance_dc_aware if only using 1 DC
    ClusterOpts = [
        {contact_points, list_to_binary(string:join(Hosts, ","))},
        {port, hd(Ports)}, %% erlcass doesn't seem to support clusters running on different ports
        {latency_aware_routing, true},
        {token_aware_routing, true},
        {number_threads_io, 4},
        {queue_size_io, 128000},
        {max_connections_host, PoolSize},
        {tcp_nodelay, true},
        {tcp_keepalive, {true, 1800}}
    ],
    application:set_env(erlcass, log_level, 3),
    application:set_env(erlcass, keyspace, <<"fmke">>),
    application:set_env(erlcass, cluster_options, ClusterOpts),
    application:ensure_all_started(erlcass),

    ok = try_add_prep_stmt(read_patient, ?Q_READ_PAT),
    ok = try_add_prep_stmt(read_pat_presc_ids, ?Q_READ_PAT_PRESC_IDS),
    ok = try_add_prep_stmt(read_pharmacy, ?Q_READ_PHARM),
    ok = try_add_prep_stmt(read_pharm_presc_ids, ?Q_READ_PHARM_PRESC_IDS),
    ok = try_add_prep_stmt(read_prescription, ?Q_READ_PRESC),
    ok = try_add_prep_stmt(read_presc_drugs, ?Q_READ_PRESC_DRUGS),
    ok = try_add_prep_stmt(read_facility, ?Q_READ_FAC),
    ok = try_add_prep_stmt(read_staff, ?Q_READ_STAFF),
    ok = try_add_prep_stmt(read_staff_presc_ids, ?Q_READ_STAFF_PRESC_IDS),
    ok = try_add_prep_stmt(read_presc_drugs, ?Q_READ_PRESC_DRUGS),
    %% insert
    ok = try_add_prep_stmt(create_facility, ?S_CREATE_FAC),
    ok = try_add_prep_stmt(create_patient, ?S_CREATE_PAT),
    ok = try_add_prep_stmt(create_pharmacy, ?S_CREATE_PHARM),
    ok = try_add_prep_stmt(create_staff, ?S_CREATE_STAFF),
    ok = try_add_prep_stmt(create_prescription, ?S_CREATE_PRESC),
    ok = try_add_prep_stmt(create_presc_drugs, ?S_CREATE_PRESC_DRUGS),
    ok = try_add_prep_stmt(create_pat_presc, ?S_CREATE_PAT_PRESC),
    ok = try_add_prep_stmt(create_pharm_presc, ?S_CREATE_PHARM_PRESC),
    ok = try_add_prep_stmt(create_staff_presc, ?S_CREATE_STAFF_PRESC),
    %% update
    ok = try_add_prep_stmt(update_facility, ?S_UPDATE_FAC),
    ok = try_add_prep_stmt(update_patient, ?S_UPDATE_PAT),
    ok = try_add_prep_stmt(update_pharmacy, ?S_UPDATE_PHARM),
    ok = try_add_prep_stmt(update_staff, ?S_UPDATE_STAFF),
    ok = try_add_prep_stmt(process_prescription, ?S_PROCESS_PRESC),

    {ok, {}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, State) ->
    Reply = call(Op),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, State}.

call({read, patient, Id}) ->
    case parse_result(patient, erlcass:execute(read_patient, [Id])) of
        {error, Reason} ->
            {error, Reason};
        #patient{} = P ->
            %% TODO read prescriptions
            PrescriptionIds = case parse_result(pat_presc_ids, erlcass:execute(read_pat_presc_ids, [Id])) of
                {error, not_found} ->
                    [];
                List ->
                    List
            end,
            P#patient{prescriptions = PrescriptionIds}
    end;

call({read, pharmacy, Id}) ->
    case parse_result(pharmacy, erlcass:execute(read_pharmacy, [Id])) of
        {error, Reason} ->
            {error, Reason};
        #pharmacy{} = P ->
            %% TODO read prescriptions
            PrescriptionIds = case parse_result(pharm_presc_ids, erlcass:execute(read_pharm_presc_ids, [Id])) of
                {error, not_found} ->
                    [];
                List ->
                    List
            end,
            P#pharmacy{prescriptions = PrescriptionIds}
    end;

call({read, facility, Id}) ->
    parse_result(facility, erlcass:execute(read_facility, [Id]));

call({read, staff, Id}) ->
    case parse_result(staff, erlcass:execute(read_staff, [Id])) of
        {error, Reason} ->
            {error, Reason};
        #staff{} = P ->
            %% TODO read prescriptions
            PrescriptionIds = case parse_result(staff_presc_ids, erlcass:execute(read_staff_presc_ids, [Id])) of
                {error, not_found} ->
                    [];
                List ->
                    List
            end,
            P#staff{prescriptions = PrescriptionIds}
    end;

call({create, facility, [Id, Name, Address, Type]}) ->
    case parse_result(facility, erlcass:execute(read_facility, [Id])) of
        {error, _Reason} ->
            erlcass:execute(create_facility, [Id, Name, Address, Type]);
        #facility{} ->
            {error, id_taken(facility)}
    end;

call({update, facility, [Id, Name, Address, Type]}) ->
    case parse_result(facility, erlcass:execute(read_facility, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(facility)};
        #facility{} ->
            erlcass:execute(update_facility, [Name, Address, Type, Id])
    end;

call({create, patient, [Id, Name, Address]}) ->
    case parse_result(patient, erlcass:execute(read_patient, [Id])) of
        {error, _Reason} ->
            erlcass:execute(create_patient, [Id, Name, Address]);
        #patient{} ->
            {error, id_taken(patient)}
    end;

call({update, patient, [Id, Name, Address]}) ->
    case parse_result(patient, erlcass:execute(read_patient, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(patient)};
        #patient{} ->
            erlcass:execute(update_patient, [Name, Address, Id])
    end;

call({create, pharmacy, [Id, Name, Address]}) ->
    case parse_result(pharmacy, erlcass:execute(read_pharmacy, [Id])) of
        {error, _Reason} ->
            erlcass:execute(create_pharmacy, [Id, Name, Address]);
        #pharmacy{} ->
            {error, id_taken(pharmacy)}
    end;

call({update, pharmacy, [Id, Name, Address]}) ->
    case parse_result(pharmacy, erlcass:execute(read_pharmacy, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(pharmacy)};
        #pharmacy{} ->
            erlcass:execute(update_pharmacy, [Name, Address, Id])
    end;

call({create, staff, [Id, Name, Address, Speciality]}) ->
    case parse_result(staff, erlcass:execute(read_staff, [Id])) of
        {error, _Reason} ->
            erlcass:execute(create_staff, [Id, Name, Address, Speciality]);
        #staff{} ->
            {error, id_taken(staff)}
    end;

call({update, staff, [Id, Name, Address, Speciality]}) ->
    case parse_result(staff, erlcass:execute(read_staff, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(staff)};
        #staff{} ->
            erlcass:execute(update_staff, [Name, Address, Speciality, Id])
    end;

call({read, staff, Id, prescriptions}) ->
    case parse_result(staff, erlcass:execute(read_staff, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(staff)};
        #staff{} ->
            case parse_result(staff_presc_ids, erlcass:execute(read_staff_presc_ids, [Id])) of
                {error, not_found} ->
                    %% no prescription tuples were found in the table
                    [];
                [_H|_T] = ListIDs ->
                    lists:map(fun(PrescId) ->
                        Presc = parse_result(prescription, erlcass:execute(read_prescription, [PrescId])),
                        Drugs = parse_result(presc_drugs, erlcass:execute(read_presc_drugs, [PrescId])),
                        Presc#prescription{drugs = Drugs}
                    end, ListIDs)
            end
    end;

call({read, pharmacy, Id, prescriptions}) ->
    case parse_result(pharmacy, erlcass:execute(read_pharmacy, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(pharmacy)};
        #pharmacy{} ->
            case parse_result(pharm_presc_ids, erlcass:execute(read_pharm_presc_ids, [Id])) of
                {error, not_found} ->
                    %% no prescription tuples were found in the table
                    [];
                [_H|_T] = ListIDs ->
                    lists:map(fun(PrescId) ->
                        Presc = parse_result(prescription, erlcass:execute(read_prescription, [PrescId])),
                        Drugs = parse_result(presc_drugs, erlcass:execute(read_presc_drugs, [PrescId])),
                        Presc#prescription{drugs = Drugs}
                    end, ListIDs)
            end
    end;

call({read, pharmacy, Id, processed_prescriptions}) ->
    case parse_result(pharmacy, erlcass:execute(read_pharmacy, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(pharmacy)};
        #pharmacy{} ->
            case parse_result(pharm_presc_ids, erlcass:execute(read_pharm_presc_ids, [Id])) of
                {error, not_found} ->
                    %% no prescription tuples were found in the table
                    [];
                [_H|_T] = ListIDs ->
                    Prescriptions = lists:map(
                        fun(PrescId) ->
                            Presc = parse_result(prescription, erlcass:execute(read_prescription, [PrescId])),
                            Drugs = parse_result(presc_drugs, erlcass:execute(read_presc_drugs, [PrescId])),
                            Presc#prescription{drugs = Drugs}
                        end, ListIDs),
                    lists:filter(fun(Presc) ->
                        Presc#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE
                    end, Prescriptions)
            end
    end;

call({read, prescription, Id}) ->
    case parse_result(prescription, erlcass:execute(read_prescription, [Id])) of
        {error, not_found} ->
            {error, not_found};
        #prescription{} = P ->
            Drugs = case parse_result(presc_drugs, erlcass:execute(read_presc_drugs, [Id])) of
                {error, not_found} ->
                    %% no prescription drugs were found in the table
                    [];
                [_H|_T] = ListDrugs ->
                    ListDrugs
            end,
            P#prescription{drugs = Drugs}
    end;

call({read, prescription, Id, [drugs]}) ->
    case parse_result(prescription, erlcass:execute(read_prescription, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(prescription)};
        #prescription{} ->
            %% record does not contain drugs, we need to fetch them manually
            case parse_result(presc_drugs, erlcass:execute(read_presc_drugs, [Id])) of
                {error, not_found} ->
                    %% no prescription drugs were found in the table
                    [];
                [_H|_T] = Drugs ->
                    Drugs
            end
    end;

call({update, prescription, Id, {date_processed, DateProcessed}}) ->
    case parse_result(prescription, erlcass:execute(read_prescription, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(prescription)};
        #prescription{} = P ->
            case P#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {error, prescription_already_processed};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    Date = erlcass_time:date_from_epoch(calendar:rfc3339_to_system_time(DateProcessed ++ "T00:00:00")),
                    ok = erlcass:execute(process_prescription, [Date, Id])
            end
    end;

call({update, prescription, Id, {drugs, add, Drugs}}) ->
    case parse_result(prescription, erlcass:execute(read_prescription, [Id])) of
        {error, _Reason} ->
            {error, no_such_entity(prescription)};
        #prescription{} = P ->
            case P#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {error, prescription_already_processed};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    lists:map(fun(Drug) ->
                        ok = erlcass:execute(create_presc_drugs, [Id, Drug])
                    end, Drugs),
                    ok
            end
    end;

call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}) ->
    CheckResult = check_keys([{prescription, Id}], [{patient, PatientId},
                              {pharmacy, PharmacyId}, {staff, PrescriberId}]),
    case CheckResult of
        ok ->
            {ok, Stm1} = erlcass:bind_prepared_statement(create_prescription),
            Date = erlcass_time:date_from_epoch(calendar:rfc3339_to_system_time(DatePrescribed ++ "T00:00:00")),
            ok = erlcass:bind_prepared_params_by_index(Stm1, [Id, PatientId, PrescriberId, PharmacyId, Date]),
            %% create patient prescription ref
            {ok, Stm2} = erlcass:bind_prepared_statement(create_pat_presc),
            ok = erlcass:bind_prepared_params_by_index(Stm2, [PatientId, Id]),
            %% create pharmacy prescription ref
            {ok, Stm3} = erlcass:bind_prepared_statement(create_pharm_presc),
            ok = erlcass:bind_prepared_params_by_index(Stm3, [PharmacyId, Id]),
            %% create staff prescription ref
            {ok, Stm4} = erlcass:bind_prepared_statement(create_staff_presc),
            ok = erlcass:bind_prepared_params_by_index(Stm4, [PrescriberId, Id]),
            %% create drugs
            DrugStmts = lists:map(fun(Drug) ->
                {ok, Stmt} = erlcass:bind_prepared_statement(create_presc_drugs),
                ok = erlcass:bind_prepared_params_by_index(Stmt, [Id, Drug]),
                Stmt
            end, Drugs),
            ok = erlcass:batch_execute(?CASS_BATCH_TYPE_LOGGED, [Stm1, Stm2, Stm3, Stm4] ++ DrugStmts, [
                {consistency_level, ?CASS_CONSISTENCY_QUORUM}
            ]);
        {exists, [{prescription, Id}]} ->
            {error, id_taken(prescription)};
        {missing, [{Entity, _EntityId} | _T]} ->
            {error, no_such_entity(Entity)}
    end.

check_keys([], []) -> ok;
check_keys([], ShouldExist) ->
    {Entity, Id} = hd(ShouldExist),
    case parse_result(Entity, erlcass:execute(cass_read_query(Entity), [Id])) of
        {error, not_found} ->
            {missing, [{Entity, Id}]};
        _Found ->
            check_keys([], tl(ShouldExist))
    end;
check_keys(ShouldNotExist, ShouldExist) ->
    {Entity, Id} = hd(ShouldNotExist),
    case parse_result(Entity, erlcass:execute(cass_read_query(Entity), [Id])) of
        {error, not_found} ->
            check_keys(tl(ShouldNotExist), ShouldExist);
        _ ->
            {exists, [{Entity, Id}]}
    end.

cass_read_query(patient) ->                 read_patient;
cass_read_query(pharmacy) ->                read_pharmacy;
cass_read_query(prescription) ->            read_prescription;
cass_read_query(staff) ->                   read_staff.

try_add_prep_stmt(Name, Statement) ->
    case erlcass:add_prepare_statement(Name, Statement) of
        ok -> ok;
        {error,already_exist} -> ok
    end.

no_such_entity(facility) ->     no_such_facility;
no_such_entity(patient) ->      no_such_patient;
no_such_entity(pharmacy) ->     no_such_pharmacy;
no_such_entity(prescription) -> no_such_prescription;
no_such_entity(staff) ->        no_such_staff.

id_taken(facility) ->     facility_id_taken;
id_taken(patient) ->      patient_id_taken;
id_taken(pharmacy) ->     pharmacy_id_taken;
id_taken(prescription) -> prescription_id_taken;
id_taken(staff) ->        staff_id_taken.

parse_presc_id([Id]) -> Id.

parse_result(_Entity, {error, Reason}) -> {error, Reason};
parse_result(_Entity, {ok, _Columns, []}) -> {error, not_found};
parse_result(pat_presc_ids, {ok, ?STAFF_PRESCS_COLUMNS, Ids}) ->
    lists:map(fun parse_presc_id/1, Ids);
parse_result(staff_presc_ids, {ok, ?STAFF_PRESCS_COLUMNS, Ids}) ->
    lists:map(fun parse_presc_id/1, Ids);
parse_result(pharm_presc_ids, {ok, ?STAFF_PRESCS_COLUMNS, Ids}) ->
    lists:map(fun parse_presc_id/1, Ids);
parse_result(presc_drugs, {ok, ?PRESC_DRUGS_COLUMNS, Drugs}) ->
    lists:map(fun parse_presc_id/1, Drugs);
parse_result(facility, {ok, ?FAC_COLUMNS, [[Id, Name, Address, Type]]}) ->
    #facility{id = Id, name = Name, address = Address, type = Type};
parse_result(patient, {ok, ?PAT_COLUMNS, [[Id, Name, Address]]}) ->
    #patient{id = Id, name = Name, address = Address};
parse_result(pharmacy, {ok, ?PHARM_COLUMNS, [[Id, Name, Address]]}) ->
    #pharmacy{id = Id, name = Name, address = Address};
parse_result(staff, {ok, ?STAFF_COLUMNS, [[Id, Name, Address, Speciality]]}) ->
    #staff{id = Id, name = Name, address = Address, speciality = Speciality};
parse_result(prescription, {ok, ?PRESC_COLUMNS, [[Id, PatientId, PrescriberId, PharmacyId,
                                                  DatePrescribed, DateProcessed]]}) ->
    #prescription{
        id = Id,
        patient_id = PatientId,
        pharmacy_id = PharmacyId,
        prescriber_id = PrescriberId,
        date_prescribed = DatePrescribed,
        date_processed = case DateProcessed of
            null ->
                <<"undefined">>;
            Date ->
                Date
        end,
        drugs = [],
        is_processed = case DateProcessed of
            null ->
                ?PRESCRIPTION_NOT_PROCESSED_VALUE;
            _Other ->
                ?PRESCRIPTION_PROCESSED_VALUE
        end
    }.
