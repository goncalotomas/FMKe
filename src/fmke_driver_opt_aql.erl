%% --------------------------------------------------------------
%% Database driver for AQL, an SQL-like interface for AntidoteDB.
%%
%% For more information about AQL, check its repository here:
%% https://github.com/mrshankly/secure-aql
%%
%% For more information about the AQL's client being used, see
%% aqlc's repository here: https://github.com/mrshankly/aqlc
%% --------------------------------------------------------------

-module(fmke_driver_opt_aql).

-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

-export([
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    {ok, _Started} = application:ensure_all_started(aqlc),
    {ok, {1}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, {EntityID}) ->
    {Reply, NewEntityID} = call(Op, EntityID),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, {NewEntityID}}.

call({read, patient, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_patient(Conn, Tx, Id) of
        {ok, {Id, Name, Address}} ->
            Prescriptions = case read_pat_presc_ids(Conn, Tx, Id) of
                {ok, P} when is_list(P) ->
                    P;
                _Error ->
                    []
            end,
            {#patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, pharmacy, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_pharmacy(Conn, Tx, Id) of
        {ok, {Id, Name, Address}} ->
            Prescriptions = case read_pharm_presc_ids(Conn, Tx, Id) of
                {ok, P} when is_list(P) ->
                    P;
                _Error ->
                    []
            end,
            {#pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, facility, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_facility(Conn, Tx, Id) of
        {ok, {Id, Name, Address, Type}} ->
            {#facility{id = Id, name = Name, address = Address, type = Type}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, staff, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_staff(Conn, Tx, Id) of
        {ok, {Id, Name, Address, Speciality}} ->
            Prescriptions = case read_staff_presc_ids(Conn, Tx, Id) of
                {ok, P} when is_list(P) ->
                    P;
                _Error ->
                    []
            end,
            {
                #staff{
                    id = Id,
                    name = Name,
                    address = Address,
                    speciality = Speciality,
                    prescriptions = Prescriptions
                },
                EntityID
            };
        Error ->
            {Error, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({create, facility, [Id, Name, Address, Type]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_facility(Conn, Tx, Id) of
        {error, not_found} ->
            {create_facility(Conn, Tx, Id, Name, Address, Type), EntityID};
        {ok, _Facility} ->
            {{error, id_taken(facility)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({update, facility, [Id, Name, Address, Type]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case update_facility(Conn, Tx, Id, Name, Address, Type) of
        {error, not_found} ->
            {{error, no_such_entity(facility)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID};
        ok ->
            {ok, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({create, patient, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_patient(Conn, Tx, Id) of
        {error, not_found} ->
            {create_patient(Conn, Tx, Id, Name, Address), EntityID};
        {ok, _Patient} ->
            {{error, id_taken(patient)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({update, patient, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case update_patient(Conn, Tx, Id, Name, Address) of
        {error, not_found} ->
            {{error, no_such_entity(patient)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID};
        ok ->
            {ok, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({create, pharmacy, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_pharmacy(Conn, Tx, Id) of
        {error, not_found} ->
            {create_pharmacy(Conn, Tx, Id, Name, Address), EntityID};
        {ok, _Pharmacy} ->
            {{error, id_taken(pharmacy)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({update, pharmacy, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case update_pharmacy(Conn, Tx, Id, Name, Address) of
        {error, not_found} ->
            {{error, no_such_entity(pharmacy)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID};
        ok ->
            {ok, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({create, staff, [Id, Name, Address, Speciality]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_staff(Conn, Tx, Id) of
        {error, not_found} ->
            {create_staff(Conn, Tx, Id, Name, Address, Speciality), EntityID};
        {ok, _Staff} ->
            {{error, id_taken(staff)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({update, staff, [Id, Name, Address, Speciality]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case update_staff(Conn, Tx, Id, Name, Address, Speciality) of
        {error, not_found} ->
            {{error, no_such_entity(staff)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID};
        ok ->
            {ok, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({read, staff, Id, prescriptions}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_staff(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, no_such_entity(staff)}, EntityID};
        {ok, _Staff} ->
            case read_staff_presc_ids(Conn, Tx, Id) of
                {ok, P} when is_list(P) ->
                    Prescriptions = lists:map(fun(PrescriptionId) ->
                        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} =
                            read_prescription(Conn, Tx, PrescriptionId),
                        {ok, Drugs} = read_presc_drugs(Conn, Tx, PrescriptionId),
                        make_prescription(PrescriptionId, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs)
                    end, P),
                    {Prescriptions, EntityID};
                _Error ->
                    {[], EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, pharmacy, Id, prescriptions}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_pharmacy(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, no_such_entity(pharmacy)}, EntityID};
        {ok, _Pharmacy} ->
            case read_pharm_presc_ids(Conn, Tx, Id) of
                {ok, P} when is_list(P) ->
                    Prescriptions = lists:map(fun(PrescriptionId) ->
                        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} =
                            read_prescription(Conn, Tx, PrescriptionId),
                        {ok, Drugs} = read_presc_drugs(Conn, Tx, PrescriptionId),
                        make_prescription(PrescriptionId, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs)
                    end, P),
                    {Prescriptions, EntityID};
                _Error ->
                    {[], EntityID}
            end
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, pharmacy, Id, processed_prescriptions}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_pharmacy(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, no_such_entity(pharmacy)}, EntityID};
        {ok, _Pharmacy} ->
            case read_pharm_presc_ids(Conn, Tx, Id) of
                {ok, P} when is_list(P) ->
                    AllPrescriptions = lists:map(fun(PrescriptionId) ->
                        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} =
                            read_prescription(Conn, Tx, PrescriptionId),
                        {ok, Drugs} = read_presc_drugs(Conn, Tx, PrescriptionId),
                        make_prescription(PrescriptionId, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs)
                    end, P),
                    ProcessedPrescriptions = lists:filter(fun(Presc) ->
                        Presc#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE
                    end, AllPrescriptions),
                    {ProcessedPrescriptions, EntityID};
                _Error ->
                    {[], EntityID}
            end
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, prescription, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_prescription(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, not_found}, EntityID};
        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} ->
            case read_presc_drugs(Conn, Tx, Id) of
                {ok, Drugs} when is_list(Drugs) ->
                    {make_prescription(Id, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs), EntityID};
                {error, Reason} ->
                    {{error, Reason}, EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({read, prescription, Id, [drugs]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_prescription(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, _Prescription} ->
            case read_presc_drugs(Conn, Tx, Id) of
                {ok, Drugs} when is_list(Drugs) ->
                    {Drugs, EntityID};
                {error, Reason} ->
                    {{error, Reason}, EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    commit_and_clean(Conn, Tx),
    Result;

call({update, prescription, Id, {date_processed, NewDateProcessed}}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_prescription(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} ->
            Prescription = make_prescription(Id, PatID, DocID, PharmID, DatePrescribed, DateProcessed, []),
            case Prescription#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {{error, prescription_already_processed}, EntityID};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    {process_prescription(Conn, Tx, Id, NewDateProcessed), EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({update, prescription, Id, {drugs, add, Drugs}}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    Result = case read_prescription(Conn, Tx, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} ->
            Prescription = make_prescription(Id, PatID, DocID, PharmID, DatePrescribed, DateProcessed, []),
            case Prescription#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {{error, prescription_already_processed}, EntityID};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    NewEntityID = lists:foldl(fun(Drug, NewID) ->
                        create_presc_drugs(Conn, Tx, NewID, Id, Drug),
                        NewID + 1
                    end, EntityID, Drugs),
                    {ok, NewEntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    {ok, Tx} = aqlc:start_transaction(Conn),
    CheckResult = check_keys(
        Conn, Tx,
        [
            {prescription, fun read_prescription/3, Id}
        ],
        [
            {patient, fun read_patient/3, PatientId},
            {pharmacy, fun read_pharmacy/3, PharmacyId},
            {staff, fun read_staff/3, PrescriberId}
        ]
    ),

    Result = case CheckResult of
        ok ->
            create_prescription(Conn, Tx, Id, PatientId, PrescriberId, PharmacyId, DatePrescribed),

            create_pat_presc(Conn, Tx, EntityID, PatientId, Id),
            create_pharm_presc(Conn, Tx, EntityID + 1, PharmacyId, Id),
            create_staff_presc(Conn, Tx, EntityID + 2, PrescriberId, Id),

            NewEntityID = lists:foldl(fun(Drug, NewID) ->
                create_presc_drugs(Conn, Tx, NewID, Id, Drug),
                NewID + 1
            end, EntityID + 3, Drugs),

            {ok, NewEntityID};
        {exists, Entity} ->
            {{error, id_taken(Entity)}, EntityID};
        {missing, Entity} ->
            {{error, no_such_entity(Entity)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    ok = commit_and_clean(Conn, Tx),
    Result;

call(_, _) ->
    throw(not_implemented).

%% Queries

read_patient(Conn, Transaction, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address from FmkePatients where ID = ~B;",
        [ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Patient]]} when is_list(Patient) ->
            {ok, {ID, proplists:get_value('Name', Patient), proplists:get_value('Address', Patient)}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_pat_presc_ids(Conn, Transaction, PatientID) ->
    Query = io_lib:format(
        "select PrescriptionID from FmkePatientPrescriptions where PatientID = ~B;",
        [PatientID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Prescriptions]} when is_list(Prescriptions) ->
            P = lists:map(fun(Prescription) -> proplists:get_value('PrescriptionID', Prescription) end, Prescriptions),
            {ok, P};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_pharmacy(Conn, Transaction, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address from FmkePharmacies where ID = ~B;",
        [ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Pharmacy]]} when is_list(Pharmacy) ->
            {ok, {ID, proplists:get_value('Name', Pharmacy), proplists:get_value('Address', Pharmacy)}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_pharm_presc_ids(Conn, Transaction, PharmacyID) ->
    Query = io_lib:format(
        "select PrescriptionID from FmkePharmacyPrescriptions where PharmacyID = ~B",
        [PharmacyID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Prescriptions]} when is_list(Prescriptions) ->
            P = lists:map(fun(Prescription) -> proplists:get_value('PrescriptionID', Prescription) end, Prescriptions),
            {ok, P};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_facility(Conn, Transaction, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address,Type from FmkeTreatmentFacilities where ID = ~B",
        [ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Facility]]} when is_list(Facility) ->
            Name = proplists:get_value('Name', Facility),
            Address = proplists:get_value('Address', Facility),
            Type = proplists:get_value('Type', Facility),
            {ok, {ID, Name, Address, Type}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_prescription(Conn, Transaction, ID) ->
    Query = io_lib:format(
        "select ID,PatID,DocID,PharmID,DatePrescribed,DateProcessed from FmkePrescriptions where ID = ~B",
        [ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Prescription]]} when is_list(Prescription) ->
            PatID = proplists:get_value('PatID', Prescription),
            DocID = proplists:get_value('DocID', Prescription),
            PharmID = proplists:get_value('PharmID', Prescription),
            DatePrescribed = proplists:get_value('DatePrescribed', Prescription),
            DateProcessed = proplists:get_value('DateProcessed', Prescription),
            {ok, {ID, PatID, DocID, PharmID, DatePrescribed, DateProcessed}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_presc_drugs(Conn, Transaction, PrescriptionID) ->
    Query = io_lib:format(
        "select Drug from FmkePrescriptionDrugs where PrescriptionID = ~B",
        [PrescriptionID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Drugs]} when is_list(Drugs) ->
            D = lists:map(fun(Drug) -> proplists:get_value('Drug', Drug) end, Drugs),
            {ok, D};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_staff(Conn, Transaction, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address,Speciality from FmkeMedicalStaff where ID = ~B",
        [ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Staff]]} when is_list(Staff) ->
            Name = proplists:get_value('Name', Staff),
            Address = proplists:get_value('Address', Staff),
            Speciality = proplists:get_value('Speciality', Staff),
            {ok, {ID, Name, Address, Speciality}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_staff_presc_ids(Conn, Transaction, StaffID) ->
    Query = io_lib:format(
        "select PrescriptionID from FmkeStaffPrescriptions where StaffID = ~B",
        [StaffID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Prescriptions]} when is_list(Prescriptions) ->
            P = lists:map(fun(Prescription) -> proplists:get_value('PrescriptionID', Prescription) end, Prescriptions),
            {ok, P};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

%% Creates

create_facility(Conn, Transaction, ID, Name, Address, Type) ->
    Query = io_lib:format(
        "insert into FmkeTreatmentFacilities (ID,Name,Address,Type) VALUES (~B,'~s','~s','~s')",
        [ID, Name, Address, Type]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_patient(Conn, Transaction, ID, Name, Address) ->
    Query = io_lib:format(
        "insert into FmkePatients (ID,Name,Address) VALUES (~B,'~s','~s')",
        [ID, Name, Address]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_pharmacy(Conn, Transaction, ID, Name, Address) ->
    Query = io_lib:format(
        "insert into FmkePharmacies (ID,Name,Address) VALUES (~B,'~s','~s')",
        [ID, Name, Address]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_staff(Conn, Transaction, ID, Name, Address, Speciality) ->
    Query = io_lib:format(
        "insert into FmkeMedicalStaff (ID,Name,Address,Speciality) VALUES (~B,'~s','~s','~s')",
        [ID,Name,Address,Speciality]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_prescription(Conn, Transaction, ID, PatID, DocID, PharmID, DatePrescribed) ->
    Query = io_lib:format(
        "insert into FmkePrescriptions (ID,PatID,DocID,PharmID,DatePrescribed,DateProcessed) " ++
        "VALUES (~B,~B,~B,~B,'~s','')",
        [ID,PatID,DocID,PharmID,DatePrescribed]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_presc_drugs(Conn, Transaction, ID, PrescriptionID, Drug) ->
    Query = io_lib:format(
        "insert into FmkePrescriptionDrugs (ID, PrescriptionID, Drug) VALUES (~B,~B,'~s')",
        [ID,PrescriptionID,Drug]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_pat_presc(Conn, Transaction, ID, PatientID, PrescriptionID) ->
    Query = io_lib:format(
        "insert into FmkePatientPrescriptions (ID, PatientID, PrescriptionID) VALUES (~B,~B,~B)",
        [ID,PatientID,PrescriptionID]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_pharm_presc(Conn, Transaction, ID, PharmacyID, PrescriptionID) ->
    Query = io_lib:format(
        "insert into FmkePharmacyPrescriptions (ID, PharmacyID, PrescriptionID) VALUES (~B,~B,~B)",
        [ID,PharmacyID,PrescriptionID]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

create_staff_presc(Conn, Transaction, ID, StaffID, PrescriptionID) ->
    Query = io_lib:format(
        "insert into FmkeStaffPrescriptions (ID, StaffID, PrescriptionID) VALUES (~B,~B,~B)",
        [ID,StaffID,PrescriptionID]
    ),
    {ok, ok} = aqlc:query(Conn, Query, Transaction),
    ok.

%% Updates

update_facility(Conn, Transaction, ID, Name, Address, Type) ->
    Query = io_lib:format(
        "update FmkeTreatmentFacilities set Name = '~s' and Address = '~s' and Type = '~s' where ID = ~B",
        [Name, Address, Type, ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [0]} ->
            {error, not_found};
        {ok, [1]} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

update_patient(Conn, Transaction, ID, Name, Address) ->
    Query = io_lib:format(
        "update FmkePatients set Name = '~s' and Address = '~s' where ID = ~B",
        [Name, Address, ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [0]} ->
            {error, not_found};
        {ok, [1]} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

update_pharmacy(Conn, Transaction, ID, Name, Address) ->
    Query = io_lib:format(
        "update FmkePharmacies set Name = '~s' and Address = '~s' where ID = ~B",
        [Name, Address, ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [0]} ->
            {error, not_found};
        {ok, [1]} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

update_staff(Conn, Transaction, ID, Name, Address, Speciality) ->
    Query = io_lib:format(
        "update FmkeMedicalStaff set Name = '~s' and Address = '~s' and Speciality = '~s' where ID = ~B",
        [Name, Address, Speciality, ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [0]} ->
            {error, not_found};
        {ok, [1]} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

process_prescription(Conn, Transaction, ID, DateProcessed) ->
    Query = io_lib:format(
        "update FmkePrescriptions set DateProcessed = '~s' where ID = ~B",
        [DateProcessed, ID]
    ),
    case aqlc:query(Conn, Query, Transaction) of
        {ok, [0]} ->
            {error, not_found};
        {ok, [1]} ->
            ok;
        {error, Reason} ->
            {error, Reason}
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

make_prescription(ID, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs) ->
    #prescription{
        id = ID,
        patient_id = PatID,
        pharmacy_id = PharmID,
        prescriber_id = DocID,
        date_prescribed = DatePrescribed,
        date_processed = case DateProcessed of
            "" ->
                <<"undefined">>;
            Date ->
                Date
        end,
        drugs = Drugs,
        is_processed = case DateProcessed of
            "" ->
                ?PRESCRIPTION_NOT_PROCESSED_VALUE;
            _Other ->
                ?PRESCRIPTION_PROCESSED_VALUE
        end
    }.

check_keys(_, _, [], []) ->
    ok;
check_keys(Conn, Tx, [], [{Entity, Fn, Id} | Rest]) ->
    case Fn(Conn, Tx, Id) of
        {error, not_found} ->
            {missing, Entity};
        {ok, _} ->
            check_keys(Conn, Tx, [], Rest);
        {error, Reason} ->
            {error, Reason}
    end;
check_keys(Conn, Tx, [{Entity, Fn, Id} | Rest], ShouldExist) ->
    case Fn(Conn, Tx, Id) of
        {error, not_found} ->
            check_keys(Conn, Tx, Rest, ShouldExist);
        {ok, _} ->
            {exists, Entity};
        {error, Reason} ->
            {error, Reason}
    end.

commit_and_clean(Conn, Transaction) ->
    Result = case aqlc:commit_transaction(Conn, Transaction) of
        ok ->
            ok;
        {error, Reason} ->
            lager:warning("AQL transaction failed: ~p~n", [Reason]),
            {error, aborted}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result.
