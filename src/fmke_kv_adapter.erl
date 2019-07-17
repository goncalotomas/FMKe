-module(fmke_kv_adapter).

-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server callbacks
-export([
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-type get_result() :: app_record() | {error, not_found}.
-type db_entry() :: {key(), get_result()}.
-type key_or_entry() :: key() | db_entry().

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([Driver, DataModel]) ->
    lager:info("booted ~p process, using the ~p module (~p data model)~n", [?MODULE, Driver, DataModel]),
    {ok, {Driver, DataModel}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, State) ->
    Reply = call(Op, State),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, State}.

call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId | _T] = Fields}, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    PrescKey = gen_key(prescription, Id),
    PatKey = gen_key(patient, PatientId),
    StaffKey = gen_key(staff, PrescriberId),
    PharmKey = gen_key(pharmacy, PharmacyId),
    {[P | [Patient, Staff, Pharmacy] = Ks], Context2} = Driver:get([
        {PrescKey, prescription}, {PatKey, patient}, {StaffKey, staff}, {PharmKey, pharmacy}
    ], Context),

    ChecksOk = check_keys([{PrescKey, P}], lists:zip([PatKey, StaffKey, PharmKey], Ks)),
    PrscObj = mk_app_rec(prescription, Fields),
    {Result, Context4} = case {ChecksOk, DataModel} of
        {true, nested} ->
            {WResult, Context3} = Driver:put([
                {PrescKey, prescription, PrscObj},
                {PatKey, patient, Patient#patient{prescriptions = [PrscObj | Patient#patient.prescriptions]}},
                {StaffKey, staff, Staff#staff{prescriptions = [PrscObj | Staff#staff.prescriptions]}},
                {PharmKey, pharmacy, Pharmacy#pharmacy{prescriptions = [PrscObj | Pharmacy#pharmacy.prescriptions]}}
            ], Context2),
            {prsc_wrt_res(WResult), Context3};
        {true, non_nested} ->
            {WResult, Context3} = Driver:put([
                {PrescKey, prescription, PrscObj},
                {<<PatKey/binary, "_prescriptions">>, prescription_ref, PrescKey},
                {<<StaffKey/binary, "_prescriptions">>, prescription_ref, PrescKey},
                {<<PharmKey/binary, "_prescriptions">>, prescription_ref, PrescKey}
            ], Context2),
            {prsc_wrt_res(WResult), Context3};
        {{false, key_exists, PrescKey}, _} ->
            {{error, prescription_id_taken}, Context2};
        {{false, missing_key, PatKey}, _} ->
            {{error, no_such_patient}, Context2};
        {{false, missing_key, PharmKey}, _} ->
            {{error, no_such_pharmacy}, Context2};
        {{false, missing_key, StaffKey}, _} ->
            {{error, no_such_staff}, Context2};
        {Error, _} ->
            {{error, Error}, Context2}
    end,

    case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end;

call({create, Entity, [Id | _T] = Fields}, {Driver, _DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    Key = gen_key(Entity, Id),
    {[RResult], Context2} = Driver:get([{Key, Entity}], Context),
    {Reply, Context4} = case RResult of
        {error, not_found} ->
            {[WResult], Context3} = Driver:put([{Key, Entity, mk_app_rec(Entity, Fields)}], Context),
            {WResult, Context3};
        _Record when Entity =:= facility ->     {{error, facility_id_taken}, Context2};
        _Record when Entity =:= patient ->      {{error, patient_id_taken}, Context2};
        _Record when Entity =:= pharmacy ->     {{error, pharmacy_id_taken}, Context2};
        _Record when Entity =:= staff ->        {{error, staff_id_taken}, Context2}
    end,

    case Driver:commit_transaction(Context4, []) of
        ok -> Reply;
        {error, Reason} -> {error, Reason}
    end;

call({update, prescription, Id, Action}, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    Key = gen_key(prescription, Id),
    {[RResult], Context2} = Driver:get([{Key, prescription}], Context),
    {Reply, Context5} = case RResult of
        {error, not_found} ->
            {{error, no_such_prescription}, Context2};
        Record when is_record(Record, prescription) ->
            case {Record#prescription.is_processed, DataModel} of
                {?PRESCRIPTION_PROCESSED_VALUE, _} ->
                    {{error, prescription_already_processed}, Context2};
                {?PRESCRIPTION_NOT_PROCESSED_VALUE, non_nested} ->
                    {[WResult], Context3} = Driver:put([
                            {Key, prescription, chg_presc(Record, Action)}
                        ], Context2),
                    {WResult, Context3};
                {?PRESCRIPTION_NOT_PROCESSED_VALUE, nested} ->
                    P = chg_presc(Record, Action),
                    PatKey = gen_key(patient, P#prescription.patient_id),
                    PharmKey = gen_key(pharmacy, P#prescription.pharmacy_id),
                    StaffKey = gen_key(staff, P#prescription.prescriber_id),
                    {[Pat, Pharm, Staff], Context3} = Driver:get([
                            {PatKey, patient},
                            {PharmKey, pharmacy},
                            {StaffKey, staff}
                        ], Context2),
                    BinId = list_to_binary(integer_to_list(Id)),
                    NPat = Pat#patient{prescriptions =
                                            lists:map(
                                                fun(Pscp) -> case Pscp#prescription.id of
                                                    Id -> P;
                                                    BinId -> P;
                                                    _ -> Pscp
                                                end end, Pat#patient.prescriptions)},
                    NPharm = Pharm#pharmacy{prescriptions =
                                            lists:map(
                                                fun(Pscp) -> case Pscp#prescription.id of
                                                    Id -> P;
                                                    BinId -> P;
                                                    _ -> Pscp
                                                end end, Pharm#pharmacy.prescriptions)},
                    NStaff = Staff#staff{prescriptions =
                                            lists:map(
                                                fun(Pscp) -> case Pscp#prescription.id of
                                                    Id -> P;
                                                    BinId -> P;
                                                    _ -> Pscp
                                                end end, Staff#staff.prescriptions)},
                    {WResult, Context4} = Driver:put([
                            {Key, prescription, P},
                            {PatKey, patient, NPat},
                            {PharmKey, pharmacy, NPharm},
                            {StaffKey, staff, NStaff}
                        ], Context3),
                    {prsc_wrt_res(WResult), Context4}
            end
    end,

    case Driver:commit_transaction(Context5, []) of
        ok -> Reply;
        {error, Reason} -> {error, Reason}
    end;

call({update, Entity, [Id | _T] = Fields}, {Driver, _DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    Key = gen_key(Entity, Id),
    {[RResult], Context2} = Driver:get([{Key, Entity}], Context),
    {Reply, Context4} = case RResult of
        {error, not_found} when Entity =:= facility ->      {{error, no_such_facility}, Context2};
        {error, not_found} when Entity =:= patient ->       {{error, no_such_patient}, Context2};
        {error, not_found} when Entity =:= pharmacy ->      {{error, no_such_pharmacy}, Context2};
        {error, not_found} when Entity =:= staff ->         {{error, no_such_staff}, Context2};
        _Record ->
            {[WResult], Context3} = Driver:put([{Key, Entity, mk_app_rec(Entity, Fields)}], Context),
            {WResult, Context3}
    end,

    case Driver:commit_transaction(Context4, []) of
        ok -> Reply;
        {error, Reason} -> {error, Reason}
    end;

call({read, Entity, Id}, {Driver, _DataModel}) when Entity =:= patient; Entity =:= pharmacy; Entity =:= staff ->
    {ok, Context} = Driver:start_transaction([]),
    Key = gen_key(Entity, Id),
    {[RResult, Prescs], Context2} = Driver:get([{Key, Entity},
                                        {<<Key/binary, "_prescriptions">>, prescription_ref}], Context),
    Result = case RResult of
        {error, not_found} -> {error, not_found};
        X when is_record(X, patient), is_list(Prescs) -> X#patient{prescriptions = Prescs};
        X when is_record(X, pharmacy), is_list(Prescs) -> X#pharmacy{prescriptions = Prescs};
        X when is_record(X, staff), is_list(Prescs) -> X#staff{prescriptions = Prescs};
        BaseRecord -> BaseRecord
    end,
    case Driver:commit_transaction(Context2, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end;

call({read, Entity, Id}, {Driver, _DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[RResult], Context2} = Driver:get([{gen_key(Entity, Id), Entity}], Context),
    case Driver:commit_transaction(Context2, []) of
        ok -> RResult;
        {error, Reason} -> {error, Reason}
    end;


call({read, Entity, Id, prescriptions}, {Driver, nested}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[ReadResult], Context2} = Driver:get([{gen_key(Entity, Id), Entity}], Context),
    {Result, Context4} = case ReadResult of
        {error, not_found} ->
            {{error, no_such_entity(Entity)}, Context2};
        Rec when is_record(Rec, patient) ->
            {Rec#patient.prescriptions, Context2};
        Rec when is_record(Rec, pharmacy) ->
            {Rec#pharmacy.prescriptions, Context2};
        Rec when is_record(Rec, staff) ->
            {Rec#staff.prescriptions, Context2}
    end,
    case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end;
call({read, Entity, Id, prescriptions}, {Driver, non_nested}) ->
    {ok, Context} = Driver:start_transaction([]),
    Key = gen_key(Entity, Id),
    {[EntityObj, Prescriptions], Context2} = Driver:get([{Key, Entity},
                                            {<<Key/binary, "_prescriptions">>, prescription_ref}], Context),
    {Result, Context4} = case EntityObj of
        {error, not_found} ->
            {{error, no_such_entity(Entity)}, Context2};
        _List when is_list(Prescriptions)->
            {Prescriptions, Context2};
        _List ->
            {[], Context2}
    end,
    case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end;

call({read, Entity, Id, processed_prescriptions}, {Driver, nested}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[ReadResult], Context2} = Driver:get([{gen_key(Entity, Id), Entity}], Context),
    FilterFun = fun(P) -> P#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE end,
    {Result, Context4} = case ReadResult of
        {error, not_found} ->
            {{error, no_such_pharmacy}, Context2};
        Rec when is_record(Rec, pharmacy) ->
            {lists:filter(FilterFun, Rec#pharmacy.prescriptions), Context2}
    end,
    case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end;
call({read, Entity, Id, processed_prescriptions}, {Driver, non_nested}) ->
    {ok, Context} = Driver:start_transaction([]),
    Key = gen_key(Entity, Id),
    {[EntityObj, Prescriptions], Context2} = Driver:get([{gen_key(Entity, Id), Entity},
                                            {<<Key/binary, "_prescriptions">>, prescription_ref}], Context),
    FilterFun = fun(P) -> P#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE end,
    {Result, Context4} = case EntityObj of
        {error, not_found} ->
            {{error, no_such_entity(Entity)}, Context2};
        _List when is_list(Prescriptions) ->
            {Prescs, Context5} = Driver:get(lists:map(fun(K) -> {K, prescription} end, Prescriptions), Context),
            {lists:filter(FilterFun, Prescs), Context5};
        _List ->
            {[], Context2}
    end,
    case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end;

call({read, prescription, Id, [drugs]}, {Driver, _DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[RResult], Context2} = Driver:get([{gen_key(prescription, Id), prescription}], Context),
    case Driver:commit_transaction(Context2, []) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case RResult of
                {error, not_found} ->
                    {error, no_such_prescription};
                Rec when is_record(Rec, prescription) ->
                    Rec#prescription.drugs
            end
    end.

chg_presc(P, {drugs, add, Drugs}) ->
    P#prescription{drugs = P#prescription.drugs ++ Drugs};
chg_presc(P, {date_processed, DateProcessed}) ->
    P#prescription{is_processed = ?PRESCRIPTION_PROCESSED_VALUE, date_processed = DateProcessed}.

prsc_wrt_res([]) -> ok;
prsc_wrt_res([H|T]) when H == ok -> prsc_wrt_res(T);
prsc_wrt_res([H|_T]) when H =/= ok -> {error, write_failed, H}.

%% after a get request returns multiple results, checks if every K in Keys1 is {error, not_found}, and every K in Keys2
%% returns a valid application record
-spec check_keys(Keys1::list(key_or_entry()), Keys2::list(key_or_entry())) ->
    true | {false, atom()} | {false, atom(), binary()}.
check_keys([], []) ->
    true;
check_keys([{K, H} | _T], _Keys2) when is_record(H, facility); is_record(H, patient); is_record(H, pharmacy);
                                       is_record(H, prescription); is_record(H, staff) ->
    {false, key_exists, K};
check_keys([{_K, H} | T], Keys2) when H == {error, not_found} ->
    check_keys(T, Keys2);
check_keys(_Keys1, [{K, H} | _T]) when H == {error, not_found} ->
    {false, missing_key, K};
check_keys(Keys1, [{_K, H} | T]) when is_record(H, facility); is_record(H, patient); is_record(H, pharmacy);
                                      is_record(H, prescription); is_record(H, staff) ->
    check_keys(Keys1, T).

mk_app_rec(facility, [Id, Name, Address, Type]) ->
    #facility{id = Id, name = Name, address = Address, type = Type};
mk_app_rec(patient, [Id, Name, Address]) ->
    #patient{id = Id, name = Name, address = Address};
mk_app_rec(pharmacy, [Id, Name, Address]) ->
    #pharmacy{id = Id, name = Name, address = Address};
mk_app_rec(prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]) ->
    #prescription{
        id = Id,
        patient_id = PatientId,
        prescriber_id = PrescriberId,
        pharmacy_id = PharmacyId,
        date_prescribed = DatePrescribed,
        drugs = Drugs
    };
mk_app_rec(staff, [Id, Name, Address, Speciality]) ->
    #staff{id = Id, name = Name, address = Address, speciality = Speciality}.

gen_key(facility, Id) ->        binary_str_w_int("facility_", Id);
gen_key(patient, Id) ->         binary_str_w_int("patient_", Id);
gen_key(pharmacy, Id) ->        binary_str_w_int("pharmacy_", Id);
gen_key(prescription, Id) ->    binary_str_w_int("prescription_", Id);
gen_key(staff, Id) ->           binary_str_w_int("staff_", Id).

binary_str_w_int(Str, Int) when is_binary(Int) ->
    list_to_binary(unicode:characters_to_list([Str, binary_to_list(Int)]));
binary_str_w_int(Str, Int) when is_integer(Int) ->
    list_to_binary(unicode:characters_to_list([Str, integer_to_list(Int)])).

no_such_entity(facility) ->     no_such_facility;
no_such_entity(patient) ->      no_such_patient;
no_such_entity(pharmacy) ->     no_such_pharmacy;
no_such_entity(prescription) -> no_such_prescription;
no_such_entity(staff) ->        no_such_staff.
