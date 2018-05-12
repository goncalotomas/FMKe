%% TODO UPDATE PRESCRIPTION MEDICATION
%% TODO PROCESS PRESCRIPTION
-module(fmke_kv_adapter).

-behaviour(gen_fmke_adapter).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

%% gen_fmke_adapter callbacks
-export([
    start/1,
    stop/0,
    create_patient/3,
    create_pharmacy/3,
    create_facility/4,
    create_staff/4,
    create_prescription/6,
    get_facility_by_id/1,
    get_patient_by_id/1,
    get_pharmacy_by_id/1,
    get_processed_pharmacy_prescriptions/1,
    get_pharmacy_prescriptions/1,
    get_prescription_by_id/1,
    get_prescription_medication/1,
    get_staff_by_id/1,
    get_staff_prescriptions/1,
    process_prescription/2,
    update_patient_details/3,
    update_pharmacy_details/3,
    update_facility_details/4,
    update_staff_details/4,
    update_prescription_medication/3
  ]).

start(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).

init([Driver, DataModel]) ->
    lager:info("~p will use the ~p module (~p data model)~n", [?MODULE, Driver, DataModel]),
    {ok, {Driver, DataModel}}.

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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId | _T] = Fields},
        _From, {Driver, DataModel}) ->
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
                {PatKey, patient, Patient#patient{prescriptions = [PrescKey | Patient#patient.prescriptions]}},
                {StaffKey, staff, Staff#staff{prescriptions = [PrescKey | Staff#staff.prescriptions]}},
                {PharmKey, pharmacy, Pharmacy#pharmacy{prescriptions = [PrescKey | Pharmacy#pharmacy.prescriptions]}}
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

    Reply = case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Reply, {Driver, DataModel}};

handle_call({create, Entity, [Id | _T] = Fields}, _From, {Driver, DataModel}) ->
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

    Result = case Driver:commit_transaction(Context4, []) of
        ok -> Reply;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, {Driver, DataModel}};

handle_call({update, prescription, Id, Action}, _From, {Driver, DataModel}) ->
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
                    % io:format("fmke_kv_adapter: P= ~p", [P]),
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
                                                    _ -> Pscp
                                                end end, Pharm#pharmacy.prescriptions)},
                    NStaff = Staff#staff{prescriptions =
                                            lists:map(
                                                fun(Pscp) -> case Pscp#prescription.id of
                                                    % Id -> io:format("WROTE IT!"),P;
                                                    Id -> P;
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

    Result = case Driver:commit_transaction(Context5, []) of
        ok -> Reply;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, {Driver, DataModel}};

handle_call({update, Entity, [Id | _T] = Fields}, _From, {Driver, DataModel}) ->
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

    Result = case Driver:commit_transaction(Context4, []) of
        ok -> Reply;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, {Driver, DataModel}};

handle_call({read, Entity, Id}, _From, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[RResult], Context2} = Driver:get([{gen_key(Entity, Id), Entity}], Context),
    Result = case Driver:commit_transaction(Context2, []) of
        ok -> RResult;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, {Driver, DataModel}};

handle_call({read, staff, Id, [prescriptions]}, _From, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[ReadResult], Context2} = Driver:get([{gen_key(staff, Id), staff}], Context),
    {Result, Context4} = case ReadResult of
        {error, not_found} ->
            {{error, no_such_staff}, Context2};
        Rec when is_record(Rec, staff) ->
            case DataModel of
                nested ->
                    %% within the staff record is a list of prescription objects
                    {Rec#staff.prescriptions, Context2};
                non_nested ->
                    %% within the staff record is a list of keys
                    Entries = lists:map(fun(P) -> {P, prescription} end, Rec#staff.prescriptions),
                    Driver:get(Entries, Context2)
            end
    end,
    Reply = case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Reply, {Driver, DataModel}};

handle_call({read, pharmacy, Id, [prescriptions]}, _From, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[ReadResult], Context2} = Driver:get([{gen_key(pharmacy, Id), pharmacy}], Context),
    {Result, Context4} = case ReadResult of
        {error, not_found} ->
            {{error, no_such_pharmacy}, Context2};
        Rec when is_record(Rec, pharmacy) ->
            case DataModel of
                nested ->
                    %% within the pharmacy record is a list of prescription objects
                    {Rec#pharmacy.prescriptions, Context2};
                non_nested ->
                    %% within the pharmacy record is a list of keys
                    Entries = lists:map(fun(P) -> {P, prescription} end, Rec#pharmacy.prescriptions),
                    Driver:get(Entries, Context2)
            end
    end,
    Reply = case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Reply, {Driver, DataModel}};

handle_call({read, pharmacy, Id, [prescriptions], FilterFun}, _From, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[ReadResult], Context2} = Driver:get([{gen_key(pharmacy, Id), pharmacy}], Context),
    {Result, Context4} = case ReadResult of
        {error, not_found} ->
            {{error, no_such_pharmacy}, Context2};
        Rec when is_record(Rec, pharmacy) ->
            case DataModel of
                nested ->
                    %% within the pharmacy record is a list of prescription objects
                    {lists:filter(FilterFun, Rec#pharmacy.prescriptions), Context2};
                non_nested ->
                    %% within the pharmacy record is a list of keys
                    Entries = lists:map(fun(P) -> {P, prescription} end, Rec#pharmacy.prescriptions),
                    {Prescs, Context3} = Driver:get(Entries, Context2),
                    {lists:filter(FilterFun, Prescs), Context3}
            end
    end,
    Reply = case Driver:commit_transaction(Context4, []) of
        ok -> Result;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Reply, {Driver, DataModel}};

handle_call({read, prescription, Id, [drugs]}, _From, {Driver, DataModel}) ->
    {ok, Context} = Driver:start_transaction([]),
    {[RResult], Context2} = Driver:get([{gen_key(prescription, Id), prescription}], Context),
    Result = case Driver:commit_transaction(Context2, []) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case RResult of
                {error, not_found} ->
                    {error, no_such_prescription};
                Rec when is_record(Rec, prescription) ->
                    Rec#prescription.drugs
            end
    end,
    {reply, Result, {Driver, DataModel}}.

chg_presc(P, {drugs, add, Drugs}) ->
    P#prescription{drugs = P#prescription.drugs ++ Drugs};
chg_presc(P, {date_processed, DateProcessed}) ->
    P#prescription{is_processed = ?PRESCRIPTION_PROCESSED_VALUE, date_processed = DateProcessed}.

prsc_wrt_res([]) -> ok;
prsc_wrt_res([H|T]) when H == ok -> prsc_wrt_res(T);
prsc_wrt_res([H|_T]) when H =/= ok -> {error, write_failed, H}.

%% after a get request returns multiple results, checks if every K in Keys1 is {error, not_found}, and every K in Keys2
%% returns a valid application record
-spec check_keys(Keys1::list(binary()), Keys2::list(binary())) -> true | {false, atom()} | {false, atom(), binary()}.
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
