%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(fmke_driver_riak_kv).
-include("fmke.hrl").
-include("fmke_kv.hrl").

-behaviour(gen_fmke_kv_driver).

%% gen_server exports
-export ([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%% API
-export([
    %% Setup and teardown functions
    start/1,
    stop/0,

    %% Transactional context functions
    start_transaction/1,
    commit_transaction/2,

    %% Data access functions (with transactional context)
    get/2,
    put/2
]).

-define (SERVER, ?MODULE).
-define (BUCKET_TYPE, <<"maps">>).
-define (FAC_FIELDS, [{?FACILITY_ID_KEY, register}, {?FACILITY_NAME_KEY, register},
                      {?FACILITY_ADDRESS_KEY, register}, {?FACILITY_TYPE_KEY, register}]).
-define (PAT_FIELDS, [{?PATIENT_ID_KEY, register},{?PATIENT_NAME_KEY, register},
                      {?PATIENT_ADDRESS_KEY, register}]).
-define (PHARM_FIELDS, [{?PHARMACY_ID_KEY, register},{?PHARMACY_NAME_KEY, register},
                      {?PHARMACY_ADDRESS_KEY, register}]).
-define (STAFF_FIELDS, [{?STAFF_ID_KEY, register},{?STAFF_NAME_KEY, register}, {?STAFF_ADDRESS_KEY, register},
                    {?STAFF_SPECIALITY_KEY, register}]).
-define (PRESC_FIELDS_W_DEFAULTS, [
    {?PRESCRIPTION_ID_KEY, register, -1}, {?PRESCRIPTION_PATIENT_ID_KEY, register, -1},
    {?PRESCRIPTION_PRESCRIBER_ID_KEY, register, -1}, {?PRESCRIPTION_PHARMACY_ID_KEY, register, -1},
    {?PRESCRIPTION_DATE_PRESCRIBED_KEY, register, <<"undefined">>},
    {?PRESCRIPTION_DATE_PROCESSED_KEY, register, <<"undefined">>},
    {?PRESCRIPTION_DRUGS_KEY, set, []},
    {?PRESCRIPTION_IS_PROCESSED_KEY, register, ?PRESCRIPTION_NOT_PROCESSED_VALUE}
]).

%% -------------------------------------------------------------------
%% Setup and teardown functions
%% -------------------------------------------------------------------
start(DataModel) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, DataModel, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(DataModel) ->
    lager:info("~p starting with ~p data model~n", [?MODULE, DataModel]),
    {ok, DataModel}.

%% -------------------------------------------------------------------
%% Riak transaction related exports (Transactional API)
%% -------------------------------------------------------------------
start_transaction(_Opts) ->
    gen_server:call(?MODULE, start_transaction).

commit_transaction(Pid, _Opts) ->
    gen_server:call(?MODULE, {commit_transaction, Pid}).

%% -------------------------------------------------------------------
%% Data access exports - Interaction with Riak KV
%% -------------------------------------------------------------------
get(Keys, Pid) ->
    gen_server:call(?MODULE, {get, Keys, Pid}).

put(Entries, Pid) ->
    gen_server:call(?MODULE, {put, Entries, Pid}).

%% gen server callbacks, contain driver logic

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(start_transaction, _From, DataModel) ->
    %% Driver needs a Pid to request client operations, fetch Pid from fmke_db_conn_manager and put it in TxnContext
    Pid = fmke_db_conn_manager:checkout(),
    {reply, {ok, Pid}, DataModel};

handle_call({commit_transaction, Pid}, _From, DataModel) ->
    %% Interaction with database is complete for current operation, return pid to connection pool
    fmke_db_conn_manager:checkin(Pid),
    {reply, {ok, []}, DataModel};

handle_call({get, Keys, Pid}, _From, DataModel) ->
    Results = lists:map(
                    fun({Key, Type}) ->
                        Bucket = get_bucket(Type),
                        case riakc_pb_socket:fetch_type(Pid, {?BUCKET_TYPE, Bucket}, Key) of
                            {error, {notfound, map}} ->
                                {error, not_found};
                            {{ok, []}} ->
                                {error, not_found};
                            {ok, Value} ->
                                pack(DataModel, Type, Value);
                            Other ->
                                lager:error("Got unrecognised value: ~p~n", [Other]),
                                {error, unrecognised_value, Other}
                        end
                    end, Keys),
    {reply, {Results, Pid}, DataModel};

handle_call({put, Entries, Pid}, _From, DataModel) ->
    Results = lists:map(
                    fun({Key, Type, Value}) ->
                        Bucket = get_bucket(Type),
                        UnpackedVal = unpack(DataModel, Type, Value),
                        riakc_pb_socket:update_type(Pid, {?BUCKET_TYPE, Bucket}, Key, riakc_map:to_op(UnpackedVal))
                    end, Entries),
    {reply, {Results, Pid}, DataModel}.

%% Parses a value obtained from Riak into a common application record format.
-spec pack(atom(), entity(), riakc_map:map()) -> app_record().
%% TODO in pack patient, see if there are nested prescriptions
pack(_, facility, Fac) ->
    [Id, Name, Address, Type] = lists:map(fun(F) -> riakc_map:fetch(F, Fac) end, ?FAC_FIELDS),
    #facility{id = Id, name = Name, address = Address, type = Type};

pack(DataModel, patient, Pat) ->
    [Id, Name, Address] = lists:map(fun(F) -> riakc_map:fetch(F, Pat) end, ?PAT_FIELDS),
    Prescriptions = fetch_prescs(DataModel, Pat, ?PATIENT_PRESCRIPTIONS_KEY),
    #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions};

pack(DataModel, pharmacy, Pharm) ->
    [Id, Name, Address] = lists:map(fun(F) -> riakc_map:fetch(F, Pharm) end, ?PHARM_FIELDS),
    Prescriptions = fetch_prescs(DataModel, Pharm, ?PHARMACY_PRESCRIPTIONS_KEY),
    #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions};

pack(_, prescription, Presc) ->
    [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, DateProcessed, Drugs, IsProcessed] = lists:map(
            fun({Field, Type, Default}) -> fetch(Presc, Field, Type, Default) end, ?PRESC_FIELDS_W_DEFAULTS),
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

pack(DataModel, staff, Staff) ->
    [Id, Name, Address, Speciality] = lists:map(fun(F) -> riakc_map:fetch(F, Staff) end, ?STAFF_FIELDS),
    Prescriptions = fetch_prescs(DataModel, Staff, ?STAFF_PRESCRIPTIONS_KEY),
    #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions}.

unpack(_, facility, #facility{id = Id, name = Name, address = Address, type = Type}) ->
    riakc_map:update({?FACILITY_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?FACILITY_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?FACILITY_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?FACILITY_TYPE_KEY, register}, update_reg(Type), riakc_map:new()))));

unpack(_, patient, #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
    riakc_map:update({?PATIENT_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PATIENT_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PATIENT_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?PATIENT_PRESCRIPTIONS_KEY, set}, update_set(Prescriptions), riakc_map:new()))));

unpack(nested, pharmacy, #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
    riakc_map:update({?PHARMACY_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PHARMACY_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PHARMACY_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?PHARMACY_PRESCRIPTIONS_KEY, set}, update_set(lists:map(fun(P) -> pack(nested, prescription, P) end, Prescriptions)), riakc_map:new()))));
unpack(non_nested, pharmacy, #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
    riakc_map:update({?PHARMACY_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PHARMACY_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PHARMACY_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?PHARMACY_PRESCRIPTIONS_KEY, set}, update_set(Prescriptions), riakc_map:new()))));

unpack(_, prescription, #prescription{id = Id, patient_id = PatientId, prescriber_id = PrescriberId,
                                        pharmacy_id = PharmacyId, date_prescribed = DatePrescribed,
                                        date_processed = DateProcessed, drugs = Drugs, is_processed = IsProcessed}) ->
    riakc_map:update({?PRESCRIPTION_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PRESCRIPTION_PATIENT_ID_KEY, register}, update_reg(PatientId),
    riakc_map:update({?PRESCRIPTION_PRESCRIBER_ID_KEY, register}, update_reg(PrescriberId),
    riakc_map:update({?PRESCRIPTION_PHARMACY_ID_KEY, register}, update_reg(PharmacyId),
    riakc_map:update({?PRESCRIPTION_DATE_PRESCRIBED_KEY, register}, update_reg(DatePrescribed),
    riakc_map:update({?PRESCRIPTION_DATE_PROCESSED_KEY, register}, update_reg(DateProcessed),
    riakc_map:update({?PRESCRIPTION_IS_PROCESSED_KEY, register}, update_reg(IsProcessed),
    riakc_map:update({?PRESCRIPTION_DRUGS_KEY, set}, update_set(Drugs), riakc_map:new()))))))));

unpack(_, staff,
            #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions}) ->
    riakc_map:update({?STAFF_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?STAFF_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?STAFF_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?STAFF_SPECIALITY_KEY, register}, update_reg(Speciality),
    riakc_map:update({?STAFF_PRESCRIPTIONS_KEY, set}, update_set(Prescriptions), riakc_map:new()))))).

update_reg(V) when is_list(V) -> fun(R) -> riakc_register:set(list_to_binary(V), R) end;
update_reg(V) when is_binary(V) -> fun(R) -> riakc_register:set(V, R) end;
update_reg(V) when is_integer(V) -> fun(R) -> riakc_register:set(list_to_binary(integer_to_list(V)), R) end.
update_set(Vs) -> fun(S) -> riakc_set:add_elements(lists:map(fun bin/1, Vs), S) end.

bin(V) when is_list(V) -> list_to_binary(V);
bin(V) when is_binary(V) -> V;
bin(V) -> term_to_binary(V).

%% An alternative to riakc_map:fetch/2 that returns a default value if the key can't be fetched
-spec fetch(riakc_map:map(), binary(), atom(), term()) -> term().

fetch(Map, Key, Type, DefaultVal) ->
    try
        riakc_map:fetch({Key, Type}, Map)
    catch
        _:_ -> DefaultVal
    end.

fetch_prescs(DataModel, Map, Key) ->
    Prescs = fetch(Map, Key, set, []),
    parse_prescs(DataModel, Prescs).

parse_prescs(nested, Prescs) -> lists:map(fun(P) -> pack(nested, prescription, P) end, Prescs);
parse_prescs(non_nested, Prescs) -> Prescs.

%% Returns the name of the Riak bucket where entities of a certain type should be stored.
-spec get_bucket(entity()) -> binary().

get_bucket(facility) ->         <<"facilities">>;
get_bucket(patient) ->          <<"patients">>;
get_bucket(pharmacy) ->         <<"pharmacies">>;
get_bucket(prescription) ->     <<"prescriptions">>;
get_bucket(staff) ->            <<"staff">>.
