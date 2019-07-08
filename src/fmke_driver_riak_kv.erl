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


%% API
-export([
    %% Transactional context functions
    start_transaction/1,
    commit_transaction/2,

    %% Data access functions (with transactional context)
    get/2,
    put/2
]).

-define (SERVER, ?MODULE).
-define (BUCKET_TYPE, <<"maps">>).
-define (REF_BUCKET_TYPE, <<"sets">>).
-define (PRESC_BUCKET, <<"prescriptions">>).
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
%% Riak transaction related exports (Transactional API)
%% -------------------------------------------------------------------
start_transaction(_Opts) ->
    Pid = fmke_db_conn_manager:checkout(),
    {ok, Pid}.


commit_transaction(Pid, _Opts) ->
    fmke_db_conn_manager:checkin(Pid),
    ok.

%% -------------------------------------------------------------------
%% Data access exports - Interaction with Riak KV
%% -------------------------------------------------------------------
get(Keys, Pid) ->
    {ok, DataModel} = application:get_env(?APP, data_model),
    Results = lists:map(
                    fun({Key, Type}) ->
                        case riakc_pb_socket:fetch_type(Pid, get_riak_props(Type), Key) of
                            {error, {notfound, map}} ->
                                {error, not_found};
                            {error, {notfound, set}} ->
                                [];
                            {ok, Value} ->
                                pack(Pid, DataModel, Type, Value)
                        end
                    end, Keys),
    {Results, Pid}.

put(Entries, Pid) ->
    {ok, DataModel} = application:get_env(?APP, data_model),
    Results = lists:map(
                    fun({Key, Type, Value}) ->
                        Bucket = get_bucket(Type),
                        UnpackedVal = unpack(DataModel, Type, Value),
                        riakc_pb_socket:update_type(Pid, {?BUCKET_TYPE, Bucket}, Key, riakc_map:to_op(UnpackedVal))
                    end, Entries),
    {Results, Pid}.

%% Parses a value obtained from Riak into a common application record format.
-spec pack(pid(), atom(), entity(), riakc_map:map()) -> app_record().
pack(_Pid, _, facility, Fac) ->
    [Id, Name, Address, Type] = lists:map(fun(F) -> riakc_map:fetch(F, Fac) end, ?FAC_FIELDS),
    #facility{id = Id, name = Name, address = Address, type = Type};

pack(Pid, DataModel, patient, Pat) ->
    [Id, Name, Address] = lists:map(fun(F) -> riakc_map:fetch(F, Pat) end, ?PAT_FIELDS),
    Prescriptions = fetch_prescs(DataModel, Pid, Pat, ?PATIENT_PRESCRIPTIONS_KEY),
    #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions};

pack(Pid, DataModel, pharmacy, Pharm) ->
    [Id, Name, Address] = lists:map(fun(F) -> riakc_map:fetch(F, Pharm) end, ?PHARM_FIELDS),
    Prescriptions = fetch_prescs(DataModel, Pid, Pharm, ?PHARMACY_PRESCRIPTIONS_KEY),
    #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions};

pack(_Pid, _DataModel, prescription, Presc) ->
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

pack(Pid, DataModel, staff, Staff) ->
    [Id, Name, Address, Speciality] = lists:map(fun(F) -> riakc_map:fetch(F, Staff) end, ?STAFF_FIELDS),
    Prescriptions = fetch_prescs(DataModel, Pid, Staff, ?STAFF_PRESCRIPTIONS_KEY),
    #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions};

pack(_Pid, non_nested, prescription_ref, Set) ->
    riakc_set:value(Set).

unpack(_, facility, #facility{id = Id, name = Name, address = Address, type = Type}) ->
    riakc_map:update({?FACILITY_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?FACILITY_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?FACILITY_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?FACILITY_TYPE_KEY, register}, update_reg(Type), riakc_map:new()))));

unpack(nested, patient, #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
    riakc_map:update({?PATIENT_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PATIENT_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PATIENT_ADDRESS_KEY, register}, update_reg(Address),
    update_map(?PATIENT_PRESCRIPTIONS_KEY, Prescriptions))));

unpack(non_nested, patient, #patient{id = Id, name = Name, address = Address}) ->
    riakc_map:update({?PATIENT_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PATIENT_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PATIENT_ADDRESS_KEY, register}, update_reg(Address), riakc_map:new())));

unpack(nested, pharmacy, #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
    riakc_map:update({?PHARMACY_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PHARMACY_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PHARMACY_ADDRESS_KEY, register}, update_reg(Address),
    update_map(?PHARMACY_PRESCRIPTIONS_KEY, Prescriptions))));

unpack(non_nested, pharmacy, #pharmacy{id = Id, name = Name, address = Address}) ->
    riakc_map:update({?PHARMACY_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?PHARMACY_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?PHARMACY_ADDRESS_KEY, register}, update_reg(Address), riakc_map:new())));

unpack(non_nested, prescription, Element) ->
    riakc_set:add_element(Element, riakc_set:new());

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

unpack(nested, staff,
            #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions}) ->
    riakc_map:update({?STAFF_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?STAFF_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?STAFF_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?STAFF_SPECIALITY_KEY, register}, update_reg(Speciality),
    update_map(?STAFF_PRESCRIPTIONS_KEY, Prescriptions)))));

unpack(non_nested, staff,
            #staff{id = Id, name = Name, address = Address, speciality = Speciality}) ->
    riakc_map:update({?STAFF_ID_KEY, register}, update_reg(Id),
    riakc_map:update({?STAFF_NAME_KEY, register}, update_reg(Name),
    riakc_map:update({?STAFF_ADDRESS_KEY, register}, update_reg(Address),
    riakc_map:update({?STAFF_SPECIALITY_KEY, register}, update_reg(Speciality), riakc_map:new())))).

update_reg(V) when is_list(V) -> fun(R) -> riakc_register:set(list_to_binary(V), R) end;
update_reg(V) when is_binary(V) -> fun(R) -> riakc_register:set(V, R) end;
update_reg(V) when is_integer(V) -> fun(R) -> riakc_register:set(list_to_binary(integer_to_list(V)), R) end.
update_set(Vs) -> fun(S) -> riakc_set:add_elements(lists:map(fun bin/1, Vs), S) end.
update_map(NestedKey, Ps) ->
    lists:foldl(fun(P, Acc) ->
        riakc_map:update({NestedKey, map}, fun(M) ->
            riakc_map:update({bin(P#prescription.id), register}, fun(R) ->
                riakc_register:set(term_to_binary(P), R) end, M)
            end,
        Acc)
    end, riakc_map:new(), Ps).

bin(V) when is_list(V) -> list_to_binary(V);
bin(V) when is_binary(V) -> V;
bin(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
bin(V) -> term_to_binary(V).

%% An alternative to riakc_map:fetch/2 that returns a default value if the key can't be fetched
-spec fetch(riakc_map:map(), binary(), atom(), term()) -> term().

fetch(Map, Key, Type, DefaultVal) ->
    try
        riakc_map:fetch({Key, Type}, Map)
    catch
        _:_ -> DefaultVal
    end.

fetch_prescs(nested, _Pid, Map, Key) ->
    parse_prescs(nested, fetch(Map, Key, map, []));
fetch_prescs(non_nested, _, _, _) ->
    [].

parse_prescs(nested, Prescs) ->
    lists:map(fun({{_PrescId, register}, Bin}) -> binary_to_term(Bin) end, Prescs).

%% Returns the name of the Riak bucket where entities of a certain type should be stored.
-spec get_bucket(entity()) -> binary().

get_bucket(facility) ->         <<"facilities">>;
get_bucket(patient) ->          <<"patients">>;
get_bucket(pharmacy) ->         <<"pharmacies">>;
get_bucket(prescription) ->     <<"prescriptions">>;
get_bucket(staff) ->            <<"staff">>.

get_riak_props(prescription_ref) ->
    {?REF_BUCKET_TYPE, ?PRESC_BUCKET};
get_riak_props(Type) ->
    {?BUCKET_TYPE, get_bucket(Type)}.
