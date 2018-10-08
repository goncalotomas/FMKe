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
-module(fmke_db_driver_redis).
% -include("fmke.hrl").
% -include("fmke_kv.hrl").
%
% -behaviour(fmke_gen_simplified_kv_driver).
%
% -export([
%     start/1,
%     stop/1,
%
%     start_transaction/1,
%     commit_transaction/1,
%
%     get/3,
%     put/4
% ]).
%
% %% -------------------------------------------------------------------
% %% Setup and teardown functions
% %% -------------------------------------------------------------------
% start(_) ->
%     ok.
%
% stop(_) ->
%     ok.
%
% %% Transactions %% Dummy transactions; Redis doesnot support transactions in a
% %% cluster setup
% start_transaction(_OldContext) ->
%     Pid = fmke_db_conn_manager:checkout(),
%     {ok, {Pid}}.
%
% commit_transaction({Pid}) ->
%     fmke_db_conn_manager:checkin(Pid),
%     {ok, {}}.
%
% %% -------------------------------------------------------------------
% %% Data access exports - Interaction with Redis
% %% -------------------------------------------------------------------
% get_presc_key(patient) -> ?PATIENT_PRESCRIPTIONS_KEY;
% get_presc_key(pharmacy) -> ?PHARMACY_PRESCRIPTIONS_KEY;
% get_presc_key(staff) -> ?STAFF_PRESCRIPTIONS_KEY.
%
% get(Key, prescription, Context) ->
%     Ops = [["HGETALL", Key], ["SMEMBERS", concat_bin_strings([Key, ?PRESCRIPTION_DRUGS_KEY])]],
%     [{ok, Fields}, {ok, Drugs}] = execute_multi_get(Context, Ops),
%     case Fields of
%         [] -> {{error, not_found}, Context};
%         _ ->  {{ok, build_app_record(Fields ++ [?PRESCRIPTION_DRUGS_KEY, Drugs])}, Context}
%     end;
%
% get(Key, KeyType, Context) when KeyType == patient; KeyType == pharmacy; KeyType == staff ->
%     Ops = [["HGETALL", Key], ["KEYS", concat_bin_strings([Key, get_presc_key(KeyType), <<"*">>])]],
%     [{ok, Fields}, {ok, PrescKeys}] = execute_multi_get(Context, Ops),
%     case Fields of
%         [] -> {{error, not_found}, Context};
%         _ ->  case PrescKeys of
%                   [] ->       {{ok, build_app_record(Fields)}, Context};
%                   [_H|_T] ->  Prescs = fetch_nested_prescriptions(Context, sort_presc_keys(lists:sort(PrescKeys))),
%                               {{ok, build_app_record(Fields ++ [get_presc_key(KeyType), Prescs])}, Context}
%               end
%     end;
%
% get(Key, _KeyType, Context) ->
%   case execute_get(Context, ["HGETALL", Key]) of
%       {ok, []} -> {{error, not_found}, Context};
%       {ok, Res} -> {{ok, build_app_record(Res)}, Context}
%   end.
%
% sort_presc_keys(Props) -> sort_presc_keys(Props, []).
%
% sort_presc_keys([], Props) -> Props;
% sort_presc_keys([H|T], Props) ->
%     case string:find(binary_to_list(H), binary_to_list(?PRESCRIPTION_DRUGS_KEY), trailing) of
%         nomatch ->
%                   NewProps = Props ++ [{H, undefined}],
%                   sort_presc_keys(T, NewProps);
%         _ ->
%                   PrescKey = string:slice(H, 0, byte_size(H)-byte_size(?PRESCRIPTION_DRUGS_KEY)),
%                   NewProps = lists:keyreplace(PrescKey, 1, Props, {PrescKey, H}),
%                   sort_presc_keys(T, NewProps)
%     end.
%
% fetch_nested_prescriptions(Context, Prescs) -> fetch_nested_prescriptions(Context, Prescs, []).
%
% fetch_nested_prescriptions(Context, [], Accum) ->
%     Prescs = execute_multi_get(Context, Accum),
%     parse_prescription_list(Prescs);
% fetch_nested_prescriptions(Context, [{Key, DrugsKey} | T], Accum) ->
%     fetch_nested_prescriptions(Context, T, [["HGETALL", Key], ["SMEMBERS", DrugsKey] | Accum]).
%
% parse_prescription_list(List) ->
%     parse_prescription_list(List, []).
%
% parse_prescription_list([], Accum) ->
%     lists:reverse(Accum);
% parse_prescription_list([{error, _Reason} | T], Accum) ->
%     parse_prescription_list(T, Accum);
% parse_prescription_list([{ok, Fields}, {ok, Drugs} | T], Accum) ->
%     parse_prescription_list(T, [build_app_record(Fields ++ [?PRESCRIPTION_DRUGS_KEY, Drugs]) | Accum]).
%
% build_app_record([?PATIENT_ID_KEY, Id,
%                   ?PATIENT_NAME_KEY, Name,
%                   ?PATIENT_ADDRESS_KEY, Address]) ->
%                       #patient{id=Id, name=Name, address=Address};
%
% build_app_record([?PATIENT_ID_KEY, Id,
%                   ?PATIENT_NAME_KEY, Name,
%                   ?PATIENT_ADDRESS_KEY, Address,
%                   ?PATIENT_PRESCRIPTIONS_KEY, Prescs]) ->
%                       #patient{id=Id, name=Name, address=Address, prescriptions=Prescs};
%
% build_app_record([?FACILITY_ID_KEY, Id,
%                   ?FACILITY_NAME_KEY, Name,
%                   ?FACILITY_ADDRESS_KEY, Address,
%                   ?FACILITY_TYPE_KEY, Type]) ->
%                       #facility{id=Id, name=Name, address=Address, type=Type};
%
% build_app_record([?PHARMACY_ID_KEY, Id,
%                   ?PHARMACY_NAME_KEY, Name,
%                   ?PHARMACY_ADDRESS_KEY, Address]) ->
%                       #pharmacy{id=Id, name=Name, address=Address};
%
% build_app_record([?PHARMACY_ID_KEY, Id,
%                   ?PHARMACY_NAME_KEY, Name,
%                   ?PHARMACY_ADDRESS_KEY, Address,
%                   ?PHARMACY_PRESCRIPTIONS_KEY, Prescriptions]) ->
%                       #pharmacy{id=Id, name=Name, address=Address, prescriptions=Prescriptions};
%
% build_app_record([?STAFF_ID_KEY, Id,
%                   ?STAFF_NAME_KEY, Name,
%                   ?STAFF_ADDRESS_KEY, Address,
%                   ?STAFF_SPECIALITY_KEY, Speciality]) ->
%                       #staff{id=Id, name=Name, address=Address, speciality=Speciality};
%
% build_app_record([?STAFF_ID_KEY, Id,
%                   ?STAFF_NAME_KEY, Name,
%                   ?STAFF_ADDRESS_KEY, Address,
%                   ?STAFF_SPECIALITY_KEY, Speciality,
%                   ?STAFF_PRESCRIPTIONS_KEY, Prescriptions]) ->
%                       #staff{id=Id, name=Name, address=Address, speciality=Speciality, prescriptions=Prescriptions};
%
% build_app_record([?PRESCRIPTION_ID_KEY, Id,
%                   ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
%                   ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId,
%                   ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
%                   ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed,
%                   ?PRESCRIPTION_DRUGS_KEY, Drugs]) ->
%                       #prescription{
%                           id = Id,
%                           patient_id = PatientId,
%                           pharmacy_id = PharmacyId,
%                           prescriber_id = PrescriberId,
%                           date_prescribed = DatePrescribed,
%                           drugs = Drugs
%                       };
%
% build_app_record([?PRESCRIPTION_ID_KEY, Id,
%                   ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
%                   ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId,
%                   ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
%                   ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed,
%                   ?PRESCRIPTION_IS_PROCESSED_KEY, IsProcessed,
%                   ?PRESCRIPTION_DATE_PROCESSED_KEY, DateProcessed,
%                   ?PRESCRIPTION_DRUGS_KEY, Drugs]) ->
%                       #prescription{
%                           id = Id,
%                           patient_id = PatientId,
%                           pharmacy_id = PharmacyId,
%                           prescriber_id = PrescriberId,
%                           date_prescribed = DatePrescribed,
%                           date_processed = DateProcessed,
%                           drugs = Drugs,
%                           is_processed = IsProcessed
%                       }.
%
% %% Updates
% put(Key, _KeyType, ListOfOps, Context) ->
%     try  update_nested_objects(Key, ListOfOps, Context) of
%         ok -> {ok, Context}
%     catch
%       _:Reason -> {error, Reason, Context}
%     end.
%
% update_nested_objects(Key, ListOfOps, Context) ->
%     lists:foreach(fun(Op) ->
%                       case update_nested_object(Key, Op, Context) of
%                           ok -> ok;
%                           {error, Reason} -> throw(erlang:error(Reason))
%                       end
%                    end, ListOfOps).
%
% update_nested_object(Map, {create_register, Key, Value}, Context) ->
%     Op = ["HSET", Map, Key, Value],
%     execute_op(Context, Op);
%
% update_nested_object(Map, {create_set, Key, Elements}, Context) ->
%     InnerKey = inner_map_key(Map, Key),
%     Op = ["SADD", InnerKey] ++ Elements,
%     execute_op(Context, Op);
%
% update_nested_object(Map, {create_map, Key, NestedOps}, Context) ->
%     InnerKey = inner_map_key(Map, Key),
%     update_nested_objects(InnerKey, NestedOps, Context);
%
% update_nested_object(Map, {update_map, Key, NestedOps}, Context) ->
%     InnerKey = inner_map_key(Map, Key),
%     update_nested_objects(InnerKey, NestedOps, Context).
%
% inner_map_key(Map, Key) ->
%     list_to_binary(binary_to_list(Map) ++ binary_to_list(Key)).
%
% execute_op({Pid}, Op) ->
%     case eredis:q(Pid, Op) of
%         {ok, _} -> ok;
%         {error, Reason} -> {error, Reason}
%     end.
%
% execute_get({Pid}, Op) ->
%    eredis:q(Pid, Op).
%
% execute_multi_get({Pid}, Ops) ->
%   case eredis:qp(Pid, Ops) of
%       {error, Reason} -> {error, Reason};
%       Res -> Res
%   end.
%
% concat_bin_strings(Keys) ->
%   iolist_to_binary(Keys).
