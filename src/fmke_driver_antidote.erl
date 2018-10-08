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
-module(fmke_driver_antidote).
%
% -include("fmke.hrl").
% -include("fmke_kv.hrl").
% -include("fmke_antidote.hrl").
%
% -behaviour(gen_fmke_kv_driver).
% -behaviour(gen_server).
%
% %% gen_server exports
% -export ([
%   init/1,
%   handle_call/3,
%   handle_cast/2
% ]).
%
% %% API
% -export([
%     %% Setup and teardown functions
%     start/1,
%     stop/0,
%
%     %% Transactional context functions
%     start_transaction/1,
%     commit_transaction/2,
%
%     %% Data access functions (with transactional context)
%     get/2,
%     put/2
% ]).
%
% -define (SERVER, ?MODULE).
%
% %% -------------------------------------------------------------------
% %% Antidote Context Definition
% %% -------------------------------------------------------------------
% -record(antidote_context, {
%     pid :: pid(),
%     txn_id :: txid()
% }).
%
% %% -------------------------------------------------------------------
% %% Setup and teardown functions
% %% -------------------------------------------------------------------
% start(DataModel) ->
%     gen_server:start_link({local, ?SERVER}, ?MODULE, DataModel, []).
%
% stop() ->
%     gen_server:call(?MODULE, stop).
%
% init(DataModel) ->
%     lager:info("~p starting with ~p data model~n", [?MODULE, DataModel]),
%     {ok, DataModel}.
%
% %% -------------------------------------------------------------------
% %% Riak transaction related exports (Transactional API)
% %% -------------------------------------------------------------------
% start_transaction(_Opts) ->
%     gen_server:call(?MODULE, start_transaction).
%
% commit_transaction(Pid, _Opts) ->
%     gen_server:call(?MODULE, {commit_transaction, Pid}).
%
% %% -------------------------------------------------------------------
% %% Data access exports - Interaction with Riak KV
% %% -------------------------------------------------------------------
% get(Keys, Pid) ->
%     gen_server:call(?MODULE, {get, Keys, Pid}).
%
% put(Entries, Pid) ->
%     gen_server:call(?MODULE, {put, Entries, Pid}).
%
% %% gen server callbacks, contain driver logic
%
% handle_cast(_Msg, State) ->
%     {noreply, State}.
%
% handle_call(start_transaction, _From, {DataModel, _}) ->
%     %% Driver needs a Pid to request client operations, fetch Pid from fmke_db_conn_manager and put it in TxnContext
%     Pid = fmke_db_conn_manager:checkout(),
%     {ok, TxnId} = antidotec_pb:start_transaction(Pid, ignore, {}),
%     {reply, {ok, Pid}, {DataModel, #antidote_context{pid = Pid, txn_id = TxnId}}};
%
% handle_call({commit_transaction, Pid}, _From, DataModel) ->
%     %% Interaction with database is complete for current operation, return pid to connection pool
%     CmtRes = ,
%     Result = fmke_db_conn_manager:checkin(Pid),
%     CommitResult = case antidotec_pb:commit_transaction(Pid, TransactionId) of
%       {ok, _Something} -> {Result, #antidote_context{pid = Pid}};
%       {error, unknown} -> {{error, txn_aborted}, #antidote_context{pid = Pid}}
%     end
%     {reply, ok, DataModel};
%
% handle_call({get, Keys, Pid}, _From, DataModel) ->
%     Results = lists:map(
%                     fun({Key, Type}) ->
%                         Bucket = get_bucket(Type),
%                         case riakc_pb_socket:fetch_type(Pid, {?BUCKET_TYPE, Bucket}, Key) of
%                             {error, {notfound, map}} ->
%                                 {error, not_found};
%                             {ok, Value} ->
%                                 pack(DataModel, Type, Value);
%                             Other ->
%                                 lager:error("Got unrecognised value: ~p~n", [Other]),
%                                 {error, unrecognised_value, Other}
%                         end
%                     end, Keys),
%     {reply, {Results, Pid}, DataModel};
%
% get(Key, RecordType, Context = #antidote_context{pid = Pid, txn_id = TxnId}) ->
%   Object = create_read_bucket(Key, ?MAP), %% application records are all of type ?MAP
%   {ok, [Value]} = antidotec_pb:read_values(Pid, [Object], TxnId),
%   case Value of
%     {_Something, []} -> {{error, not_found}, Context};
%     {map, MapObject} -> {{ok, build_app_record(RecordType, MapObject)}, Context}
%   end.
%
% build_app_record(patient, Object) ->
%   Id = find_key(Object, ?PATIENT_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?PATIENT_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?PATIENT_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Prescriptions = read_nested_prescriptions(Object, ?PATIENT_PRESCRIPTIONS_KEY),
%   #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
% build_app_record(pharmacy, Object) ->
%   Id = find_key(Object, ?PHARMACY_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?PHARMACY_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?PHARMACY_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Prescriptions = read_nested_prescriptions(Object, ?PHARMACY_PRESCRIPTIONS_KEY),
%   #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
% build_app_record(staff, Object) ->
%   Id = find_key(Object, ?STAFF_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?STAFF_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?STAFF_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Speciality = find_key(Object, ?STAFF_SPECIALITY_KEY, ?LWWREG, <<"undefined">>),
%   Prescriptions = read_nested_prescriptions(Object, ?STAFF_PRESCRIPTIONS_KEY),
%   #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions};
% build_app_record(facility, Object) ->
%   Id = find_key(Object, ?FACILITY_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?FACILITY_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?FACILITY_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Type = find_key(Object, ?FACILITY_TYPE_KEY, ?LWWREG, <<"undefined">>),
%   #facility{id = Id, name = Name, address = Address, type = Type};
% build_app_record(prescription, Object) ->
%   Id = find_key(Object, ?PRESCRIPTION_ID_KEY, ?LWWREG, -1),
%   PatientId = find_key(Object, ?PRESCRIPTION_PATIENT_ID_KEY, ?LWWREG, <<"undefined">>),
%   PrescriberId = find_key(Object, ?PRESCRIPTION_PRESCRIBER_ID_KEY, ?LWWREG, <<"undefined">>),
%   PharmacyId = find_key(Object, ?PRESCRIPTION_PHARMACY_ID_KEY, ?LWWREG, <<"undefined">>),
%   DatePrescribed = find_key(Object, ?PRESCRIPTION_DATE_PRESCRIBED_KEY, ?LWWREG, <<"undefined">>),
%   IsProcessed = find_key(Object, ?PRESCRIPTION_IS_PROCESSED_KEY, ?LWWREG, ?PRESCRIPTION_NOT_PROCESSED_VALUE),
%   DateProcessed = find_key(Object, ?PRESCRIPTION_DATE_PROCESSED_KEY, ?LWWREG, <<"undefined">>),
%   Drugs = find_key(Object, ?PRESCRIPTION_DRUGS_KEY, ?ORSET, []),
%   #prescription{
%       id = Id,
%       patient_id = PatientId,
%       pharmacy_id = PharmacyId,
%       prescriber_id = PrescriberId,
%       date_prescribed = DatePrescribed,
%       date_processed = DateProcessed,
%       drugs = Drugs,
%       is_processed = IsProcessed
%   }.
%
% read_nested_prescriptions(Object, Key) ->
%   case find_key(Object, Key, ?NESTED_MAP, []) of
%       [] -> [];
%       ListPrescriptions -> lists:map(
%                               fun({_PHeader, Prescription}) ->
%                                   build_app_record(prescription, Prescription)
%                               end,
%                             ListPrescriptions)
%   end.
%
% %% Return status {(ok | error), context}
% put_map(Key, KeyType, Value, Context = #antidote_context{pid = Pid, txn_id = TxnId}) ->
%   Object = create_write_bucket(Key, KeyType, Value),
%
%   Result = antidotec_pb:update_objects(Pid, [Object], TxnId),
%   {Result, Context}.
%
% %% Return status {(ok | error), context}
% start_transaction(_OldContext) ->
%   Pid = fmke_db_conn_manager:checkout(),
%   {ok, TxnId} = antidotec_pb:start_transaction(Pid, ignore, {}),
%   {ok, #antidote_context{pid = Pid, txn_id = TxnId}}.
%
% %% Return status {(ok | error), context}
% commit_transaction(#antidote_context{pid = Pid, txn_id = TransactionId}) ->
%   CmtRes = antidotec_pb:commit_transaction(Pid, TransactionId),
%   Result = fmke_db_conn_manager:checkin(Pid),
%   case CmtRes of
%     {ok, _Something} -> {Result, #antidote_context{pid = Pid}};
%     {error, unknown} -> {{error, txn_aborted}, #antidote_context{pid = Pid}}
%   end.
%
% %% Creates an Antidote bucket of a certain type.
% -spec create_write_bucket(field(), crdt(), term()) -> object_bucket().
% create_write_bucket(Key, ?MAP, Value) ->
%   Bucket = create_read_bucket(Key, ?MAP),
%   {Bucket, update, Value}.
%
% %% Creates an Antidote bucket of a certain type.
% -spec create_read_bucket(field(), crdt()) -> object_bucket().
% create_read_bucket(Key, Type) ->
%   {Key, Type, <<"bucket">>}.
%
% %%-----------------------------------------------------------------------------
% %% Internal auxiliary functions - simplifying calls to external modules
% %%-----------------------------------------------------------------------------
% lwwreg_assign_op(Value) when is_binary(Value) ->
%   {assign, Value};
% lwwreg_assign_op(Value) when is_list(Value) ->
%   {assign, list_to_binary(Value)};
% lwwreg_assign_op(Value) when is_integer(Value) ->
%   {assign, integer_to_binary(Value)}.
%
% build_lwwreg_op(Key, Value) ->
%   build_map_op(Key, ?LWWREG, lwwreg_assign_op(Value)).
%
% build_add_set_op(Key, Elements) ->
%   build_map_op(Key, ?ORSET, add_to_set_op(Elements)).
%
% add_to_set_op(Elements) when is_list(Elements) ->
%   {add_all, [list_to_binary(X) || X <- Elements]}.
%
% build_map_op(Key, Type, Op) ->
%   {{Key, Type}, Op}.
%
% put(Key, _KeyType, ListOps, Context) ->
%   put_map(Key, ?MAP, inner_update_map(ListOps), Context).
%
% inner_update_map([]) ->
%   [];
% inner_update_map([H|T]) ->
%   HeadOp = case H of
%        %% These cases are for direct children of maps
%        {create_register, Key, Value} -> build_lwwreg_op(Key, Value);
%        {create_set, Key, Elements} -> build_add_set_op(Key, Elements);
%        %% When nested fields are necessary
%        {create_map, Key, NestedOps} -> build_update_map_bucket_op(Key, inner_update_map(NestedOps));
%        {update_map, Key, NestedOps} -> build_update_map_bucket_op(Key, inner_update_map(NestedOps))
%   end,
%   [HeadOp] ++ inner_update_map(T).
%
% find_key(Map, Key, KeyType, FallbackValue) ->
%   try lists:keyfind({Key, KeyType}, 1, Map) of
%     false -> FallbackValue;
%     {{Key, KeyType}, Value} -> Value
%   catch
%     _:_ -> FallbackValue
%   end.
%
% build_map_update_op(NestedOps) ->
%   {update, NestedOps}.
%
% build_update_map_bucket_op(Key, NestedOps) ->
%   build_map_op(Key, ?MAP, build_map_update_op(NestedOps)).
