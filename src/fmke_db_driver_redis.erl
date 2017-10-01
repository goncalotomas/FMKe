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
-include("fmke.hrl").
-include("fmk_kv.hrl").

-behaviour(fmke_gen_kv_driver).

-export([
    init/1,
    stop/1,

    start_transaction/1,
    commit_transaction/1,

    get/3,
    put/4
]).

%% -------------------------------------------------------------------
%% Setup and teardown functions
%% -------------------------------------------------------------------
init(Params) ->
    case fmke_sup:start_link() of
       {ok, Pid} -> start_conn_pool(Pid, Params);
       _Error -> _Error
    end.

start_conn_pool(Pid, Params) ->
    Hostnames = proplists:get_value(db_conn_hostnames, Params),
    Ports = proplists:get_value(db_conn_ports, Params),
    ConnPoolSize = proplists:get_value(db_conn_pool_size, Params),
    {ok,_} = fmke_db_conn_pool:start([
        {db_conn_hostnames, Hostnames},
        {db_conn_ports, Ports},
        {db_conn_module, eredis},
        {db_conn_pool_size, ConnPoolSize}
    ]),
    {ok,Pid}.

stop({Pid}) ->
    eredis:stop(Pid).

%% Transactions %% Dummy transactions; Redis doesnot support transactions in a
%% cluster setup
start_transaction({}) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    {ok, {Pid}}.

commit_transaction({Pid}) ->
    poolboy:checkin(fmke_db_connection_pool, Pid),
    {ok, {}}.

% %% Get
% %% Redis does not support map that holds keys of different types,
% %% So just return a dummy object and use find_key to get specific key-value pairs.
% get(Key, _KeyType, Context) ->
%     {ok, {map, Key}, Context}.
get(Key, _KeyType, Context) ->
  case execute_get(Context, ["HGETALL", Key]) of
      {ok,[]} -> {{error, not_found}, Context};
      {ok,Res} -> {{ok, build_app_record(Res)}, Context}
  end.

build_app_record([?PATIENT_ID_KEY, Id,
                  ?PATIENT_NAME_KEY, Name,
                  ?PATIENT_ADDRESS_KEY, Address]) ->
                      #patient{id=Id, name=Name, address=Address};

build_app_record([?FACILITY_ID_KEY, Id,
                  ?FACILITY_NAME_KEY, Name,
                  ?FACILITY_ADDRESS_KEY, Address,
                  ?FACILITY_TYPE_KEY, Type]) ->
                      #facility{id=Id, name=Name, address=Address, type=Type};

build_app_record([?PHARMACY_ID_KEY, Id,
                  ?PHARMACY_NAME_KEY, Name,
                  ?PHARMACY_ADDRESS_KEY, Address]) ->
                      #pharmacy{id=Id, name=Name, address=Address};

build_app_record([?STAFF_ID_KEY, Id,
                  ?STAFF_NAME_KEY, Name,
                  ?STAFF_ADDRESS_KEY, Address,
                  ?STAFF_SPECIALITY_KEY, Speciality]) ->
                      #staff{id=Id, name=Name, address=Address, speciality=Speciality};

build_app_record([?PRESCRIPTION_ID_KEY, Id,
                  ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
                  ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId,
                  ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
                  ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed]) ->
                      #prescription{
                          id=Id
                          ,patient_id=PatientId
                          ,pharmacy_id=PharmacyId
                          ,prescriber_id=PrescriberId
                          ,date_prescribed=DatePrescribed
                          ,drugs=[]
                      };

build_app_record([?PRESCRIPTION_ID_KEY, Id,
                  ?PRESCRIPTION_PATIENT_ID_KEY, PatientId,
                  ?PRESCRIPTION_PRESCRIBER_ID_KEY, PrescriberId,
                  ?PRESCRIPTION_PHARMACY_ID_KEY, PharmacyId,
                  ?PRESCRIPTION_DATE_PRESCRIBED_KEY, DatePrescribed,
                  ?PRESCRIPTION_DATE_PROCESSED_KEY, DateProcessed,
                  ?PRESCRIPTION_IS_PROCESSED_KEY, IsProcessed]) ->
                      #prescription{
                          id=Id
                          ,patient_id=PatientId
                          ,pharmacy_id=PharmacyId
                          ,prescriber_id=PrescriberId
                          ,date_prescribed=DatePrescribed
                          ,date_processed=DateProcessed
                          ,drugs=[]
                          ,is_processed=IsProcessed
                      }.

find_key({map, Map}, Key, register, Context) ->
    Query = ["HGET", Map, Key],
    case execute_get(Context, Query) of
      {ok, Res} -> {ok, Res, Context};
      {error, Reason} -> {error, Reason, Context}
    end;
find_key({map, Map}, Key, map, Context) ->
    InnerKey = inner_map_key(Map, Key),
    {ok, {map, InnerKey}, Context};
find_key({map, Map}, Key, set, Context) ->
    InnerKey = inner_map_key(Map, Key),
    Query = ["SMEMBERS", InnerKey],
    case execute_get(Context, Query) of
        {ok, Res} -> {ok, Res, Context};
        {error, Reason} -> {error, Reason, Context}
    end.

%% Updates
put(Key, _KeyType, ListOfOps, Context) ->
    try  update_nested_objects(Key, ListOfOps, Context) of
        ok -> {ok, Context}
    catch
      _:Reason -> {error, Reason, Context}
    end.

update_nested_objects(Key, ListOfOps, Context) ->
    lists:foreach(fun(Op) ->
                          case update_nested_object(Key, Op, Context) of
                              ok -> ok;
                              {error, Reason} -> throw(erlang:error(Reason))
                          end
                       end, ListOfOps).

update_nested_object(Map, {create_register, Key, Value}, Context) ->
    Op = ["HSET", Map, Key, Value],
    execute_op(Context, Op);

update_nested_object(Map, {create_set, Key, Elements}, Context) ->
    InnerKey = inner_map_key(Map, Key),
    Op = ["SADD", InnerKey] ++ Elements,
    execute_op(Context, Op);

update_nested_object(Map, {create_map, Key, NestedOps}, Context) ->
    InnerKey = inner_map_key(Map, Key),
    update_nested_objects(InnerKey, NestedOps, Context);

update_nested_object(Map, {update_map, Key, NestedOps}, Context) ->
   InnerKey = inner_map_key(Map, Key),
   update_nested_objects(InnerKey, NestedOps, Context).

inner_map_key(Map, Key) ->
    list_to_binary(binary_to_list(Map) ++ binary_to_list(Key)).

execute_op({Pid}, Op) ->
    case eredis:q(Pid,Op) of
      {ok, _} -> ok;
      {error, Reason} -> {error, Reason}
    end.

execute_get({Pid}, Op) ->
   case eredis:q(Pid,Op) of
       {ok, Res} -> {ok, Res};
       {error, Reason} -> {error, Reason}
   end.
