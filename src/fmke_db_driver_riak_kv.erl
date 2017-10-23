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
-module(fmke_db_driver_riak_kv).
-include("fmke.hrl").
-include("fmk_kv.hrl").

-behaviour(fmke_gen_simplified_kv_driver).

%% API
-export([
    %% Setup and teardown functions
    start/1,
    stop/1,

    %% Transactional context functions
    start_transaction/1,
    commit_transaction/1,

    %% Data access functions (with transactional context)
    get/3,
    put/4
]).

-define (BUCKET_TYPE, <<"maps">>).

%% -------------------------------------------------------------------
%% Setup and teardown functions
%% -------------------------------------------------------------------
start(Params) ->
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
        {db_conn_module, riakc_pb_socket},
        {db_conn_pool_size, ConnPoolSize}
    ]),
    {ok,Pid}.

stop(_Context = {_,Pid,_}) ->
    riakc_pb_socket:stop(Pid).

%% -------------------------------------------------------------------
%% Data access exports - Interaction with Riak KV
%% -------------------------------------------------------------------
put(Key,KeyType,ListOps,Context={Pid}) ->
    %% Create new local riak map object.
    Map = build_ops(ListOps),

    %% Save map to Riak
    Result = riakc_pb_socket:update_type(
      %% structure of update_type
      %% (Pid, {bucket_type, bucket_name}, key, object)
      Pid, {<<"maps">>, get_bucket(KeyType)}, Key, riakc_map:to_op(Map)
    ),
    {Result,Context}.

get(Key, RecordType, Context) ->
    BucketName = get_bucket(RecordType),
    case get_key(Key,BucketName,Context) of
        {error,{notfound,map}} -> {{error, not_found}, Context};
        {{ok,[]}} -> {{error, not_found}, Context};
        {ok,Object} -> {{ok, build_app_record(RecordType,Object)}, Context}
    end.

build_app_record(facility,Object) ->
    Id = riakc_map:fetch({?FACILITY_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?FACILITY_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?FACILITY_ADDRESS_KEY, register}, Object),
    Type = riakc_map:fetch({?FACILITY_TYPE_KEY, register}, Object),
    #facility{id=Id,name=Name,address=Address,type=Type};
build_app_record(patient,Object) ->
    Id = riakc_map:fetch({?PATIENT_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?PATIENT_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?PATIENT_ADDRESS_KEY, register}, Object),
    Prescriptions = parse_nested_prescriptions(tryfetch(?PATIENT_PRESCRIPTIONS_KEY,map,Object,[])),
    #patient{id=Id,name=Name,address=Address,prescriptions=Prescriptions};
build_app_record(pharmacy,Object) ->
    Id = riakc_map:fetch({?PHARMACY_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?PHARMACY_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?PHARMACY_ADDRESS_KEY, register}, Object),
    Prescriptions = parse_nested_prescriptions(tryfetch(?PHARMACY_PRESCRIPTIONS_KEY,map,Object,[])),
    #pharmacy{id=Id,name=Name,address=Address,prescriptions=Prescriptions};
build_app_record(prescription,Object) ->
    Id = riakc_map:fetch({?PRESCRIPTION_ID_KEY, register}, Object),
    PatientId = tryfetch(?PRESCRIPTION_PATIENT_ID_KEY,register,Object,<<"undefined">>),
    PrescriberId = tryfetch(?PRESCRIPTION_PRESCRIBER_ID_KEY,register,Object,<<"undefined">>),
    PharmacyId = tryfetch(?PRESCRIPTION_PHARMACY_ID_KEY,register,Object,<<"undefined">>),
    DatePrescribed = riakc_map:fetch({?PRESCRIPTION_DATE_PRESCRIBED_KEY, register}, Object),
    IsProcessed = tryfetch(?PRESCRIPTION_IS_PROCESSED_KEY,register,Object,?PRESCRIPTION_NOT_PROCESSED_VALUE),
    DateProcessed = tryfetch(?PRESCRIPTION_DATE_PROCESSED_KEY,register,Object,<<"undefined">>),
    Drugs = riakc_map:fetch({?PRESCRIPTION_DRUGS_KEY, set}, Object),
    #prescription{
        id=Id
        ,patient_id=PatientId
        ,pharmacy_id=PharmacyId
        ,prescriber_id=PrescriberId
        ,date_prescribed=DatePrescribed
        ,date_processed=DateProcessed
        ,drugs=Drugs
        ,is_processed=IsProcessed
    };
build_app_record(staff,Object) ->
    Id = riakc_map:fetch({?STAFF_ID_KEY, register}, Object),
    Name = riakc_map:fetch({?STAFF_NAME_KEY, register}, Object),
    Address = riakc_map:fetch({?STAFF_ADDRESS_KEY, register}, Object),
    Speciality = riakc_map:fetch({?STAFF_SPECIALITY_KEY, register}, Object),
    Prescriptions = parse_nested_prescriptions(tryfetch(?STAFF_PRESCRIPTIONS_KEY,map,Object,[])),
    #staff{id=Id,name=Name,address=Address,speciality=Speciality,prescriptions=Prescriptions}.

tryfetch(Key,KeyType,Map,DefaultVal) ->
    try
        riakc_map:fetch({Key, KeyType}, Map)
    catch
        _:_ -> DefaultVal
    end.

parse_nested_prescriptions([]) -> [];
parse_nested_prescriptions([[]]) -> [];
parse_nested_prescriptions({{_NestedKey,map},ListPrescriptions}) ->
    [build_app_record(prescription,Map) || Map <- ListPrescriptions];
parse_nested_prescriptions(ListPrescriptions=[_H|_T]) when is_list(ListPrescriptions) ->
    [build_app_record(prescription,{map,ListFields,[],[],undefined}) || {{_SomeKey,map},ListFields} <- ListPrescriptions].


build_ops(ListOps) ->
    Map = riakc_map:new(),
    put_key_build_ops_rec(Map,ListOps).

put_key_build_ops_rec(Map, []) ->
    Map;
put_key_build_ops_rec(Map, _ListOps = [H|T]) ->
    NewMap = case H of
        {create_register,Key,Value} ->
            update_nested_register(Map,set,Key,Value);
        {create_set,Key,Elements} ->
            update_nested_set(Map,add,Key,Elements);
        {create_map,Key,ListUpdates} ->
            update_nested_map(Map,Key,ListUpdates);
        {create_flag,Op,Key} ->
            %% NOTE: not used in practice
            update_nested_flag(Map,Op,Key);
        {create_counter,Key,Op,Value} ->
            %% NOTE: not used in practice
            update_nested_counter(Map,Op,Key,Value);
        {update_map,Key,ListUpdates} ->
            update_nested_map(Map,Key,ListUpdates)
    end,
    put_key_build_ops_rec(NewMap, T).



update_nested_map(Map, _NestedKey, []) -> Map;
update_nested_map(Map, NestedKey, ListUpdates) when is_binary(NestedKey) ->
    Key = {NestedKey, map},
    UpdateFunction = fun(M) -> put_key_build_ops_rec(M,ListUpdates) end,
    update_local_map_obj(Key,UpdateFunction,Map).

update_nested_register(Map, Op, NestedKey, Value) when is_list(NestedKey) ->
    update_nested_register(Map,Op,list_to_binary(NestedKey),Value);

update_nested_register(Map, Op, NestedKey, Value) when is_integer(Value) ->
    update_nested_register(Map,Op,NestedKey,integer_to_binary(Value));

update_nested_register(Map, Op, NestedKey, Value) when is_list(Value) ->
    update_nested_register(Map,Op,NestedKey,list_to_binary(Value));

update_nested_register(Map, Op, NestedKey, Value) when is_binary(NestedKey), is_binary(Value) ->
    Key = {NestedKey, register},
    UpdateFunction = get_nested_register_updt_fun(Map,Op,NestedKey,Value),
    update_local_map_obj(Key,UpdateFunction,Map).

update_nested_set(Map, Op, NestedKey, Value) when is_binary(NestedKey) ->
    Key = {NestedKey, set},
    UpdateFunction = get_nested_set_updt_fun(Op,Value),
    update_local_map_obj(Key,UpdateFunction,Map).

update_nested_flag(Map, Op, NestedKey) ->
    Key = {NestedKey, flag},
    UpdateFunction = get_nested_flag_updt_fun(Op),
    update_local_map_obj(Key,UpdateFunction,Map).

update_nested_counter(Map, Op, NestedKey, Value) when is_binary(NestedKey), is_integer(Value) ->
    Key = {NestedKey, counter},
    UpdateFunction = get_nested_counter_updt_fun(Op,Value),
    update_local_map_obj(Key,UpdateFunction,Map).

get_nested_register_updt_fun(Map,Op,NestedKey,Value) ->
    case Op of
        set ->
            fun(S) -> riakc_register:set(Value, S) end;
        erase ->
            fun(_S) -> riakc_map:erase({NestedKey, register}, Map) end
    end.

get_nested_set_updt_fun(Op, Values) ->
    BinaryValues = [list_to_binary(Element)|| Element <- Values],
    case Op of
        add ->
            fun(S) -> riakc_set:add_elements(BinaryValues, S) end;
        delete ->
            fun(S) -> riakc_set:del_element(Values, S) end
    end.

get_nested_flag_updt_fun(Op) ->
    case Op of
        set -> fun(F) -> riakc_flag:enable(F) end;
        unset -> fun(F) -> riakc_flag:disable(F) end
    end.

get_nested_counter_updt_fun(Op,Amount) ->
    case Op of
        inc -> fun(C) -> riakc_counter:increment(Amount, C) end;
        dec -> fun(C) -> riakc_counter:decrement(Amount, C) end
    end.

update_local_map_obj(NestedKey, UpdateFunction, Map) ->
    riakc_map:update(NestedKey, UpdateFunction, Map).

get_key(Key, _Type, _Context) when is_list(Key) ->
    get_key(list_to_binary(Key),_Type,_Context);

get_key(_Key, Type, _Context) when is_atom(Type) ->
    get_key(_Key,get_bucket_from_entity(Type),_Context);

get_key(_Key, _Bucket, {}) ->
    erlang:error(no_transactional_context);

get_key(Key, Bucket, _Context = {Pid}) ->
    riakc_pb_socket:fetch_type(Pid, {?BUCKET_TYPE, Bucket}, Key).

get_bucket(facility) ->
    <<"facilities">>;
get_bucket(patient) ->
    <<"patients">>;
get_bucket(pharmacy) ->
    <<"pharmacies">>;
get_bucket(prescription) ->
    <<"prescriptions">>;
get_bucket(staff) ->
    <<"staff">>;
get_bucket(_Other) ->
    <<"undefined">>.

%% TODO dis not correct'
get_bucket_from_entity(Entity) ->
    list_to_binary(atom_to_list(Entity)).

%% -------------------------------------------------------------------
%% Riak transaction related exports (Transactional API)
%% -------------------------------------------------------------------

start_transaction(_Context = {_Pid}) ->
    erlang:error(transaction_already_started);

start_transaction(_Context = {}) ->
    Pid = poolboy:checkout(fmke_db_connection_pool),
    {ok, {Pid}}.

commit_transaction(_Context = {Pid}) ->
    poolboy:checkin(fmke_db_connection_pool, Pid),
    {ok, {}}.
