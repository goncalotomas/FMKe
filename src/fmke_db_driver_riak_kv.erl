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
-include("fmk.hrl").

-behaviour(gen_kv_driver).

%% API
-export([
         init/1,
         stop/1,
         start_transaction/1,
         get_application_record/3,
         commit_transaction/1
        ]).

-export([
         %get_counter/2,
         %inc_counter/3,
         %dec_counter/3,
         get_key/3,
         get_map/2,
         update_map/3
        ]).

%-define (CRDT_COUNTER, riakc_counter).
%-define (CRDT_SET, riakc_set).
%-define (CRDT_REGISTER, riakc_register).
-define (CRDT_MAP, riakc_map).


%% Context is a tuple {riak, Pid}.
%% Key is a tuple {<<"BUCKET">>, <<"KEY">>}.


%%ATTENTION: Does not manage multiple connections.
init({[NodeAddress | _OtherNA] , [Port | _OP]}) ->
    set_app_vars([NodeAddress],[Port]),
    start_sup_link();

%%ATTENTION: Does not manage multiple connections.
init({[NodeAddress | _OtherNA] , [Port | _OP], HeadNodeName, HeadNodeCookie}) ->
    erlang:set_cookie(HeadNodeName, HeadNodeCookie),
    set_app_vars([NodeAddress],[Port]),
    case start_sup_link() of
        {ok, Pid} ->
            create_buckets(HeadNodeName),
            %{ok, {riak, Pid}};
            %Needed to change context format to be compatible with FMKe.
            {ok, Pid};
        Other -> {error, Other}
    end.

start_sup_link() ->
    case fmk_sup:start_link() of
       {ok, Pid} -> start_conn_pool(Pid);
       _Error -> _Error
    end.

start_conn_pool(Pid) ->
    RiakHostnames = get_app_var(db_conn_hostnames,["127.0.0.1"]),
    RiakPorts = get_app_var(db_conn_ports,["8087"]),
    {ok,_} = fmke_db_conn_pool:start([
        {hostnames, RiakHostnames},
        {ports, RiakPorts},
        {module, riakc_pb_socket}
    ]),
    {ok,Pid}.

set_app_vars(NodeAddresses,NodePorts) ->
    fmk_config:set(db_conn_module,riakc_pb_socket),
    set_app_var(db_conn_hostnames,"RIAK_ADDRESSES",NodeAddresses),
    set_app_var(db_conn_ports,"RIAK_PB_PORTS",NodePorts).

get_app_var(VarName,DefaultVal) ->
    fmk_config:get(VarName,DefaultVal).

set_app_var(VarName, EnvVarName, DefaultVal) ->
    EnvOrDefault = os:getenv(EnvVarName, DefaultVal),
    ParsedValue = parse_app_var(VarName,EnvOrDefault),
    fmk_config:set(VarName,ParsedValue),
    io:format("Set ~p application variable to value ~p~n",[VarName,ParsedValue]).

get_application_record(Key, _RecordType, Context) ->
  io:format("inside get_application_record(~p,~p,~p)",[Key,_RecordType,Context]),
  case get_key({_RecordType,Key},map,Context) of
      {{ok,[]},_Context1} -> {{error, not_found}, Context};
      {{ok,Object},_Context2} -> build_app_record(_RecordType,Object)
  end.

build_app_record(patient,Object) when is_list(Object) andalso (length(Object) >= 3)  ->
  Id = lists:nth(1,Object),
  Name = lists:nth(2,Object),
  Address = lists:nth(3,Object),
  % Prescriptions = read_nested_prescriptions(Object, ?PATIENT_PRESCRIPTIONS_KEY),
  #patient{id=Id,name=Name,address=Address}.%,prescriptions=Prescriptions};
% build_app_record(pharmacy,Object) ->
%   Id = find_key(Object, ?PHARMACY_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?PHARMACY_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?PHARMACY_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Prescriptions = read_nested_prescriptions(Object, ?PHARMACY_PRESCRIPTIONS_KEY),
%   #pharmacy{id=Id,name=Name,address=Address,prescriptions=Prescriptions};
% build_app_record(staff,Object) ->
%   Id = find_key(Object, ?STAFF_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?STAFF_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?STAFF_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Speciality = find_key(Object, ?STAFF_SPECIALITY_KEY, ?LWWREG, <<"undefined">>),
%   Prescriptions = read_nested_prescriptions(Object, ?STAFF_PRESCRIPTIONS_KEY),
%   #staff{id=Id,name=Name,address=Address,speciality=Speciality,prescriptions=Prescriptions};
% build_app_record(facility,Object) ->
%   Id = find_key(Object, ?FACILITY_ID_KEY, ?LWWREG, -1),
%   Name = find_key(Object, ?FACILITY_NAME_KEY, ?LWWREG, <<"undefined">>),
%   Address = find_key(Object, ?FACILITY_ADDRESS_KEY, ?LWWREG, <<"undefined">>),
%   Type = find_key(Object, ?FACILITY_TYPE_KEY, ?LWWREG, <<"undefined">>),
%   #facility{id=Id,name=Name,address=Address,type=Type};
% build_app_record(prescription,Object) ->
%   Id = find_key(Object, ?PRESCRIPTION_ID_KEY, ?LWWREG, -1),
%   PatientId = find_key(Object, ?PRESCRIPTION_PATIENT_ID_KEY, ?LWWREG, <<"undefined">>),
%   PrescriberId = find_key(Object, ?PRESCRIPTION_PRESCRIBER_ID_KEY, ?LWWREG, <<"undefined">>),
%   PharmacyId = find_key(Object, ?PRESCRIPTION_PHARMACY_ID_KEY, ?LWWREG, <<"undefined">>),
%   DatePrescribed = find_key(Object, ?PRESCRIPTION_DATE_PRESCRIBED_KEY, ?LWWREG, <<"undefined">>),
%   IsProcessed = find_key(Object, ?PRESCRIPTION_IS_PROCESSED_KEY, ?LWWREG, ?PRESCRIPTION_NOT_PROCESSED_VALUE),
%   DateProcessed = find_key(Object, ?PRESCRIPTION_DATE_PROCESSED_KEY, ?LWWREG, <<"undefined">>),
%   Drugs = find_key(Object, ?PRESCRIPTION_DRUGS_KEY, ?ORSET, []),
%   #prescription{
%       id=Id
%       ,patient_id=PatientId
%       ,pharmacy_id=PharmacyId
%       ,prescriber_id=PrescriberId
%       ,date_prescribed=DatePrescribed
%       ,date_processed=DateProcessed
%       ,drugs=Drugs
%       ,is_processed=IsProcessed
%   }.

%% exactly the same to antidote_kv_driver
parse_app_var(db_conn_hostnames,ListAddresses) when is_list(ListAddresses), (length(ListAddresses) > 0) ->
    [list_to_atom(X) || X <- ListAddresses];
parse_app_var(db_conn_hostnames,ListAddresses) ->
    [list_to_atom(X) || X <- parse_list_from_env_var(ListAddresses)];
parse_app_var(db_conn_ports,ListPorts) when is_list(ListPorts), length(ListPorts) > 0 ->
    case is_integer(lists:nth(1,ListPorts)) of
        true ->
            [X || X <- ListPorts];
        false -> [list_to_integer(X) || X <- ListPorts]
    end;
parse_app_var(db_conn_ports,ListPorts) ->
    parse_list_from_env_var(ListPorts).

parse_list_from_env_var(String) ->
    io:format("RECEIVED: ~p\n",[String]),
    try
      string:tokens(String,",") %% CSV style
    catch
      _:_  ->
        bad_input_format
    end.

stop(_Context = {_,Pid,_}) ->
    riakc_pb_socket:stop(Pid).

%% RPC to create bucket types in Riak.
%% This needs to be improved to avoid using quotation marks.
create_buckets(RiakNodeName) ->
    %CounterType = lists:flatten(["\"",atom_to_list(counter),"\""]),
    %SetType = lists:flatten(["\"",atom_to_list(set),"\""]),
    %%RegisterType = lists:flatten(["\"",atom_to_list(register),"\""]),
    MapType = lists:flatten(["\"",atom_to_list(map),"\""]),

    lists:foreach(fun(Type) ->
                          rpc:call(RiakNodeName, riak_kv_console, bucket_type_create,
                                   [[Type, "{\"props\":{ \"datatype\" : "++ Type ++ " }}" ]]),
                          rpc:call(RiakNodeName, riak_kv_console, bucket_type_activate, [[Type]])
                  end,
                  %[CounterType, SetType, RegisterType, MapType]).
                  [MapType]).

adapt_to_riak({Bucket, Key}, Type) ->
    TypeString = lists:flatten(["\"",atom_to_list(Type),"\""]),
    BinaryBucket = build_binary_bucket(Bucket),
    BinaryKey = Key,
    {{TypeString, BinaryBucket}, BinaryKey};
adapt_to_riak(Key,Type) ->
    adapt_to_riak({read_bucket(Key),Key},Type).

build_binary_bucket(Bucket) when is_list(Bucket) ->
    list_to_binary(Bucket);
build_binary_bucket(Bucket) when is_atom(Bucket) ->
    build_binary_bucket(atom_to_list(Bucket)).

get_key(Key, Type, Context = {riak_tx, Pid, RiakObjs}) ->
    {B, K} = RiakKey = adapt_to_riak(Key, Type),
    RiakType = get_type_driver(Type),
    case dict:find({RiakType, RiakKey}, RiakObjs) of
        {ok, CachedObj} -> {{ok, get_value(RiakType, CachedObj)},Context};
        _ ->
            FetchedObj = case riakc_pb_socket:fetch_type(Pid, B, K) of
                             {ok, Obj} -> Obj;
                             {error, _Reason} -> create_obj(RiakType, [])
                         end,
            UpdtRiakObjs = dict:store({RiakType, RiakKey}, FetchedObj, RiakObjs),
            {{ok, get_value(RiakType, FetchedObj)}, {riak_tx, Pid, UpdtRiakObjs}}
    end.

get_map(Key, Context) ->
    get_key(Key, map, Context).

read_bucket(Key) when is_binary(Key) ->
    read_bucket(binary_to_list(Key));
read_bucket(Key) when is_list(Key) ->
    Entities = ["facility","patient","pharmacy","prescription","staff","treatment","event"],
    SearchResults = lists:map(fun(Entity) -> string:str(Key,Entity) end,Entities),
    case return_positive(SearchResults) of
        0 -> "undefined";
        Position -> lists:nth(Position,Entities)
    end.

return_positive(List) when is_list(List) ->
    return_positive_rec(1,List).

return_positive_rec(CurrPos,[H|T]) when H =:= 0 ->
    return_positive_rec(CurrPos+1,T);
return_positive_rec(CurrPos,[H|_]) when H > 0 ->
    CurrPos;
return_positive_rec(_CurrPos,[]) ->
    0.

update_map(Key, Operation, Context0 = {riak_tx, Pid, _RiakObjs0}) ->
    StringKey = binary_to_list(Key),
    RiakBucket = read_bucket(StringKey),
    RiakKey = adapt_to_riak({RiakBucket,Key}, map),
    RiakType = get_type_driver(map),

    {{ok, _}, {_,_,RiakObjs1}} = get_key(Key, map, Context0),
    CachedRiakObj = dict:fetch({RiakType, RiakKey}, RiakObjs1),
    UpdtObj = lists:foldl(fun(Op, Map0)->
                                  process_op(Op, Map0)
                          end, CachedRiakObj, Operation),
    UpdtRiakObjs = dict:store({RiakType, RiakKey}, UpdtObj, RiakObjs1),
    {ok, {riak_tx, Pid, UpdtRiakObjs}}.

process_op({update_map, Key, Value},Parent) ->
    riakc_map:update({Key, map}, fun(Child) ->
                                         process_op(Value, Child) end, Parent);

process_op({create_map, Key, Value},Parent) ->
    riakc_map:update({Key, map}, fun(Child) ->
                                         process_op(Value, Child) end, Parent);

process_op({create_set, Key, Values}, Parent) ->
    lists:foldl(fun(Value, Map) ->
                        riakc_map:update({Key, set},
                                         fun(Set) ->
                                                 riakc_set:add_element(Value, Set)
                                         end, Map) end,
                Parent, Values);

process_op({create_register, Key, Value}, Parent) ->
    riakc_map:update({Key, register}, fun(Register) ->
                                              riakc_register:set(Value, Register)
                                      end, Parent).

%get_counter(Key, Context = {riak_tx, _Pid, _RiakObjs}) ->
%    get_key(Key, counter, Context).

%inc_counter(Key, Amount, Context = {riak_tx, _Pid, _RiakObjs}) ->
%   updt_counter(Key, increment, Amount, Context).

%dec_counter(Key, Amount, Context = {riak_tx, _Pid, _RiakObjs}) ->
%   updt_counter(Key, decrement, Amount, Context).

%updt_counter(Key, Operation, Amount, Context0 = {riak_tx, Pid, _RiakObjs0}) ->
%    RiakKey = adapt_to_riak(Key, counter),
%    RiakType = get_type_driver(counter),
%    {{ok, _}, {_,_,RiakObjs1}} = get_key(Key, counter, Context0),
%    CachedRiakObj = dict:fetch({RiakType, RiakKey}, RiakObjs1),
%    UpdtObj = execute_local_op({RiakType, Operation, [Amount]}, CachedRiakObj),
%    UpdtRiakObjs = dict:store({RiakType, RiakKey}, UpdtObj, RiakObjs1),
%    {ok, get_value(RiakType, UpdtObj), {riak_tx, Pid, UpdtRiakObjs}}.
%
%%Can use this with any riakc data type.
%execute_local_op(_Operation = {RiakType, OpName, Params}, Object) ->
%    erlang:apply(RiakType, OpName, lists:append(Params, [Object])).

start_transaction({riak_tx, _,_}) ->
    erlang:error(transaction_already_started);

%start_transaction({riak, Pid} = _Context) ->
%    {ok, {riak_tx, Pid, dict:new()}};

start_transaction(_OldContext) ->
    Pid = poolboy:checkout(fmke_fmke_db_conn_pool),
    {ok, {riak_tx, Pid, dict:new()}}.

%start_transaction(_Context) ->
%    erlang:error(not_implemented).

commit_transaction(_Context = {riak_tx, Pid, RiakObjs}) ->
    dict:fold(fun({RiakType, {B, K}}, Object, _) ->
                      riakc_pb_socket:update_type(Pid, B, K, RiakType:to_op(Object))
              end, nil, RiakObjs),
    poolboy:checkin(fmke_fmke_db_conn_pool, Pid),
    {ok, Pid}.

create_obj(RiakType, []) ->
    erlang:apply(RiakType, new, []).

get_value(RiakType, Obj) ->
    RiakType:value(Obj).

get_type_driver(map) -> ?CRDT_MAP.

%get_type_driver(counter) -> ?CRDT_COUNTER;

%get_type_driver(set) -> ?CRDT_SET;

%get_type_driver(register) -> ?CRDT_REGISTER.
