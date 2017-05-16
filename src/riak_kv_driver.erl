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
-module(riak_kv_driver).
-include("fmk.hrl").

-behaviour(gen_kv_driver).

%% API
-export([
         init/1,
         stop/1,
         start_transaction/1,
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
    riakc_pb_socket:start_link(NodeAddress, Port);

%%ATTENTION: Does not manage multiple connections.
init({[NodeAddress | _OtherNA] , [Port | _OP], HeadNodeName, HeadNodeCookie}) ->
    erlang:set_cookie(HeadNodeName, HeadNodeCookie),
    case riakc_pb_socket:start_link(NodeAddress, Port) of
        {ok, Pid} ->
            create_buckets(HeadNodeName),
            %{ok, {riak, Pid}};
            %Needed to change context format to be compatible with FMKe.
            {ok, Pid};
        Other -> {error, Other}
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
    BucketBinary = list_to_binary(Bucket),
    KeyBinary = list_to_binary(Key),
    {{TypeString, BucketBinary}, KeyBinary}.

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
            {ok, get_value(RiakType, FetchedObj), {riak_tx, Pid, UpdtRiakObjs}}
    end.

get_map(Key, Context) ->
    get_key(Key, map, Context).

update_map(Key, Operation, Context0 = {riak_tx, Pid, _RiakObjs0}) ->
    RiakKey = adapt_to_riak(Key, map),
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

start_transaction(Pid) ->
    {ok, {riak_tx, Pid, dict:new()}}.

%start_transaction(_Context) ->
%    erlang:error(not_implemented).

commit_transaction(_Context = {riak_tx, Pid, RiakObjs}) ->
    dict:fold(fun({RiakType, {B, K}}, Object, _) ->
                      riakc_pb_socket:update_type(Pid, B, K, RiakType:to_op(Object))
              end, nil, RiakObjs),
    {ok, Pid}.

create_obj(RiakType, []) ->
    erlang:apply(RiakType, new, []).

get_value(RiakType, Obj) ->
    RiakType:value(Obj).

get_type_driver(map) -> ?CRDT_MAP.

%get_type_driver(counter) -> ?CRDT_COUNTER;

%get_type_driver(set) -> ?CRDT_SET;

%get_type_driver(register) -> ?CRDT_REGISTER.

