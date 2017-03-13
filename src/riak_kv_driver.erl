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
  get_key/3, %% Returns status {({ok, object}| {error, reason}), context}
  get_list_of_keys/3, %% Returns status {({ok, list(object)} | {error, reason}), context}
  put/4, %% Return status {(ok | error), context}
  start_transaction/1, %% Return status {(ok | error), context}
  commit_transaction/2, %% Return status {(ok | error), context} <-- TRANSACTION_ID MUST BE REMOVED FROM INTERFACE
  execute_local_op/3, %% Return {obj}
  create_obj/2 %% Return {obj}
]).

%% Context is a tuple {riak, Pid}.
%% Key is a tuple {<<"BUCKET">>, <<"KEY">>}.


%% RPC to create bucket types in Riak.
%% This needs to be improved to avoid using quotation marks.
create_buckets(RiakNodeName) ->
    CounterType = lists:flatten(["\"",atom_to_list(counter),"\""]),
    MapType = lists:flatten(["\"",atom_to_list(map),"\""]),
    SetType = lists:flatten(["\"",atom_to_list(set),"\""]),
    lists:foreach(fun(Type) ->
                          rpc:call(RiakNodeName, riak_kv_console, bucket_type_create,
                                   [[Type, "{\"props\":{ \"datatype\" : \"counter\"}}" ]]),
                          rpc:call(RiakNodeName, riak_kv_console, bucket_type_activate, [[Type]])
                  end, [CounterType,MapType,SetType]).

adapt_to_riak({Bucket, Key}, Type) ->
    TypeString = lists:flatten(["\"",atom_to_list(Type),"\""]),
    BucketBinary = list_to_binary(Bucket),
    KeyBinary = list_to_binary(Key),
    TypeBinary = list_to_binary(TypeString),
    {TypeBinary, BucketBinary, KeyBinary};

adapt_to_riak(Bucket, Type) ->
    TypeString = lists:flatten(["\"",atom_to_list(Type),"\""]),
    BucketBinary = list_to_binary(Bucket),
    TypeBinary = list_to_binary(TypeString),
    {TypeBinary, BucketBinary}.

%%ATTENTION: Does not manage multiple connections
init({[NodeAddress | _OtherNA] , [Port | _OP], HeadNodeName, HeadNodeCookie}) ->
    erlang:set_cookie(HeadNodeName, HeadNodeCookie),
    case riakc_pb_socket:start_link(NodeAddress, Port) of
        {ok, Pid} ->
            create_buckets(HeadNodeName),
            {riak, Pid};
        Other -> {error, Other}
    end.

get_key(Key, Type, Context = {riak, Pid}) ->
    {T, B, K} = adapt_to_riak(Key, Type),
    case riakc_pb_socket:fetch_type(Pid, {T, B}, K) of
        {ok, Obj} ->
            {{ok, Obj}, Context};
        {error, Reason} -> {{error, Reason}, Context}
    end.

%Riak does not support this, do we really need it?
get_list_of_keys(Table, TableType, {riak, Pid} = Context) ->
    {T, B} = adapt_to_riak(Table, TableType),
    Result = riakc_pb_socket:list_keys(Pid, {T, B}),
    {Result, Context}.

put(Key, Type, Object, {riak, Pid} = Context) ->
    {T, B, K} = adapt_to_riak(Key, Type),
    RiakType = get_type_driver(Type),
    case riakc_pb_socket:update_type(Pid, {T, B}, K, RiakType:to_op(Object))  of
        ok -> {ok, Context};
        {error, Reason} -> {{error, Reason}, Context}
    end.

start_transaction(Context) ->
    {ok, Context}.
commit_transaction(Context, _TransactionId) ->
    {ok,Context}.

execute_local_op(_Operation = {Type, OpName, Params}, Object, _Context) ->
    erlang:apply(get_type_driver(Type), OpName, lists:append(Params, [Object])).

create_obj(Type, []) ->
    erlang:apply(get_type_driver(Type), new, []).

%% Add more data types if necessary.
get_type_driver(counter) -> riakc_counter;

get_type_driver(set) -> riakc_set;

get_type_driver(map) -> riakc_map.

