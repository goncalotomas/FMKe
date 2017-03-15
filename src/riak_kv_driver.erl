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
  %get_list_of_keys/3, %% Returns status {({ok, list(object)} | {error, reason}), context}
  %put/4, %% Return status {(ok | error), context}
  start_transaction/1, %% Return status {(ok | error), context}
  commit_transaction/1 %% Return status {(ok | error), context}
  %execute_local_op/3, %% Return {obj}
  %create_obj/2 %% Return {obj}
]).

-export([
    get_counter/2, %{({ok, integer()} | error), context}.
    inc_counter/3, %{({ok, integer()} | error), context}.
    dec_counter/3 %{({ok, integer()} | error), context}.
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
    {{TypeString, BucketBinary}, KeyBinary}.

%adapt_to_riak(Bucket, Type) ->
%    TypeString = lists:flatten(["\"",atom_to_list(Type),"\""]),
%    BucketBinary = list_to_binary(Bucket),
%    TypeBinary = list_to_binary(TypeString),
%    {TypeBinary, BucketBinary}.

%%ATTENTION: Does not manage multiple connections
init({[NodeAddress | _OtherNA] , [Port | _OP], HeadNodeName, HeadNodeCookie}) ->
    erlang:set_cookie(HeadNodeName, HeadNodeCookie),
    case riakc_pb_socket:start_link(NodeAddress, Port) of
        {ok, Pid} ->
            create_buckets(HeadNodeName),
            {ok, {riak, Pid}};
        Other -> {error, Other}
    end.

%NEED TO TEST IF DICTIONARY SUPPORTS THE KEY USED.
%NEED TO CHECK IF CACHE IS WORKING PROPERLY.
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

get_counter(Key, Context = {riak_tx, _Pid, _RiakObjs}) ->
    get_key(Key, counter, Context).

inc_counter(Key, Amount, Context = {riak_tx, _Pid, _RiakObjs}) ->
   updt_counter(Key, increment, Amount, Context).

dec_counter(Key, Amount, Context = {riak_tx, _Pid, _RiakObjs}) ->
   updt_counter(Key, decrement, Amount, Context).

updt_counter(Key, Operation, Amount, Context0 = {riak_tx, Pid, _RiakObjs0}) ->
    RiakKey = adapt_to_riak(Key, counter),
    RiakType = get_type_driver(counter),

    {{ok, _}, {_,_,RiakObjs1}} = get_key(Key, counter, Context0),
    CachedRiakObj = dict:fetch({RiakType, RiakKey}, RiakObjs1),
    UpdtObj = execute_local_op({RiakType, Operation, [Amount]}, CachedRiakObj),
    UpdtRiakObjs = dict:store({RiakType, RiakKey}, UpdtObj, RiakObjs1),
    {ok, get_value(RiakType, UpdtObj), {riak_tx, Pid, UpdtRiakObjs}}.

start_transaction({riak, Pid} = _Context) ->
    {ok, {riak_tx, Pid, dict:new()}};

start_transaction({riak_tx, _,_}) ->
    erlang:error(transaction_already_started);

start_transaction(_Context) ->
    erlang:error(not_implemented).

commit_transaction(_Context = {riak_tx, Pid, RiakObjs}) ->
    dict:fold(fun({RiakType, {B, K}}, Object, _) ->
                         riakc_pb_socket:update_type(Pid, B, K, RiakType:to_op(Object))
                 end, nil, RiakObjs),
    {ok, {riak, Pid}}.

execute_local_op(_Operation = {RiakType, OpName, Params}, Object) ->
    erlang:apply(RiakType, OpName, lists:append(Params, [Object])).

create_obj(RiakType, []) ->
    erlang:apply(RiakType, new, []).

get_value(RiakType, Obj) ->
    RiakType:value(Obj).

%% Add more data types if necessary.
get_type_driver(counter) -> riakc_counter;

get_type_driver(set) -> riakc_set;

get_type_driver(map) -> riakc_map.
