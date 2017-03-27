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
-module(redis_kv_driver).
-include("fmk.hrl").

-behaviour(gen_kv_driver).

-export([init/1,
         start_transaction/1,
         commit_transaction/1,
         update_map/3,
         get_map/2,
         find_key/4,
         stop/1
        ]).

init([Host, Port]) ->
    {ok, Connection} = eredis:start_link(Host, Port),
    {ok, {connection, Connection}}.

stop({connection, C}) ->
  eredis:stop(C).

%% Transactions %% Dummy transactions; Redis doesnot support transactions in a
%% cluster setup
start_transaction({connection, Connection}) ->
    {ok, {connection, Connection}}.

commit_transaction(Context) ->
    {ok, Context}.

%% Get
%% Redis does not support map that holds keys of different types,
%% So just return a dummy object and use find_key to get specific key-value pairs.
get_map(Key, Context) ->
    {ok, {map, Key, Context}, Context}.

find_key({map, Map}, Key, register, Context = {connection, C}) ->
    Query = ["HGET", Map, Key],
    case execute_get(C, Query) of
      {ok, Res} -> {ok, Res, Context};
      {error, Reason} -> {error, Reason, Context}
    end;
find_key({map, Map}, Key, map, Context) ->
    InnerKey = inner_map_key(Map, Key),
    {ok, {map, InnerKey}, Context};
find_key({map, Map}, Key, set, Context = {connection, C}) ->
    InnerKey = inner_map_key(Map, Key),
    Query = ["SMEMBERS", InnerKey],
    case execute_get(C, Query) of
        {ok, Res} -> {ok, Res, Context};
        {error, Reason} -> {error, Reason, Context}
    end.

%% Updates
update_map(Key, ListOfOps, Context = {connection, C}) ->
  try  lists:foreach(fun(Op) ->
                        case update_nested_object(Key, Op, C) of
                            ok -> ok;
                            {error, Reason} -> throw(erlang:error(Reason))
                        end
                     end, ListOfOps) of
        ok -> {ok, Context}
  catch
    _:Reason -> {error, Reason, Context}
  end.


update_nested_object(Map, {create_register, Key, Value}, Connection) ->
    Op = ["HSET", Map, Key, Value],
    execute_op(Connection, Op);

update_nested_object(Map, {create_set, Key, Elements}, Connection) ->
    InnerKey = inner_map_key(Map, Key),
    Op = ["SADD", InnerKey] ++ Elements,
    execute_op(Connection, Op);

update_nested_object(Map, {create_map, Key, NestedOps}, Connection) ->
    InnerKey = inner_map_key(Map, Key),
    update_nested_object(InnerKey, NestedOps, Connection);

update_nested_object(Map, {update_map, Key, NestedOps}, Connection) ->
   InnerKey = inner_map_key(Map, Key),
   update_nested_object(InnerKey, NestedOps, Connection).

inner_map_key(Map, Key) ->
    list_to_binary(binary_to_list(Map) ++ binary_to_list(Key)).

execute_op(Connection, Op) ->
    case eredis:q(Connection, Op) of
      {ok, _} -> ok;
      {error, Reason} -> {error, Reason};
      Other -> {error, Other}
    end.

execute_get(Connection, Op) ->
   case eredis:q(Connection, Op) of
       {ok, Res} -> {ok, Res};
       {error, Reason} -> {error, Reason};
       Other -> {error, Other}
   end.
