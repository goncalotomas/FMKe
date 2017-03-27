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

-module(gen_kv_driver).

%% Types TODO: refine type defs
-type key() :: term().
-type context() :: term(). %% specific to each driver
-type reason() :: term().
-type map_update() :: [nested_object_update()].
-type nested_object_update() :: nested_register_update() | nested_set_update() | nested_map_update().
-type nested_register_update() :: {creat_register, key(), term()}.
-type nested_set_update() :: {create_set, key(), [term()]}.
-type nested_map_update() :: {create_map, key(), map_update()} | {update_map, key(), map_update()}.
-type map_object() :: term(). %% specific to each driver
-type nested_key_type() :: register | set | map.

%% callbacks
-callback init(term()) -> {ok, context()}. %TODO: precise typespec
-callback stop(term()) -> term().

%% Transactions
-callback start_transaction(context()) -> {ok, context()}.
-callback commit_transaction(context()) -> {ok, context()}.

%% Returns a map object.
-callback get_map(key(), context()) -> {ok, map_object(), context()} | {error, reason()}.

%% Returns the value of a key inside a map
-callback find_key(map_object(), key(), nested_key_type(), context()) -> {ok, term(), context()} | {error, reason()}.

%% term() is a list of lists of operations where in each position you store the operations for each level of nesting []
%% [[{update, [{update,{key,mykey},{value,myvalue}]}, [], []] means that we will perform an operation on the top level map
%% and none in the lower levels. On the other side,
%% [[], [], [{other_update_op, something_else}]] means that we will only add
-callback update_map(key(), map_update(), context()) -> {ok, context()} | {error, reason(), context()}.
