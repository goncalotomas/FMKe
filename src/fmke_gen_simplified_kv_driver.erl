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

-module(fmke_gen_simplified_kv_driver).
-include ("fmke.hrl").

%% Types TODO: refine type defs
-type context() :: term(). %% specific to each driver
-type map_update() :: [nested_object_update()].
-type nested_object_update() :: nested_register_update() | nested_set_update() | nested_map_update().
-type nested_register_update() :: {creat_register, key(), term()}.
-type nested_set_update() :: {create_set, key(), [term()]}.
-type nested_map_update() :: {create_map, key(), map_update()} | {update_map, key(), map_update()}.


%% callbacks
-callback start(term()) -> {ok, context()} | {error, reason()}. %TODO: precise typespec
-callback stop(term()) -> term().

%% Transactions
-callback start_transaction(context()) -> {ok, context()}.
-callback commit_transaction(context()) -> {ok, context()}.

%% Returns a map object.
-callback get(key(), entity(), context()) -> {ok, app_record(), context()} | {error, reason()}.

%% term() is a list of lists of operations where in each position you store the operations for each level of nesting []
%% [[{update, [{update,{key,mykey},{value,myvalue}]}, [], []] means that we will perform an operation on the top level
%% map and none in the lower levels. On the other side,
%% [[], [], [{other_update_op, something_else}]] means that we will only add
-callback put(key(), entity(), map_update(), context()) -> {ok, context()} | {error, reason(), context()}.
