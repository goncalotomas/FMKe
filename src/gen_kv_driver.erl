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
-type context() :: term().
-type reason() :: term().

%% callbacks
-callback init(term()) -> {ok, context()}. %TODO: precise typespec

%% Transactions
-callback start_transaction(context()) -> {ok, context()}.
-callback commit_transaction(context()) -> {ok, context()}.

%% Types
-callback get_counter(key(), context()) -> {ok, integer(), context()} | {error, reason()}.
-callback inc_counter(key(), integer(), context()) -> {ok, integer(), context()}.
-callback dec_counter(key(), integer(), context()) -> {ok, integer(), context()}.
