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
-module(antidote_kv_driver).
-include("fmk.hrl").
-include("fmk_kv.hrl").
-include("fmke_antidote.hrl").
-author("goncalotomas").

-behaviour(gen_kv_driver).

%% gen_kv_driver exports
-export([
  init/1,
  stop/1,
  get_key/3,
  get_map/2,
  get_list_of_keys/3,
  put/4,
  start_transaction/1,
  commit_transaction/1,
  update_map/3,
  find_key/3
]).

%% -------------------------------------------------------------------
%% Antidote Context Definition
%% -------------------------------------------------------------------
-record(antidote_context, {
    pid :: pid(),
    txn_id :: txid()
}).

init(_Anything) ->
  Result = case fmk_sup:start_link() of
             {ok, Pid} ->
               case open_antidote_socket() of
                 {ok,_} -> {ok, Pid};
                 Err -> {error, {cannot_open_protobuff_socket, Err}}
               end;
             {error, Reason} ->
               {error, Reason}
           end,
  Result.

stop(_Anything) ->
  close_antidote_socket().

get_map(Key,Context) ->
  get_key(Key,?MAP,Context).

%% Returns status {({ok, object}| {error, reason}), context}
get_key(Key, GeneralKeyType, Context = #antidote_context{pid = Pid, txn_id = TxnId}) ->
  KeyType = convert_key_type(GeneralKeyType),
  Object = create_read_bucket(Key,KeyType),
  {ok, [Value]} = antidotec_pb:read_values(Pid, [Object], TxnId),
  case Value of
    {_Something,[]} -> {{error, not_found}, Context};
    {map, MapObject} -> {{ok, MapObject}, Context}
  end.

%% Returns status {({ok, list(object)} | {error, reason}), context}
get_list_of_keys(ListKeys, ListKeyTypes, Context = #antidote_context{pid = Pid, txn_id = TxnId}) ->
  ListObjects = (lists:foldl(
    fun(Key,KeyType) -> create_read_bucket(Key,KeyType) end,
    ListKeys,ListKeyTypes)
  ),
  {ok, Values} = antidotec_pb:read_values(Pid, ListObjects, TxnId),
  {{ok, Values}, Context}.

%% Return status {(ok | error), context}
put(Key, KeyType, Value, Context = #antidote_context{pid = Pid, txn_id = TxnId}) ->
  Object = create_write_bucket(Key,KeyType,Value),

  Result = antidotec_pb:update_objects(Pid, [Object], TxnId),
  {Result, Context}.

%% Return status {(ok | error), context}
start_transaction(_OldContext) ->
  Pid = poolboy:checkout(antidote_connection_pool),
  {ok, TxnId} = antidotec_pb:start_transaction(Pid, ignore, {}),
  {ok, #antidote_context{pid=Pid, txn_id = TxnId}}.

%% Return status {(ok | error), context}
commit_transaction(#antidote_context{pid = Pid, txn_id = TransactionId}) ->
  {ok, _CommitTime} = antidotec_pb:commit_transaction(Pid, TransactionId),
  Result = poolboy:checkin(antidote_connection_pool, Pid),
  {Result, #antidote_context{pid=Pid}}.

%% Creates an Antidote bucket of a certain type.
-spec create_write_bucket(field(), crdt(), term()) -> object_bucket().
create_write_bucket(Key,?MAP,Value) ->
  Bucket = create_read_bucket(Key,?MAP),
  {Bucket, update, Value}.

%% Creates an Antidote bucket of a certain type.
-spec create_read_bucket(field(), crdt()) -> object_bucket().
create_read_bucket(Key,Type) ->
  {Key,Type,<<"bucket">>}.

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
lwwreg_assign_op(Value) when is_binary(Value) ->
  {assign, Value};
lwwreg_assign_op(Value) when is_list(Value) ->
  {assign, list_to_binary(Value)};
lwwreg_assign_op(Value) when is_integer(Value) ->
  {assign, integer_to_binary(Value)}.

build_lwwreg_op(Key,Value) ->
  build_map_op(Key,?LWWREG,lwwreg_assign_op(Value)).

build_add_set_op(Key, Elements) ->
  build_map_op(Key,?ORSET,add_to_set_op(Elements)).

add_to_set_op(Elements) when is_list(Elements) ->
  {add_all, [list_to_binary(X) || X <- Elements]}.

build_map_op(Key,Type,Op) ->
  {{Key,Type}, Op}.

update_map(Key,ListOps,Context) ->
  put(Key,?MAP,inner_update_map(ListOps),Context).

inner_update_map([]) ->
  [];
inner_update_map([H|T]) ->
  HeadOp = case H of
       %% These cases are for direct children of maps
       {create_register, Key, Value} -> build_lwwreg_op(Key,Value);
       {create_set, Key, Elements} -> build_add_set_op(Key,Elements);
       %% When nested fields are necessary
       {create_map, Key, NestedOps} -> build_update_map_bucket_op(Key,inner_update_map(NestedOps));
       {update_map, Key, NestedOps} -> build_update_map_bucket_op(Key,inner_update_map(NestedOps))
  end,
  [HeadOp] ++ inner_update_map(T).

find_key(Map, Key, GeneralKeyType) ->
  KeyType = convert_key_type(GeneralKeyType),
  find_key(Map,Key,KeyType,not_found).

find_key(Map, Key, KeyType, FallbackValue) ->
  case lists:keyfind({Key,KeyType},1,Map) of
    false -> FallbackValue;
    {{Key,KeyType},Value} -> Value
  end.

build_map_update_op(NestedOps) ->
  {update, NestedOps}.

build_update_map_bucket_op(Key, NestedOps) ->
  build_map_op(Key,?MAP,build_map_update_op(NestedOps)).

open_antidote_socket() ->
  set_application_variable(antidote_address,"ANTIDOTE_ADDRESS",?DEFAULT_ANTIDOTE_ADDRESS),
  set_application_variable(antidote_port,"ANTIDOTE_PB_PORT",?DEFAULT_ANTIDOTE_PORT),
  AntidoteNodeAddress = fmk_config:get_env(?VAR_ANTIDOTE_PB_ADDRESS,?DEFAULT_ANTIDOTE_ADDRESS),
  AntidoteNodePort = fmk_config:get_env(?VAR_ANTIDOTE_PB_PORT,?DEFAULT_ANTIDOTE_PORT),
  antidote_pool:start([{hostname, AntidoteNodeAddress}, {port, AntidoteNodePort}]).

set_application_variable(antidote_address, "ANTIDOTE_ADDRESS", ?DEFAULT_ANTIDOTE_ADDRESS) ->
  %% try to load value from environment variable
  Default = os:getenv("ANTIDOTE_ADDRESS", ?DEFAULT_ANTIDOTE_ADDRESS),
  ListAddresses = [list_to_atom(X) || X <- parse_list_from_env_var(Default)],
  Value = application:get_env(?APP,antidote_address,ListAddresses),
  fmk_config:set(antidote_address,Value),
  Value;
set_application_variable(antidote_port, "ANTIDOTE_PB_PORT", ?DEFAULT_ANTIDOTE_PORT) ->
  %% try to load value from environment variable
  Default = os:getenv("ANTIDOTE_PB_PORT", ?DEFAULT_ANTIDOTE_PORT),
  ListPorts = parse_list_from_env_var(Default),
  Value = application:get_env(?APP,antidote_port,ListPorts),
  fmk_config:set(antidote_port,Value),
  Value.

close_antidote_socket() ->
  AntidotePbPid = fmk_config:get(?VAR_ANTIDOTE_PB_PID,undefined),
  case AntidotePbPid of
    undefined ->
      {error, error_closing_pb_socket};
    _SomethingElse ->
      ok
  end.

parse_list_from_env_var(String) ->
  io:format("RECEIVED: ~p\n",[String]),
  try
    string:tokens(String,",") %% CSV style
  catch
    _:_  ->
      bad_input_format
  end.

convert_key_type(register) ->
  ?LWWREG;
convert_key_type(map) ->
  ?MAP.