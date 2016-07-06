%% A module to interact with the Antidote platform
%% https://github.com/SyncFree/antidote/
%% This module should be easily integrated in your OTP application.
%% If you want to use this module in your application, you need an Erlang Header file (.hrl)
%% with the following macros:
%% ANTIDOTE: Node where Antidote is running. Usually 'antidote@127.0.0.1'
-module(antidote_lib).
-include("fmk.hrl").

%% These exports supply managed transactions, to make it easier to work with Antidote. Use these
%% if you don't need fine grained control over transactions.
-export ([
  create_bucket/2,
  get/2,
  put/3,
  put/4,
  txn_start/0,
  txn_start/1,
  txn_read_object/2,
  txn_read_objects/2,
  txn_update_map/4,
  txn_update_object/2,
  txn_update_objects/2,
  txn_commit/1
  ]).

%% These are utility functions, most of them related to updating maps, which can be nested and
%% therefore are a little bit harder to handle than other CRDTs
-export ([
  build_map_update/1,
  build_map_op/3,
  find_key/3
  ]).

%% Old API exports, these functions should only be used for benchmarking
-export ([
  write_to_antidote/3,
  read_from_antidote/2
  ]).



%% ------------------------------------------------------------------------------------------------
%% Antidote's transaction API wrapper - Use when you need fine grain control over transactions
%% ------------------------------------------------------------------------------------------------
txn_start() ->
  {ok,TxnDetails} = rpc:call(?ANTIDOTE,antidote,start_transaction,[ignore,[]]),
  TxnDetails.

txn_start(TimeStamp) -> 
  {ok,TxnDetails} = rpc:call(?ANTIDOTE,antidote,start_transaction,[TimeStamp,[]]),
  TxnDetails.

txn_read_object(Object,TxnDetails) ->
  {ok,[Value]} = rpc:call(?ANTIDOTE,antidote,read_objects,[[Object],TxnDetails]),
  Value.

txn_read_objects(Objects,TxnDetails) ->
  {ok,Values} = rpc:call(?ANTIDOTE,antidote,read_objects,[Objects,TxnDetails]),
  Values.

txn_update_object(ObjectUpdate,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[[ObjectUpdate],TxnDetails]).

txn_update_objects(ObjectUpdates,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[ObjectUpdates,TxnDetails]).

txn_commit(TxnDetails) ->
  {ok,_CommitTime} = rpc:call(?ANTIDOTE,antidote,commit_transaction,[TxnDetails]),
  ok.

%% ------------------------------------------------------------------------------------------------
%% Helper functions to assist in map updates
%% ------------------------------------------------------------------------------------------------
build_map_update(OpList) ->
  %[{update,{key,riak_dt_lwwreg},{assign, <<"A">>}},{update,{val,riak_dt_pncounter},{increment,4}}].
  {update, OpList}.

build_map_op(Key,Type,Op) ->
  {update, {Key,Type}, Op}.

txn_update_map(Bucket,ListOps,TxnDetails,Actor)->
  txn_update_object({Bucket,update,{ListOps,Actor}},TxnDetails).

% build_map_update(Key,KeyType,KeyUpdate,ValueType,ValueUpdate)
%   ->
%   [{update,{Key,KeyType},KeyUpdate},{update,{val,ValueType},ValueUpdate}].

%% Searches for a Value within a map that is associated with a specific key.
%% All riak_dt_map entries are of type {{key_name,key_type},Value}. Having this function avoids repeating
%% the following code numerous times when searching for an element within a map.
find_key(Map, Key, KeyType) ->
  case lists:keyfind({Key,KeyType},1,Map) of
    false -> not_found;
    {{Key,KeyType},Value} -> Value
  end.

%% ------------------------------------------------------------------------------------------------
%% Simple API - Recommended way to interact with Antidote
%% ------------------------------------------------------------------------------------------------
create_bucket(Key,Type) ->
  {Key,Type,bucket}.

get(Key,Type) ->
  Bucket = create_bucket(Key,Type),
  TxnDetails = txn_start(),
  ReadResult = txn_read_object(Bucket,TxnDetails),
  ok = txn_commit(TxnDetails),
  case ReadResult of
    [] -> [];
    Value -> Value
  end.

put(Key,Type,UpdateOp) ->
  TxnDetails = txn_start(),
  Bucket = create_bucket(Key,Type),
  ok = txn_update_object({Bucket,UpdateOp},TxnDetails),
  ok = txn_commit(TxnDetails).

put(Key,Type,UpdateOp,Actor) ->
  TxnDetails = txn_start(),
  Bucket = create_bucket(Key,Type),
  io:format("~p", [[UpdateOp]]),
  ok = txn_update_map(Bucket,[UpdateOp],TxnDetails,Actor),
  ok = txn_commit(TxnDetails).

%% ------------------------------------------------------------------------------------------------
%% ANTIDOTE'S OLD API - Should only be used for benchmarking
%% ------------------------------------------------------------------------------------------------
write_to_antidote(Key,Type,Params) ->
  rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
  rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).