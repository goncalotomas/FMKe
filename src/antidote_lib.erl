%% A module to interact with the Antidote platform
%% https://github.com/SyncFree/antidote/
%% This module should be easily integrated in your OTP application.
%% If you want to use this module in your application, you need an Erlang Header file (.hrl)
%% with the following macros:
%% ANTIDOTE: Node where Antidote is running. Usually 'antidote@127.0.0.1'
%% TODO add operations to increment/decrement counters
-module(antidote_lib).
-include("fmk.hrl").

%% These exports supply managed transactions, to make it easier to work with Antidote. Use these
%% if you don't need fine grained control over transactions.
-export ([
  create_bucket/2,
  get/2,
  put/4,
  put/5
  ]).

%% This export is internally used by the put and get functions. These functions are exported in order
%% to provide finer-grained transactional support.
-export ([
    txn_start/0,
    txn_start/1,
    txn_read_object/2,
    txn_read_objects/2,
    txn_update_map/5,
    txn_update_object/2,
    txn_update_objects/2,
    txn_commit/1
  ]).

%% These are utility functions, most of them related to updating maps, which can be nested and
%% therefore are a little bit harder to handle than other CRDTs
-export ([
  build_map_update/1,
  build_map_op/3,
  find_key/3,
  counter_increment/1,
  counter_decrement/1,
  lwwreg_assign/1
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
  {update, OpList}.

build_map_op(Key,Type,Op) ->
  {update, {Key,Type}, Op}.

txn_update_map(Bucket,Op,ListOps,TxnDetails,Actor)->
  txn_update_object({Bucket,Op,{ListOps,Actor}},TxnDetails).

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

put(Key,Type,Op,Param) ->
  TxnDetails = txn_start(),
  Bucket = create_bucket(Key,Type),
  ok = txn_update_object({Bucket,Op,Param},TxnDetails),
  ok = txn_commit(TxnDetails).

put(Key,Type,Op,Param,Actor) ->
  TxnDetails = txn_start(),
  Bucket = create_bucket(Key,Type),
  ok = txn_update_map(Bucket,Op,Param,TxnDetails,Actor),
  ok = txn_commit(TxnDetails).

%% ------------------------------------------------------------------------------------------------
%% ANTIDOTE'S OLD API - Should only be used for benchmarking
%% ------------------------------------------------------------------------------------------------
write_to_antidote(Key,Type,Params) ->
  rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
  rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).

%% ------------------------------------------------------------------------------------------------
%% CRDT operations: because CRDT interfaces may change over time...?
%% ------------------------------------------------------------------------------------------------
counter_increment(Amount) ->
  {increment,Amount}.

counter_decrement(Amount) ->
  {decrement,Amount}.

lwwreg_assign(Value) ->
  {assign,Value}.