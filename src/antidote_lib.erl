%% An erlang module to interact with the Antidote key value store
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
  get/3,
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
  build_nested_map_op/4,
  find_key/3,
  counter_increment/1,
  counter_decrement/1,
  lwwreg_assign/1,
  set_add_elements/1,
  set_remove_elements/1
  ]).

%% Old API exports, these functions should only be used for benchmarking
-export ([
  write_to_antidote/3,
  read_from_antidote/2
  ]).

%% ------------------------------------------------------------------------------------------------
%% Antidote's transaction API wrapper - Use when you need fine grain control over transactions
%% Please refer to the official Antidote transaction documentation for reference on these functions
%% ------------------------------------------------------------------------------------------------

%% A wrapper for Antidote's start_transaction function without a timestamp value.
-spec txn_start() -> {ok, txid()} | {error, reason()}.
txn_start() ->
  {ok,TxnDetails} = rpc:call(?ANTIDOTE,antidote,start_transaction,[ignore,[]]),
  TxnDetails.

%% A wrapper for Antidote's start_transaction function with a specific timestamp value.
-spec txn_start(TimeStamp::snapshot_time()) -> {ok, txid()} | {error, reason()}.
txn_start(TimeStamp) ->
  {ok,TxnDetails} = rpc:call(?ANTIDOTE,antidote,start_transaction,[TimeStamp,[]]),
  TxnDetails.

%% A wrapper for Antidote's read_objects function, with a single object being read.
-spec txn_read_object(Object::bound_object(), TxnDetails::txid()) -> term() | {error, reason()}.
txn_read_object(Object,TxnDetails) ->
  {ok,[Value]} = rpc:call(?ANTIDOTE,antidote,read_objects,[[Object],TxnDetails]),
  Value.

%% A wrapper for Antidote's read_objects function
-spec txn_read_objects(Objects::[bound_object()], TxnDetails::txid())
                                      -> [term()] | {error, reason()}.
txn_read_objects(Objects,TxnDetails) ->
  {ok,Values} = rpc:call(?ANTIDOTE,antidote,read_objects,[Objects,TxnDetails]),
  Values.

%% A wrapper for Antidote's update_objects function, with a single object being written.
-spec txn_update_object({bound_object(), op_name(), op_param()}, txid())
                                          -> ok | {error, reason()}.
txn_update_object(ObjectUpdate,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[[ObjectUpdate],TxnDetails]).

%% A wrapper for Antidote's update_objects function
-spec txn_update_objects([{bound_object(), op_name(), op_param()}], txid())
                                          -> ok | {error, reason()}.
txn_update_objects(ObjectUpdates,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[ObjectUpdates,TxnDetails]).

%% A wrapper for Antidote's commit_transaction function
-spec txn_commit(TxnDetails::txid()) -> ok | {error, reason()}.
txn_commit(TxnDetails) ->
  {ok,_CommitTime} = rpc:call(?ANTIDOTE,antidote,commit_transaction,[TxnDetails]),
  ok.

%% ------------------------------------------------------------------------------------------------
%% Helper functions to assist in map updates
%% ------------------------------------------------------------------------------------------------
-spec build_map_update([map_field_update()]) -> map_op().
build_map_update(OpList) ->
  {update, OpList}.

%% Utility function for updating a map within another map. This operation can be nested to update
%% arbitrarily nested maps.
%% ListOps - list of operations to perform on the nested map.
%% NestedMapKey - nested map's key inside the top-level map.
%% TopLevelMapType - may be of type ?MAP or ?NESTED_MAP
%% TopLevelMapKey - key for the outer map (may or may not be a top level map inside Antidote)
-spec build_nested_map_op(field(),crdt(),field(),[map_field_update()]) -> map_field_update().
build_nested_map_op(TopLevelMapKey,TopLevelMapType,NestedMapKey,ListOps) ->
  NestedMapUpdate = build_map_update(ListOps),
  NestedMapOp = build_map_op(NestedMapKey,?NESTED_MAP,NestedMapUpdate),
  TopLevelMapUpdate = build_map_update([NestedMapOp]),
  build_map_op(TopLevelMapKey,TopLevelMapType,TopLevelMapUpdate).

%% Builds an Antidote acceptable map operation, taking a key, key-type, and the actual operation.
-spec build_map_op(field(), crdt(), crdt_op()) -> map_field_update().
build_map_op(Key,Type,Op) ->
  {update, {Key,Type}, Op}.

%% Calls Antidote's transaction update function with information about the requesting Actor,
%% necessary for map update operations.
-spec txn_update_map(object_bucket(),?MAP_UPDATE_OP,[map_field_update()],txid(),actorordot())
                                                  -> ok | {error, reason()}.
txn_update_map(Object,Op,ListOps,TxnDetails,Actor)->
  txn_update_object({Object,Op,{ListOps,Actor}},TxnDetails).

%% Searches for a Value within a map that is associated with a specific key.
%% All riak_dt_map entries are of type {{key_name,key_type},Value}. Having this function avoids
%% repeating the following code numerous times when searching for an element within a map.
find_key(Map, Key, KeyType) ->
  case lists:keyfind({Key,KeyType},1,Map) of
    false -> not_found;
    {{Key,KeyType},Value} -> Value
  end.

%% ------------------------------------------------------------------------------------------------
%% Simple API - Recommended way to interact with Antidote
%% ------------------------------------------------------------------------------------------------

%% Creates an Antidote bucket of a certain type.
-spec create_bucket(field(), crdt()) -> object_bucket().
create_bucket(Key,Type) ->
  {Key,Type,bucket}.

%% A simple way of getting information from antidote, just requiring a key and key-type.
-spec get(field(), crdt()) -> term().
get(Key,Type) ->
  Bucket = create_bucket(Key,Type),
  TxnDetails = txn_start(),
  ReadResult = txn_read_object(Bucket,TxnDetails),
  ok = txn_commit(TxnDetails),
  ReadResult.

%% Alternative to get/2, using an already existing transaction ID.
%% NOTE: This does not commit the ongoing transaction!
-spec get(field(), crdt(), txid()) -> term().
get(Key,Type,Txn) ->
  Bucket = create_bucket(Key,Type),
  txn_read_object(Bucket,Txn).

%% A simple way of adding information onto Antidote, by specifying a key, key-type, operation
%% and passing in the operation parameters separately.
-spec put(field(), crdt(), crdt_op(), op_param()) -> ok | {error, reason()}.
put(Key,Type,Op,Param) ->
  TxnDetails = txn_start(),
  Bucket = create_bucket(Key,Type),
  ok = txn_update_object({Bucket,Op,Param},TxnDetails),
  ok = txn_commit(TxnDetails).

%% Same as put/4, but appropriate for maps since they require information about the Actor.
-spec put(field(), crdt(), crdt_op(), op_param(), actorordot()) -> ok | {error, reason()}.
put(Key,Type,Op,Param,Actor) ->
  TxnDetails = txn_start(),
  Bucket = create_bucket(Key,Type),
  ok = txn_update_map(Bucket,Op,Param,TxnDetails,Actor),
  ok = txn_commit(TxnDetails).

%% ------------------------------------------------------------------------------------------------
%% ANTIDOTE'S OLD API - Should only be used for benchmarking
%% ------------------------------------------------------------------------------------------------

%% Wrapper for antidote's append function
write_to_antidote(Key,Type,Params) ->
  rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

%% Wrapper for antidote's read function
read_from_antidote(Key,Type) ->
  rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).

%% ------------------------------------------------------------------------------------------------
%% CRDT operations: because Antidote's operation parsing might change over time..?
%% ------------------------------------------------------------------------------------------------

%% Returns an Antidote-compliant operation for incrementing a CRDT counter.
-spec counter_increment(integer()) -> crdt_op().
counter_increment(Amount) ->
  {increment,Amount}.

%% Returns an Antidote-compliant operation for decrementing a CRDT counter.
-spec counter_decrement(integer()) -> crdt_op().
counter_decrement(Amount) ->
  {decrement,Amount}.

%% Returns an Antidote-compliant operation for assigning a value to a CRDT lww-register.
-spec lwwreg_assign(term()) -> crdt_op().
lwwreg_assign(Value) ->
  {assign,Value}.

%% Returns an Antidote-compliant operation for adding a list of items to a CRDT set.
-spec set_add_elements([term()]) -> crdt_op().
set_add_elements(List) ->
  {add, List}.

%% Returns an Antidote-compliant operation for removing a list of items from a CRDT set.
-spec set_remove_elements([term()]) -> crdt_op().
set_remove_elements(List) ->
  {remove, List}.
