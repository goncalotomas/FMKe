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
  put/2
  ]).

%% These exports can help you make your own transactions, you can manage them as you please.
-export ([
  txn_start/0,
  txn_read_object/2,
  txn_read_objects/2,
  txn_write_object/2,
  txn_write_objects/2,
  txn_commit/1
	]).

%% Old API exports, these functions should only be used for performance testing
-export ([
  write_to_antidote/3,
  read_from_antidote/2
  ]).

%% ------------------------------------------------------------------------------------------------
%% Simple API - Recommended way to interact with Antidote
%% ------------------------------------------------------------------------------------------------
create_bucket(Key,Type) ->
  {Key,Type,bucket}.

get(Key,Type) ->
  Bucket = create_bucket(Key,Type),
  TxnDetails = txn_start(),
  Value = txn_read_object(Bucket,TxnDetails),
  ok = txn_commit(TxnDetails),
  Value.

put(Bucket) ->
  TxnDetails = txn_start(),
  ok = txn_write_object(Bucket,TxnDetails),
  ok = txn_commit(TxnDetails),
  ok.

%% ------------------------------------------------------------------------------------------------
%% Antidote's transaction API wrapper - Use when you need fine grain control over transactions
%% ------------------------------------------------------------------------------------------------
txn_start() ->
  {ok,TxnDetails} = rpc:call(?ANTIDOTE,antidote,start_transaction,[ignore,[]]),
  TxnDetails.

txn_read_object(Object,TxnDetails) ->
  {ok,[Value]} = rpc:call(?ANTIDOTE,antidote,txn_read_objects,[[Object],TxnDetails]),
  Value.

txn_read_objects(Objects,TxnDetails) ->
  {ok,Values} = rpc:call(?ANTIDOTE,antidote,txn_read_objects,[Objects,TxnDetails]),
  Values.

txn_write_object(Object,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[[Object],TxnDetails]).

txn_write_objects(Objects,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[Objects,TxnDetails]).

txn_commit(TxnDetails) ->
  {ok,_Smthng} = rpc:call(?ANTIDOTE,antidote,txn_commit,[TxnDetails]),
  ok.

%% ------------------------------------------------------------------------------------------------
%% ANTIDOTE'S OLD API - Should only be used for performance testing
%% ------------------------------------------------------------------------------------------------
write_to_antidote(Key,Type,Params) ->
  rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
  rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).