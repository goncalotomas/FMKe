%% A module to interact with the Antidote platform
%% https://github.com/SyncFree/antidote/
%% This module should be easily integrated in your OTP application.
%% If you want to use this module in your application, you need an Erlang Header file (.hrl)
%% with the following macros:
%% ANTIDOTE: Node where Antidote is running. Usually 'antidote@127.0.0.1'
-module(antidote_lib).

-export ([
  build_object/2,
  start_txn/0,
  read_object/2,
  read_objects/2,
  write_object/2,
  write_objects/2,
  commit_txn/1
	]).

%% Old API exports, these functions should only be used for performance testing
-export ([
  write_to_antidote/3,
  read_from_antidote/2
  ]).

%% ------------------------------------------------------------------------------------------------
%% Antidote's transaction API - Use these functions
%% ------------------------------------------------------------------------------------------------

build_object(Key,Type) ->
  {Key,Type,bucket}.

start_txn() ->
  {ok,TxnDetails} = rpc:call(?ANTIDOTE,antidote,start_transaction,[ignore,[]]),
  TxnDetails.

read_object(Object,TxnDetails) ->
  {ok,[Value]} = rpc:call(?ANTIDOTE,antidote,read_objects,[[Object],TxnDetails]),
  Value.

read_objects(Objects,TxnDetails) ->
  {ok,Values} = rpc:call(?ANTIDOTE,antidote,read_objects,[Objects,TxnDetails]),
  Values.

write_object(Object,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[[Object],TxnDetails]).

write_objects(Objects,TxnDetails) ->
  ok = rpc:call(?ANTIDOTE,antidote,update_objects,[Objects,TxnDetails]).

commit_txn(TxnDetails) ->
  {ok,Smthng} = rpc:call(?ANTIDOTE,antidote,commit_txn,[TxnDetails]),
  ok.

%% ------------------------------------------------------------------------------------------------
%% ANTIDOTE'S OLD API - Should only be used for performance testing
%% ------------------------------------------------------------------------------------------------
write_to_antidote(Key,Type,Params) ->
  rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
  rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).