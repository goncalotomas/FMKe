-module(fmk_core).
-include("fmk.hrl").

-export([
  write_to_antidote/3,
  read_from_antidote/2,
  txn_start/0,
  txn_read_objects/0,
  txn_write_objects/0,
  txn_commit/0
  ]).

txn_start() ->
	ok.

txn_read_objects() ->
	ok.

txn_write_objects() ->
  ok.

txn_commit() ->
  ok.

%% ------------------------------------------------------------------------------------------------
%% ANTIDOTE'S OLD API - DEPRECATED? Avoid using at all costs
%% ------------------------------------------------------------------------------------------------
write_to_antidote(Key,Type,Params) ->
	rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
	rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).
