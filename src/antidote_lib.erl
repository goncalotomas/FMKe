%% A module to interact with the Antidote platform
%% https://github.com/SyncFree/antidote/
%% This module should be easily integrated in your OTP application.
%% If you want to use this module in your application, you need an Erlang Header file (.hrl)
%% with the following macros:
%% ANTIDOTE: Node where Antidote is running. Usually 'antidote@127.0.0.1'
-module(antidote_lib).

-export ([
	]).

start_txn() ->
  ok.

read_objects() ->
  ok.

write_objects() ->
  ok.

commit_txn() ->
  ok.