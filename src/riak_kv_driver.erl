%%%-------------------------------------------------------------------
%%% @author goncalotomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2017 15:26
%%%-------------------------------------------------------------------
-module(riak_kv_driver).
-include("fmk.hrl").
-author("goncalotomas").

-behaviour(gen_kv_driver).

%% API
-export([
  init/1,
  get_key/3,
  get_list_of_keys/3,
  put/4,
  start_transaction/1,
  commit_transaction/2
]).

init(_Anything) ->
  erlang:error(not_implemented).

get_key(_Key, _KeyType, _Context) ->
  erlang:error(not_implemented).

get_list_of_keys(_ListKeys, _ListKeyTypes, _Context) ->
  erlang:error(not_implemented).

put(_Key, _KeyType, _Value, _Context) ->
  erlang:error(not_implemented).

start_transaction(_Context) ->
  erlang:error(not_implemented).

commit_transaction(_Context, _TransactionId) ->
  erlang:error(not_implemented).