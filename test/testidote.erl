-module (testidote).
-include("fmk.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ============================================================================
%% EUnit tests
%% ============================================================================
-ifdef(TEST).
testidote_test() ->
  net_kernel:start(['testidote@127.0.0.1',longnames]),
  erlang:set_cookie(node(),antidote).

antidote_ping_test() ->
  ?assertEqual(pong, net_adm:ping(?ANTIDOTE)).

counter_test() ->
  fmk_core:write_to_antidote(?TEST_COUNTER_KEY,?COUNTER_TYPE, {increment, 1}),
  fmk_core:write_to_antidote(?TEST_COUNTER_KEY,?COUNTER_TYPE, {increment, 1}),
  fmk_core:write_to_antidote(?TEST_COUNTER_KEY,?COUNTER_TYPE, {increment, 1}),
  ?assertEqual({ok,3}, fmk_core:read_from_antidote(?TEST_COUNTER_KEY,?COUNTER_TYPE)).

map_test() ->
  RegisterUpdate = {update,{key, riak_dt_lwwreg},{assign, <<"Awesome">>}},
  CounterUpdate = {update,{val, riak_dt_gcounter},{increment,1}},
  {ok,_} = fmk_core:write_to_antidote(patient123,riak_dt_map, {update,[RegisterUpdate,CounterUpdate]}),
  {ok,_} = fmk_core:read_from_antidote(patient123,riak_dt_map).

-endif.