-module (testidote).
-include("fmk.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ============================================================================
%% EUnit tests
%% NOTE: THESE TESTS REQUIRE AN ANTIDOTE NODE RUNNING ON LOCALHOST!!!
%% TODO: initializer test should be a fixture.
%% See https://github.com/erlang/rebar3/issues/1166
%% ============================================================================
-ifdef(TEST).
initializer_test() ->
  net_kernel:start(['testidote@127.0.0.1',longnames]),
  erlang:set_cookie(node(),antidote).

antidote_ping_test() ->
  ?assertEqual(pong, net_adm:ping(?ANTIDOTE)).

counter_test() ->
  ok = antidote_lib:put(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE, {increment,1}),
  ok = antidote_lib:put(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE, {increment,2}),
  ok = antidote_lib:put(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE, {increment,3}),
  ?assertEqual(6, antidote_lib:get(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE)),
  antidote_lib:put(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE, {decrement,1}),
  antidote_lib:put(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE, {decrement, 2}),
  antidote_lib:put(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE, {decrement, 3}),
  ?assertEqual(0, antidote_lib:get(?TEST_COUNTER_KEY,?TEST_COUNTER_TYPE)).

map_test() ->
  RegisterUpdate = antidote_lib:build_map_op(lwwreg,riak_dt_lwwreg,{assign, <<"Awesome">>}),
  CounterUpdate = antidote_lib:build_map_op(counter,riak_dt_pncounter,{increment,0}),
  ok = antidote_lib:put(?TEST_MAP_KEY,riak_dt_map, antidote_lib:build_map_update([RegisterUpdate,CounterUpdate])),
  Map = antidote_lib:get(?TEST_MAP_KEY,riak_dt_map),
  ?assertEqual(0, antidote_lib:find_key(Map,counter,riak_dt_pncounter)),
  ?assertEqual(<<"Awesome">>, antidote_lib:find_key(Map,lwwreg,riak_dt_lwwreg)).

nested_map_test() ->
  RegisterUpdate = antidote_lib:build_map_op(lwwreg,riak_dt_lwwreg,{assign, <<"Awesome">>}),
  MapUpdate = antidote_lib:build_map_update([RegisterUpdate]),
  NestedMapOp = antidote_lib:build_map_op(?TEST_NESTED_MAP_KEY,riak_dt_map,MapUpdate),
  ok = antidote_lib:put(?TEST_MAP_KEY,riak_dt_map, antidote_lib:build_map_update([NestedMapOp])),
  Map = antidote_lib:get(?TEST_MAP_KEY,riak_dt_map),
  NestedMap = antidote_lib:find_key(Map,?TEST_NESTED_MAP_KEY,riak_dt_map),
  ?assertEqual(<<"Awesome">>, antidote_lib:find_key(NestedMap,lwwreg,riak_dt_lwwreg)).

-endif.
