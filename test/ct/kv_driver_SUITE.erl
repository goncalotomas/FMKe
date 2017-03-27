%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(kv_driver_SUITE).

-compile({parse_transform, lager_transform}).

%% common_test callbacks
-export([%% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

-export([test_redis_kv_driver/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [test_redis_kv_driver].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(test_redis_kv_driver, Config) ->
    {ok, Context}  = redis_kv_driver:init(["127.0.0.1", 6379]),
    [{context, Context}| Config];

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

test_redis_kv_driver(Config) ->
    Context = proplists:get_value(context, Config),
    test_kv_driver_common(redis_kv_driver, Context).

test_kv_driver_common(Driver, Context) ->
    %% test register update
    register_update(Driver, Context),
    set_update(Driver, Context),
    multiple_key_updates(Driver, Context),
    nested_map_update(Driver, Context),
    ok.

register_update(Driver, Context) ->
    OuterMap = <<"outermaprkey">>,
    RegisterKey = <<"registerkey">>,
    RegisterVal = <<"regval">>,
    {ok, _} = Driver:update_map(OuterMap, [{create_register, RegisterKey, RegisterVal}], Context),
    {ok, MapObj, _} = Driver:get_map(OuterMap, Context),
    {ok, Res, _} = Driver:find_key(MapObj, RegisterKey, register, Context),
    ?assertEqual(Res, RegisterVal).

set_update(Driver, Context) ->
    OuterMap = <<"outermapskey">>,
    SetKey = <<"Setkey">>,
    SetVal = [<<"a">>, <<"b">>, <<"c">>],
    {ok, _} = Driver:update_map(OuterMap, [{create_set, SetKey, SetVal}], Context),
    {ok, MapObj, _} = Driver:get_map(OuterMap, Context),
    {ok, Res, _} = Driver:find_key(MapObj, SetKey, set, Context),
    ?assertEqual(lists:sort(SetVal), lists:sort(Res)).

multiple_key_updates(Driver, Context) ->
    OuterMap = <<"multipleupdatekey">>,
    RegisterKey1 = <<"r1">>,
    RegisterVal1 = <<"rval1">>,
    SetKey1 = <<"s1">>,
    SetVal1 = [<<"a1">>, <<"b1">>, <<"c1">>],
    RegisterKey2 = <<"r2">>,
    RegisterVal2 = <<"rval2">>,
    SetKey2 = <<"s2">>,
    SetVal2 = [<<"a2">>, <<"b2">>, <<"c2">>],
    Ops = [{create_register, RegisterKey1, RegisterVal1},
           {create_set, SetKey1, SetVal1},
           {create_register, RegisterKey2, RegisterVal2},
           {create_set, SetKey2, SetVal2}],
    {ok, _} = Driver:update_map(OuterMap, Ops, Context),
    {ok, MapObj, _} = Driver:get_map(OuterMap, Context),
    {ok, Res1, _} = Driver:find_key(MapObj, SetKey1, set, Context),
    ?assertEqual(lists:sort(Res1), lists:sort(SetVal1)),
    {ok, Res2, _} = Driver:find_key(MapObj, SetKey2, set, Context),
    ?assertEqual(lists:sort(Res2), lists:sort(SetVal2)),
    {ok, Res3, _} = Driver:find_key(MapObj, RegisterKey1, register, Context),
    ?assertEqual(Res3, RegisterVal1),
    {ok, Res4, _} = Driver:find_key(MapObj, RegisterKey2, register, Context),
    ?assertEqual(Res4, RegisterVal2).

nested_map_update(Driver, Context) ->
    OuterMap = <<"outermapmkey">>,
    InnerMapKey = <<"InnerMapKey">>,
    RegisterKey = <<"iregisterkey">>,
    RegisterVal = <<"iregval">>,
    SetKey = <<"iSetkey">>,
    SetVal = [<<"x">>, <<"y">>, <<"z">>],
    InnerMapUpdate = [{create_register, RegisterKey, RegisterVal}, {create_set, SetKey, SetVal}],
    {ok, _} = Driver:update_map(OuterMap, [{create_map, InnerMapKey, InnerMapUpdate}], Context),
    {ok, MapObj, _} = Driver:get_map(OuterMap, Context),
    {ok, InnerMapObj, _} = Driver:find_key(MapObj, InnerMapKey, map, Context),
    {ok, Res1, _} = Driver:find_key(InnerMapObj, RegisterKey, register, Context),
    ?assertEqual(Res1, RegisterVal),
    {ok, Res2, _} = Driver:find_key(InnerMapObj, SetKey, set, Context),
    ?assertEqual(lists:sort(Res2), lists:sort(SetVal)).
