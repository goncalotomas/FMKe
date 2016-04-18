-module(fmk_core).

-export([write_to_antidote/3,read_from_antidote/2,test_antidote_maps/0]). %%TODO finish

-define(ANTIDOTE, 'antidote@127.0.0.1').

write_to_antidote(Key,Type,Params) ->
	rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
	rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).

%% ----------------------------------------------------------------------------
%%                                Test Functions
%% ----------------------------------------------------------------------------
test_antidote_maps() ->
  RegisterUpdate = {update,{key, riak_dt_lwwreg},{assign, <<"Awesome">>}},
  CounterUpdate = {update,{val, riak_dt_gcounter},{increment,1}},
  write_to_antidote(patient123,riak_dt_map, {update,[RegisterUpdate,CounterUpdate]}).