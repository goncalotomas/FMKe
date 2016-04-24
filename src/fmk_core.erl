-module(fmk_core).
-include("fmk.hrl").

-export([write_to_antidote/3,read_from_antidote/2]). %%TODO finish

write_to_antidote(Key,Type,Params) ->
	rpc:call(?ANTIDOTE,antidote,append,[Key,Type,{Params,self()}]).

read_from_antidote(Key,Type) ->
	rpc:call(?ANTIDOTE,antidote,read,[Key,Type]).
