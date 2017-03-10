%%%-------------------------------------------------------------------
%%% @author goncalotomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2017 15:51
%%%-------------------------------------------------------------------
-module(gen_kv_driver).
-author("goncalotomas").

-export([behaviour_info/1]).

%% API
behaviour_info(callbacks) ->
  [{init,1}
    ,{get_key,3} %% Returns status {({ok, object}| {error, reason}), context}
    ,{get_list_of_keys,3} %% Returns status {({ok, list(object)} | {error, reason}), context}
    ,{put,4} %% Return status {(ok | error), context}
    ,{start_transaction,1}  %% Return status {(ok | error), context}
    ,{commit_transaction,2} %% Return status {(ok | error), context}
  ];
behaviour_info(_Other) ->
  undefined.