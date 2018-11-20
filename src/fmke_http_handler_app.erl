-module (fmke_http_handler_app).
-include ("fmke_http.hrl").
-include ("fmke.hrl").

-behaviour(fmke_gen_http_handler).

-export([init/2, handle_req/3, perform_operation/4]).

init(Req, Opts) ->
    fmke_gen_http_handler:init(?MODULE, Req, Opts).

handle_req(<<"GET">>, _, Req) ->
    fmke_gen_http_handler:handle_req(?MODULE, <<"GET">>, Req, [], []);

handle_req(<<"POST">>, _, Req) ->
    fmke_gen_http_handler:handle_req(?MODULE, <<"GET">>, Req, [], []);

handle_req(<<"PUT">>, _, Req) ->
    fmke_gen_http_handler:handle_req(?MODULE, <<"GET">>, Req, [], []).

perform_operation(<<"GET">>, Req, [], []) ->
    try
        StatusPropList = fmke:get_status(),
        DatabaseAddrs = lists:map(fun erlang:list_to_binary/1, proplists:get_value(database_addresses, StatusPropList)),
        StatusPropList1 = lists:keyreplace(database_addresses, 1, StatusPropList, {database_addresses, DatabaseAddrs}),
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, true, proplists:delete(http_port, StatusPropList1) )
    catch error:ErrReason ->
        lager:debug("Error getting status:~n~p~n", [ErrReason]),
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end.
