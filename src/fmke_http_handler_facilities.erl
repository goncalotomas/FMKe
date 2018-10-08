 -module (fmke_http_handler_facilities).
-include ("fmke_http.hrl").

-behaviour(fmke_gen_http_handler).

-export([init/2, handle_req/3, perform_operation/4]).

init(Req, Opts) ->
    fmke_gen_http_handler:init(?MODULE, Req, Opts).

%% Create patient function ( POST /patients )
handle_req(<<"GET">>, _, Req) ->
    fmke_gen_http_handler:handle_req(?MODULE, <<"GET">>, Req, [id], []);

handle_req(<<"POST">>, true, Req) ->
    Properties = [{id, integer}, {name, string}, {address, string}, {type, string}],
    fmke_gen_http_handler:handle_req(?MODULE, <<"POST">>, Req, [], Properties);

handle_req(<<"PUT">>, true, Req) ->
    Properties = [{name, string}, {address, string}, {type, string}],
    fmke_gen_http_handler:handle_req(?MODULE, <<"PUT">>, Req, [id], Properties).

perform_operation(<<"GET">>, Req, [{id, BinaryId}], []) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:get_facility_by_id(Id) of
            {error, Reason} -> {false, Reason};
            FacilityRecord -> {true, fmke_json:encode(FacilityRecord)}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

perform_operation(<<"POST">>, Req, [], [{id, Id}, {name, Name}, {address, Address}, {type, Type}]) ->
    try
        {Success, ServerResponse} = case fmke:create_facility(Id, Name, Address, Type) of
            ok -> {true, ok};
            {error, Reason} -> {false, Reason}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

perform_operation(<<"PUT">>, Req, [{id, BinaryId}], [{name, Name}, {address, Address}, {type, Type}]) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:update_facility_details(Id, Name, Address, Type) of
            ok -> {true, ok};
            {error, Reason} -> {false, Reason}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end.
