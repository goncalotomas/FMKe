 -module (fmke_http_handler_patients).
-include ("fmk_http.hrl").

-behaviour(fmke_gen_http_handler).

-export([init/2, handle_req/3, perform_operation/4]).

init(Req, Opts) ->
    fmke_gen_http_handler:init(?MODULE, Req, Opts).

%% Create patient function ( POST /patients )
handle_req(<<"GET">>, _, Req) ->
    fmke_gen_http_handler:handle_req(?MODULE, <<"GET">>, Req, [id], []);

handle_req(<<"POST">>, false, Req) ->
    fmke_gen_http_handler:handle_reply(?MODULE, Req, {err, bad_req}, ?ERR_MISSING_BODY);

handle_req(<<"POST">>, true, Req) ->
    Properties = [{id, integer}, {name, string}, {address, string}],
    fmke_gen_http_handler:handle_req(?MODULE, <<"POST">>, Req, [], Properties);

handle_req(<<"PUT">>, false, Req) ->
    fmke_gen_http_handler:handle_reply(?MODULE, Req, {err, bad_req}, ?ERR_MISSING_BODY);

handle_req(<<"PUT">>, true, Req) ->
    Properties = [{name, string}, {address, string}],
    fmke_gen_http_handler:handle_req(?MODULE, <<"PUT">>, Req, [id], Properties).

perform_operation(<<"GET">>, Req, [{id, BinaryId}], []) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:get_patient_by_id(Id) of
            {error, Reason} -> {false, Reason};
            PatientRecord -> {true, fmke_proplists:encode_object(patient, PatientRecord)}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

perform_operation(<<"POST">>, Req, [], [{id, Id}, {name, Name}, {address, Address}]) ->
    try
        {Success, ServerResponse} = case fmke:create_patient(Id, Name, Address) of
            ok -> {true, ok};
            {error, Reason} -> {false, Reason}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

perform_operation(<<"PUT">>, Req, [{id, BinaryId}], [{name, Name}, {address, Address}]) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:update_patient_details(Id, Name, Address) of
            ok -> {true, ok};
            {error, Reason} -> {false, Reason}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end.
