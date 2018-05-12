-module (fmke_http_handler_prescriptions).
-include ("fmke_http.hrl").

-behaviour(fmke_gen_http_handler).

-export([init/2, handle_req/3, perform_operation/4]).

init(Req, Opts) ->
    fmke_gen_http_handler:init(?MODULE, Req, Opts).

%% Create patient function ( POST /patients )
handle_req(<<"GET">>, _, Req) ->
    fmke_gen_http_handler:handle_req(?MODULE, <<"GET">>, Req, [id], []);

handle_req(<<"POST">>, true, Req) ->
    Properties = [{id, integer}, {patient_id, integer}, {prescriber_id, integer},
        {pharmacy_id, integer}, {date_prescribed, string}, {drugs, csv_string}],
    fmke_gen_http_handler:handle_req(?MODULE, <<"POST">>, Req, [], Properties);

handle_req(<<"PUT">>, true, Req) ->
    Properties = [{date_processed, string}, {drugs, csv_string}],
    fmke_gen_http_handler:handle_req(?MODULE, <<"PUT">>, Req, [id], Properties).

perform_operation(<<"GET">>, Req, [{id, BinaryId}], []) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:get_prescription_by_id(Id) of
            {error, Reason} -> {false, Reason};
            PrescriptionRecord -> {true, fmke_json:encode(PrescriptionRecord)}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

perform_operation(<<"POST">>, Req, [],
                  [{id, Id}, {patient_id, PatId}, {prescriber_id, StaffId},
                  {pharmacy_id, PharmId},  {date_prescribed, DatePresc}, {drugs, Drugs}]) ->
    try
        {Success, ServerResponse} = case fmke:create_prescription(Id, PatId, StaffId, PharmId, DatePresc, Drugs) of
            ok -> {true, ok};
            {error, txn_aborted} -> {false, <<"txn_aborted">>};
            {error, Reason} -> {false, Reason}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

%% Process prescription
perform_operation(<<"PUT">>, Req, [{id, BinaryId}], {incomplete, [{date_processed, Date}]}) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:process_prescription(Id, Date) of
            ok -> {true, ok};
            {error, txn_aborted} -> {false, <<"txn_aborted">>};
            {error, Reason} -> {false, Reason}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end;

%% Update prescription medication
perform_operation(<<"PUT">>, Req, [{id, BinaryId}], {incomplete, [{drugs, Drugs}]}) ->
    try
        Id = fmke_http_utils:parse_id(BinaryId),
        {Success, ServerResponse} = case fmke:update_prescription_medication(Id, add_drugs, Drugs) of
            ok -> {true, ok};
            {error, txn_aborted} -> {false, <<"txn_aborted">>};
            {error, prescription_already_processed} -> {false, <<"prescription_already_processed">>};
            {error, Reason} -> {false, list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))}
        end,
        fmke_gen_http_handler:handle_reply(?MODULE, Req, ok, Success, ServerResponse)
    catch error:ErrReason ->
        fmke_gen_http_handler:handle_reply(?MODULE, Req, {error, bad_req}, false, ErrReason)
    end.
