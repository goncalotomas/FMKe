-module (prescription_handler).
-include ("fmk_http.hrl").

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = handle_req(Method, HasBody, Req0),
	{ok, Req, Opts}.

handle_req(<<"POST">>, true, Req) ->
		create_prescription(Req);
handle_req(<<"POST">>, false, Req) ->
		cowboy_req:reply(400, [], ?ERR_MISSING_BODY, Req);

handle_req(<<"PUT">>, true, Req) ->
		update_prescription(Req);
handle_req(<<"PUT">>, false, Req) ->
		cowboy_req:reply(400, [], ?ERR_MISSING_BODY, Req);

handle_req(<<"GET">>, true, Req) ->
		cowboy_req:reply(400, [], ?ERR_BODY_IN_A_GET_REQUEST, Req);
handle_req(<<"GET">>, false, Req) ->
		get_prescription(Req).

create_prescription(Req) ->
		{ok, [{<<"id">>, PrescriptionId},
		{<<"patient_id">>, PatientId},
		{<<"prescriber_id">>, PrescriberId},
		{<<"pharmacy_id">>, PharmacyId},
		{<<"facility_id">>, FacilityId},
		{<<"date_prescribed">>, DatePrescribed},
		{<<"drugs">>, Drugs}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		IntegerId = binary_to_integer(PrescriptionId),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PRESCRIPTION_ID, Req);
				false ->
						IntPatientId = binary_to_integer(PatientId),
						IntPrescriberId = binary_to_integer(PrescriberId),
						IntPharmacyId = binary_to_integer(PharmacyId),
						IntFacilityId = binary_to_integer(FacilityId),
						StrDatePrescribed = binary_to_list(DatePrescribed),
						StrDrugs = parse_line(binary_to_list(Drugs)),
						ServerResponse = fmk_core:create_prescription(IntegerId,IntPatientId,IntPrescriberId,IntPharmacyId,IntFacilityId,StrDatePrescribed,StrDrugs),
						Success = ServerResponse =:= ok,
						JsonReply =	lists:flatten(io_lib:format(
								"{\"success\": \"~p\", \"result\": \"~p\"}",
								[Success,ServerResponse]
						)),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

update_prescription(Req) ->
		{ok,[
		{<<"date_processed">>, DateProcessed}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		Id = cowboy_req:binding(?BINDING_PRESCRIPTION_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PRESCRIPTION_ID, Req);
				false ->
						StrDate = binary_to_list(DateProcessed),
						ServerResponse = fmk_core:process_prescription(IntegerId,StrDate),
						Success = ServerResponse =:= ok,
						JsonReply =	lists:flatten(io_lib:format(
								"{\"success\": \"~p\", \"result\": \"~p\"}",
								[Success,ServerResponse]
						)),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

get_prescription(Req) ->
		Id = cowboy_req:binding(?BINDING_PRESCRIPTION_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PRESCRIPTION_ID, Req);
				false ->
						ServerResponse = fmk_core:get_prescription_by_id(IntegerId),
						Success = ServerResponse =/= {error,not_found},
						JsonReply = case Success of
								true ->
										lists:flatten(io_lib:format(
												("{\"success\": \"~p\", \"result\": " ++ crdt_json_encoder:encode(prescription,ServerResponse) ++ "}"),
												[Success]
										));
								false ->
										lists:flatten(io_lib:format(
												"{\"success\": \"~p\", \"result\": \"~p\"}",
												[Success,ServerResponse]
										))
						end,
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.


%% parse_line function fetched from @src http://stackoverflow.com/a/1532947/3547126

parse_line(Line) -> parse_line(Line, []).

parse_line([], Fields) -> lists:reverse(Fields);
parse_line("," ++ Line, Fields) -> parse_field(Line, Fields);
parse_line(Line, Fields) -> parse_field(Line, Fields).

parse_field("\"" ++ Line, Fields) -> parse_field_q(Line, [], Fields);
parse_field(Line, Fields) -> parse_field(Line, [], Fields).

parse_field("," ++ _ = Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field([C|Line], Buf, Fields) -> parse_field(Line, [C|Buf], Fields);
parse_field([], Buf, Fields) -> parse_line([], [lists:reverse(Buf)|Fields]).

parse_field_q(Line, Fields) -> parse_field_q(Line, [], Fields).
parse_field_q("\"\"" ++ Line, Buf, Fields) -> parse_field_q(Line, [$"|Buf], Fields);
parse_field_q("\"" ++ Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field_q([C|Line], Buf, Fields) -> parse_field_q(Line, [C|Buf], Fields).
