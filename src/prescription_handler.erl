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
		{ok, Data, _Req} = cowboy_req:read_body(Req),
		Json = jsx:decode(Data),
		PrescriptionId = proplists:get_value(<<"id">>, Json),
		PatientId = proplists:get_value(<<"patient_id">>, Json),
		PrescriberId = proplists:get_value(<<"prescriber_id">>, Json),
		PharmacyId = proplists:get_value(<<"pharmacy_id">>, Json),
		FacilityId = proplists:get_value(<<"facility_id">>, Json),
		DatePrescribed = proplists:get_value(<<"date_prescribed">>, Json),
		CsvDrugs = proplists:get_value(<<"drugs">>, Json),
		IntegerId =
			if
				is_binary(PrescriptionId) -> list_to_integer(binary_to_list(PrescriptionId));
				true -> PrescriptionId
			end,
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PRESCRIPTION_ID, Req);
				false ->
						ListDrugs = parse_line(CsvDrugs),
						ServerResponse = fmk_core:create_prescription(PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,ListDrugs),
						Success = ServerResponse =:= ok,
						Result = case ServerResponse of
								ok -> ServerResponse;
								{error, Reason} -> Reason
						end,
						JsonReply =	jsx:encode([{success,Success},{result,Result}]),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

update_prescription(Req) ->
		{ok, Data, _Req} = cowboy_req:read_body(Req),
		Json = jsx:decode(Data),
		DateProcessed = proplists:get_value(<<"date_processed">>, Json),
		CsvDrugs = proplists:get_value(<<"drugs">>, Json),
		Id = cowboy_req:binding(?BINDING_PRESCRIPTION_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PRESCRIPTION_ID, Req);
				false ->
						JsonReply = case DateProcessed of
								undefined ->
										%% Assuming that in this case we only want to update the prescription medication
										case CsvDrugs of
												undefined ->
														jsx:encode([{success,false},{result,nothing_to_update}]);
												_ListDrugs ->
														DrugsList = parse_line(CsvDrugs),
														ServerResponse = fmk_core:update_prescription_medication(IntegerId,add_drugs,DrugsList),
														Success = ServerResponse =:= ok,
														jsx:encode([{success,Success},{result,ServerResponse}])
										end;
								_Date ->
										ServerResponse = fmk_core:process_prescription(IntegerId,DateProcessed),
										Success = ServerResponse =:= ok,
										Result = case ServerResponse of
											{error, Reason} -> Reason;
											ok -> ServerResponse
										end,
										jsx:encode([{success,Success},{result,Result}])
						end,
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
										jsx:encode([{success,Success},{result,crdt_json_encoder:encode_object(prescription,ServerResponse)}]);
								false ->
										jsx:encode([{success,Success},{result,ServerResponse}])
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

parse_field_q("\"\"" ++ Line, Buf, Fields) -> parse_field_q(Line, [$"|Buf], Fields);
parse_field_q("\"" ++ Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field_q([C|Line], Buf, Fields) -> parse_field_q(Line, [C|Buf], Fields).
