-module (treatment_handler).
-include ("fmk_http.hrl").

-export([init/2]).

init(Req0, Opts) ->
	try
		Method = cowboy_req:method(Req0),
		HasBody = cowboy_req:has_body(Req0),
		Req = handle_req(Method, HasBody, Req0),
		{ok, Req, Opts}
	catch
		Err:Reason ->
			ErrorMessage = io_lib:format("Error ~p:~n~p~n~p~n", [Err, Reason, erlang:get_stacktrace()]),
			io:format(ErrorMessage),
			Req2 = cowboy_req:reply(500, #{}, ErrorMessage, Req0),
			{ok, Req2, Opts}
	end.

handle_req(<<"POST">>, true, Req) ->
		create_treatment(Req);
handle_req(<<"POST">>, false, Req) ->
		cowboy_req:reply(400, #{}, ?ERR_MISSING_BODY, Req);

handle_req(<<"GET">>, true, Req) ->
		cowboy_req:reply(400, #{}, ?ERR_BODY_IN_A_GET_REQUEST, Req);
handle_req(<<"GET">>, false, Req) ->
		get_treatment(Req).

create_treatment(Req) ->
		{ok, [{<<"id">>, Id},
		{<<"patient_id">>, PatientId},
		{<<"prescriber_id">>, PrescriberId},
    {<<"facility_id">>, FacilityId},
    {<<"date_prescribed">>, DatePrescribed}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, #{}, ?ERR_INVALID_FACILITY_ID, Req);
				false ->
            IntPatientId = binary_to_integer(PatientId),
            IntPrescriberId = binary_to_integer(PrescriberId),
            IntFacilityId = binary_to_integer(FacilityId),
						StrDate = binary_to_list(DatePrescribed),
						ServerResponse = fmk_core:create_treatment(IntegerId,IntPatientId,IntPrescriberId,IntFacilityId,StrDate),
						Success = ServerResponse =:= ok,
						JsonReply =	lists:flatten(io_lib:format(
								"{\"success\": \"~p\", \"result\": \"~p\"}",
								[Success,ServerResponse]
						)),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

get_treatment(Req) ->
		Id = cowboy_req:binding(?BINDING_TREATMENT_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, #{}, ?ERR_INVALID_TREATMENT_ID, Req);
				false ->
						ServerResponse = fmk_core:get_treatment_by_id(IntegerId),
						Success = ServerResponse =/= {error,not_found},
						JsonReply = case Success of
								true ->
										lists:flatten(io_lib:format(
												("{\"success\": \"~p\", \"result\": " ++ crdt_json_encoder:encode(treatment,ServerResponse) ++ "}"),
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
