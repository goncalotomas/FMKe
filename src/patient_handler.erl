-module (patient_handler).
-include ("fmk_http.hrl").

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = handle_req(Method, HasBody, Req0),
	{ok, Req, Opts}.

handle_req(<<"POST">>, true, Req) ->
		create_patient(Req);
handle_req(<<"POST">>, false, Req) ->
		cowboy_req:reply(400, [], ?ERR_MISSING_BODY, Req);

handle_req(<<"PUT">>, true, Req) ->
		update_patient(Req);
handle_req(<<"PUT">>, false, Req) ->
		cowboy_req:reply(400, [], ?ERR_MISSING_BODY, Req);

handle_req(<<"GET">>, true, Req) ->
		cowboy_req:reply(400, [], ?ERR_BODY_IN_A_GET_REQUEST, Req);
handle_req(<<"GET">>, false, Req) ->
		get_patient(Req).

create_patient(Req) ->
		{ok, Data, _Req2} = cowboy_req:read_body(Req),
		Json = jsx:decode(Data),
		Id = proplists:get_value(<<"id">>, Json),
		Name = proplists:get_value(<<"name">>, Json),
		Address = proplists:get_value(<<"address">>, Json),
		IntegerId = 
			if
				is_binary(Id) -> list_to_integer(binary_to_list(Id));
				true -> Id
			end,
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PATIENT_ID, Req);
				false ->
						StringName = binary_to_list(Name),
						StringAddress = binary_to_list(Address),
						io:format("calling fmk_core:create_patient(~p, ~p, ~p)~n", [IntegerId,StringName,StringAddress]),
						ServerResponse = fmk_core:create_patient(IntegerId,StringName,StringAddress),
						Success = ServerResponse =:= ok,
						JsonReply =	lists:flatten(io_lib:format(
								"{\"success\": \"~p\", \"result\": \"~p\"}",
								[Success,ServerResponse]
						)),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

update_patient(Req) ->
		{ok, [{<<"name">>, Name},
		{<<"address">>, Address}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		Id = cowboy_req:binding(?BINDING_PATIENT_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PATIENT_ID, Req);
				false ->
						StringName = binary_to_list(Name),
						StringAddress = binary_to_list(Address),
						ServerResponse = fmk_core:update_patient_details(IntegerId,StringName,StringAddress),
						Success = ServerResponse =:= ok,
						JsonReply =	lists:flatten(io_lib:format(
								"{\"success\": \"~p\", \"result\": \"~p\"}",
								[Success,ServerResponse]
						)),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

get_patient(Req) ->
		Id = cowboy_req:binding(?BINDING_PATIENT_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PATIENT_ID, Req);
				false ->
						ServerResponse = fmk_core:get_patient_by_id(IntegerId),
						Success = ServerResponse =/= {error,not_found},
						JsonReply = case Success of
								true ->
										lists:flatten(io_lib:format(
												("{\"success\": \"~p\", \"result\": " ++ crdt_json_encoder:encode(patient,ServerResponse) ++ "}"),
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
