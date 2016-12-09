-module (pharmacy_handler).
-include ("fmk_http.hrl").

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = handle_req(Method, HasBody, Req0),
	{ok, Req, Opts}.

handle_req(<<"POST">>, true, Req) ->
		create_pharmacy(Req);
handle_req(<<"POST">>, false, Req) ->
		cowboy_req:reply(400, [], ?ERR_MISSING_BODY, Req);

handle_req(<<"PUT">>, true, Req) ->
		update_pharmacy(Req);
handle_req(<<"PUT">>, false, Req) ->
		cowboy_req:reply(400, [], ?ERR_MISSING_BODY, Req);

handle_req(<<"GET">>, true, Req) ->
		cowboy_req:reply(400, [], ?ERR_BODY_IN_A_GET_REQUEST, Req);
handle_req(<<"GET">>, false, Req) ->
		get_pharmacy(Req).

create_pharmacy(Req) ->
		{ok, [{<<"id">>, PharmacyId},
		{<<"name">>, PharmacyName},
		{<<"address">>, PharmacyAddress}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		IntegerId = binary_to_integer(PharmacyId),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_FACILITY_ID, Req);
				false ->
						StringName = binary_to_list(PharmacyName),
						StringAddress = binary_to_list(PharmacyAddress),
						ServerResponse = fmk_core:create_pharmacy(IntegerId,StringName,StringAddress),
						Success = ServerResponse =:= ok,
						JsonReply =	jsx:encode([{success,Success},{result,ServerResponse}]),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

update_pharmacy(Req) ->
		{ok, [{<<"name">>, Name},
		{<<"address">>, Address}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		Id = cowboy_req:binding(?BINDING_PHARMACY_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PHARMACY_ID, Req);
				false ->
						StringName = binary_to_list(Name),
						StringAddress = binary_to_list(Address),
						ServerResponse = fmk_core:update_pharmacy_details(IntegerId,StringName,StringAddress),
						Success = ServerResponse =:= ok,
						JsonReply =	jsx:encode([{success,Success},{result,ServerResponse}]),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

get_pharmacy(Req) ->
		Id = cowboy_req:binding(?BINDING_PHARMACY_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, [], ?ERR_INVALID_PHARMACY_ID, Req);
				false ->
						ServerResponse = fmk_core:get_pharmacy_by_id(IntegerId),
						Success = ServerResponse =/= {error,not_found},
						JsonReply = case Success of
								true ->
										jsx:encode([{success,Success},{result,crdt_json_encoder:encode_object(pharmacy,ServerResponse)}]);
								false ->
										jsx:encode([{success,Success},{result,ServerResponse}])
						end,
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.
