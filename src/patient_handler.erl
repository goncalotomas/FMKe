-module (patient_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([patient_handle_function/2]).
-export([create_patient/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
		{[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
		{[
			{<<"application/json">>, patient_handle_function},
			{{<<"text">>, <<"html">>, []}, patient_handle_function}
		], Req, State}.

content_types_accepted(Req, State) ->
		{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_patient}],
			Req, State}.

patient_handle_function(Req0, State) ->
	Req = cowboy_req:reply(200, #{
			<<"content-type">> => <<"text/plain">>
	}, <<"Hello world!">>, Req0),
	{ok, Req, State}.

create_patient(Req, State) ->
		{ok, [{<<"id">>, Id},
		{<<"name">>, Name},
		{<<"address">>, Address}
		], _Req0} = cowboy_req:read_urlencoded_body(Req),
		IntegerId = binary_to_integer(Id),
		StringName = binary_to_list(Name),
		StringAddress = binary_to_list(Address),
		ok = fmk_core:create_patient(IntegerId,StringName,StringAddress),
		Req1 = cowboy_req:reply(200, #{
				<<"content-type">> => <<"text/plain">>
		}, <<"ok">>, Req),
		case cowboy_req:method(Req) of
			<<"POST">> ->
				{ok, Req1, State};
			_ ->
				{{true, <<"ko">>}, Req1, State}
		end.
