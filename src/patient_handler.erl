-module (patient_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([patient_handle_function/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, patient_handle_function}
	], Req, State}.
	
patient_handle_function(Req0=#{method := <<"POST">>}, State) ->
		true = cowboy_req:has_body(Req0),
		{ok, _Data, Req} = cowboy_req:read_body(Req0),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req0),
    {ok, Req, State};

patient_handle_function(Req, State) ->
		Body = <<"{\"patient-endpoint\": \"Hello FMKe!\"}">>,
		{Body, Req, State}.
