#!/usr/bin/env escript
%%! -pa ../_build/default/lib/jsx/ebin -Wall


main(_Args) ->
    inets:start(),
    test_post_patient(),
    test_get_patient(),
    ok.

test_get_patient() ->
    PatientId = 1,
    Resp = http_get("/patients/" ++ integer_to_list(PatientId)),
    io:format("get patient -> ~p~n", [Resp]).

test_post_patient() ->
    Patient = [
        {<<"id">>, 1},
        {<<"name">>, <<"Peter">>},
        {<<"address">>, <<"Kaiserslautern">>}
    ],
    Resp = http_post("/patients", Patient),
    io:format("Resp = ~p~n", [Resp]),
    ok.


http_get(Url) ->
    FullUrl = "http://localhost:9090" ++ Url,
    Headers = [],
    HttpOptions = [],
    Options = [{sync, true}],
    {ok, {Status, _Headers, ResponseBody}} = httpc:request(get, {FullUrl, Headers}, HttpOptions, Options),
    % check status
    {_,200,_} = Status,
    jsx:decode(list_to_binary(ResponseBody)).

http_post(Url, Data) ->
    FullUrl = "http://localhost:9090" ++ Url,
    Headers = [],
    HttpOptions = [],
    Options = [{sync, true}],
    Json = jsx:encode(Data),
    {ok, {Status, _Headers, ResponseBody}} = httpc:request(post, {FullUrl, Headers, "application/json", Json}, HttpOptions, Options),
    % check status
    {_,200,_} = Status,
    jsx:decode(list_to_binary(ResponseBody)).