%% Generally useful HTTP parsing functions. Required for gen_http_handler and for
%% other modules that may wish to manually parse a specific field.
-module (fmke_http_utils).
-include ("fmke_http.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
-export ([
    parse_body/2
    ,parse_body/3
    ,parse_id/1
    ,parse_string/1
    ,parse_csv_string/1
]).

-spec parse_body(list({atom(), atom()}), binary()) -> list({atom(), any()}).
%% Tries to extract a list of properties from an HTTP body.
%% The property list must be an Erlang proplist in the form [{prop_name, prop_type}]
%% prop_type must be: string | integer
%% Returns a proplist with all the found properties that match the passed types.
parse_body([], _) ->
    [];
parse_body(_, <<>>) ->
    [];
parse_body(PropertyList, BinaryJson) ->
    Json = jsx:decode(BinaryJson),
    parse_body(PropertyList, Json, []).

parse_body([], _, Accum) ->
    lists:reverse(Accum);
parse_body([H|T], Json, Accum) ->
    {Property, Type} = H,
    true = is_atom(Type),
    EncodedProperty = list_to_binary(atom_to_list(Property)),
    try
        ParsedValue = case Type of
            csv_string -> parse_csv_string(proplists:get_value(EncodedProperty, Json));
            string -> parse_string(proplists:get_value(EncodedProperty, Json));
            integer -> parse_id(proplists:get_value(EncodedProperty, Json));
            _ -> erlang:error(unknown_property_type, Type)
        end,
        parse_body(T, Json, [{Property, ParsedValue} | Accum])
    catch
        %% Prevents error from bubbling up since this is a tentative approach
        _:_ -> parse_body(T, Json, Accum)
    end.

%% Does a best effort approach to parsing an integer
parse_id(undefined) ->
    erlang:error(missing_id);
parse_id(Id) when is_integer(Id) andalso Id >= ?MIN_ID ->
    Id;
parse_id(Id) when is_integer(Id) ->
    erlang:error(invalid_id);
parse_id(Id) when is_list(Id) ->
    try
        parse_id(list_to_integer(Id))
    catch
        _:_ -> erlang:error(invalid_id)
    end;
parse_id(Id) when is_binary(Id) ->
    parse_id(binary_to_list(Id)).

parse_string(undefined) ->
    erlang:error(missing_string);
parse_string(String) when is_binary(String) ->
    List = binary_to_list(String),
    case io_lib:printable_unicode_list(List) of
        true -> List;
        false -> erlang:error(invalid_string)
    end;
parse_string(String) when is_list(String) ->
    String.

parse_csv_string(undefined) ->
    erlang:error(missing_csv_string);
parse_csv_string(String) ->
    ParsedString = parse_string(String),
    lists:map(fun(Str) -> string:trim(Str) end, string:tokens(ParsedString, ",")).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_undefined_id_test() ->
    ?assertException(error, missing_id, parse_id(undefined)).

parse_non_negative_id_from_integer_test() ->
    0 = parse_id(0).

parse_non_negative_id_from_string_test() ->
    1 = parse_id("1").

parse_binary_non_negative_id_test() ->
    2 = parse_id(<<"2">>).

parse_negative_id_from_integer_test() ->
    ?assertError(invalid_id, parse_id(-1)).

parse_negative_id_from_string_test() ->
    ?assertError(invalid_id, parse_id("-1")).

parse_invalid_string_as_id_test() ->
    ?assertError(invalid_id, parse_id("abc")).

parse_binary_string_as_id_test() ->
    ?assertError(invalid_id, parse_id(<<"d">>)).

parse_invalid_binary_as_id_test() ->
    ?assertError(invalid_id, parse_id(<<1, 17, 42>>)).

parse_undefined_as_string_test() ->
    ?assertError(missing_string, parse_string(undefined)).

parse_valid_string_from_binary_test() ->
    "FMKe" = parse_string(<<"FMKe">>).

parse_string_from_invalid_binary_test() ->
    ?assertError(invalid_string, parse_string(<<123, 200, 21>>)).

parse_empty_list_of_params_with_valid_body_test() ->
    [] = parse_body([], <<"body">>).

parse_empty_body_test() ->
    [] = parse_body([{username, string}, {password, string}], <<>>),
    [] = parse_body([{medicine, csv_string}], <<>>).

parse_property_with_invalid_type_from_valid_body_test() ->
    [] = parse_body([{fmke, text}], <<"{\"fmke\":\"benchmark\"}">>).

parse_multiple_properties_from_valid_body_test() ->
    [{fmke, "benchmark"}, {rating, "awesome"}]
        = parse_body([{fmke, string}, {rating, string}], <<"{\"fmke\":\"benchmark\",\"rating\":\"awesome\"}">>).

parse_partial_prop_list_from_valid_body_test() ->
    [{fmke, "benchmark"}] = parse_body([{fmke, string}, {rating, string}], <<"{\"fmke\":\"benchmark\"}">>),
    [{fmke, "benchmark"}] = parse_body([{fmke, string}, {rating, integer}], <<"{\"fmke\":\"benchmark\"}">>),
    [{fmke, "benchmark"}] = parse_body([{fmke, string}, {rating, csv_string}], <<"{\"fmke\":\"benchmark\"}">>).

parse_deeply_nested_object_from_body_test() ->
    [{fmke, [{<<"is">>, [{<<"a">>, [{<<"great">>, [{<<"benchmark">>, true}]}]}]}]}]
        = parse_body([{fmke, string}], <<"{\"fmke\":{\"is\":{\"a\":{\"great\":{\"benchmark\":true}}}}}">>).

parse_multi_value_data_from_body_test() ->
    [{fmke, [<<"great">>, <<"useful">>]}] = parse_body([{fmke, string}], <<"{\"fmke\":[\"great\",\"useful\"]}">>).

parse_csv_string_data_from_body_test() ->
    [{fmke, ["benchmark", "key-value stores"]}]
        = parse_body([{fmke, csv_string}], <<"{\"fmke\":\"benchmark,key-value stores\"}">>).

parse_integer_number_from_body_test() ->
    [{number, 20012018}] = parse_body([{number, integer}], <<"{\"number\":\"20012018\"}">>),
    %% negative numbers are not considered valid IDs so they throw an exception
    [] = parse_body([{number, integer}], <<"{\"number\":\"-20012018\"}">>).

-endif.
