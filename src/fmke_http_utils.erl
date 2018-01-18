%% Generally useful HTTP parsing functions. Required for gen_http_handler and for
%% other modules that may wish to manually parse a specific field.
-module (fmke_http_utils).
-include ("fmk_http.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
-export ([
    parse_body/2,
    parse_body/3,
    parse_id/1,
    parse_string/2,
    parse_csv_string/2
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
    Accum;
parse_body([H|T], Json, Accum) ->
    {Property, Type} = H,
    true = is_atom(Type),
    EncodedProperty = list_to_binary(atom_to_list(Property)),
    try
        ParsedValue = case Type of
            csv_string -> parse_csv_string(Property, proplists:get_value(EncodedProperty, Json));
            string -> parse_string(Property, proplists:get_value(EncodedProperty, Json));
            integer -> parse_id(proplists:get_value(EncodedProperty, Json))
        end,
        parse_body(T, Json, lists:append(Accum, [{Property, ParsedValue}]))
    catch
        _:_ ->
            %% Current property could not be found, prevent error from bubbling up
            parse_body(T, Json, Accum)
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
    try
        parse_id(binary_to_list(Id))
    catch
        error:badarg ->
            %% could be a binary integer instead of a binary list
            parse_id(binary_to_integer(Id));
        _:_ -> erlang:error(invalid_id)
    end.

parse_string(Name, undefined) ->
    erlang:error(list_to_atom("missing_" ++ atom_to_list(Name)));
parse_string(Name, String) when is_binary(String) ->
    try
        List = binary_to_list(String),
        case io_lib:printable_unicode_list(List) of
            true -> List;
            false -> erlang:error(list_to_atom("invalid_" ++ atom_to_list(Name)))
        end
    catch
        error:badarg -> erlang:error(list_to_atom("invalid_" ++ atom_to_list(Name)))
    end;
parse_string(_, String) when is_list(String) ->
    String.

parse_csv_string(Name, String) ->
    ParsedString = parse_string(Name, String),
    lists:map(fun(Str) -> re:replace(Str, " ", "", [global, {return, list}]) end, string:tokens(ParsedString, ",")).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

parse_undefined_as_string_test() ->
    ?assertError(missing_test, parse_string(test, undefined)).

parse_valid_string_from_binary_test() ->
    "FMKe" = parse_string(parameter, <<"FMKe">>).

parse_string_from_invalid_binary_test() ->
    ?assertError(invalid_parameter, parse_string(parameter, <<123, 200, 21>>)).

-endif.
