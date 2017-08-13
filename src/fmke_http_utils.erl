-module (fmke_http_utils).
-include ("fmk_http.hrl").

-export ([parse_body/2, parse_body/3, parse_id/1, parse_string/2]).

parse_body([], _) ->
		[];
parse_body(_, <<>>) ->
		[];
parse_body(PropertyList, BinaryJson) ->
		Json = jsx:decode(BinaryJson),
		parse_body(PropertyList, Json, []).

parse_body([],_,Accum) ->
		Accum;
parse_body([H|T],Json,Accum) ->
		{Property, Type} = H,
		true = is_atom(Type),
		EncodedProperty = list_to_binary(atom_to_list(Property)),
		ParsedValue = case Type of
				string -> parse_string(Property, proplists:get_value(EncodedProperty, Json));
				integer -> parse_id(proplists:get_value(EncodedProperty, Json))
		end,
		parse_body(T,Json,lists:append(Accum,[{Property, ParsedValue}])).

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
						try
								parse_id(binary_to_integer(Id))
						catch
								_:_ -> erlang:error(invalid_id)
						end;
				_:_ -> erlang:error(invalid_id)
		end.

parse_string(Name, undefined) ->
		erlang:error(list_to_atom("missing_" ++ atom_to_list(Name)));
parse_string(Name, String) when is_binary(String) ->
		try
				binary_to_list(String)
		catch
				error:badarg -> erlang:error(list_to_atom("invalid_" ++ atom_to_list(Name)))
		end;
parse_string(_, String) when is_list(String) ->
		String.
