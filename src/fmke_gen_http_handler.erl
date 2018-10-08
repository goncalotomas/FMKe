%% Default behaviour for a generic HTTP handler.
-module (fmke_gen_http_handler).

-include ("fmke_http.hrl").

-export ([init/3, handle_req/5, handle_reply/5]).

-callback init(Req::cowboy_req:req(), State::any()) -> {ok, cowboy_req:req(), any()}.
-callback handle_req(Method::binary(), HasBody::boolean(), Req::cowboy_req:req()) -> cowboy_req:req().
-callback perform_operation(Method::binary(), Req::cowboy_req:req(),
                            UrlParamsFound::list({atom(), binary()}),
                            BodyParamsFound::list({atom(), any()}))
          -> cowboy_req:req().

%% Every requests starts being processed at the init function, and processing is
%% identical throughout all modules that implement this behaviour, so this function
%% reduces code duplication.
init(Mod, Req, State) ->
    try
      Method = cowboy_req:method(Req),
      HasBody = cowboy_req:has_body(Req),
      Req1 = Mod:handle_req(Method, HasBody, Req),
      {ok, Req1, State}
    catch
        Class:Reason ->
            lager:error(io_lib:format("Error ~p:~p in request from ~p~n", [Class, Reason, Mod])),
            Req2 = handle_reply(Mod, Req, {error, internal}, false, Reason),
            {ok, Req2, State}
    end.

%% Processing any request has a pattern that involves acquiring necessary parameters
%% from the URL and/or HTTP body, executing an operation on the `fmke` module and
%% replying back with an answer.
-spec handle_req(Mod::atom(), Method::binary(), Req::cowboy_req:req(),
                 UrlParams::list(atom()),
                 BodyParams::list({atom(), integer | string})) -> cowboy_req:req().
handle_req(Mod, <<"GET">>, Req, UrlParams, _) ->
    try
        Bindings = cowboy_req:bindings(Req),
        UrlParamsFound = lists:foldl(fun(Param, Accum) ->
            case maps:get(Param, Bindings, undefined) of
                undefined -> Accum;
                Val -> [{Param, Val} | Accum]
            end
        end, [], UrlParams),
        Mod:perform_operation(<<"GET">>, Req, lists:reverse(UrlParamsFound), [])
    catch
        error:ErrReason ->
            handle_reply(Mod, Req, {error, internal}, false, ErrReason);
        _:ExReason ->
            handle_reply(Mod, Req, {error, internal}, false, ExReason)
    end;

handle_req(Mod, Method, Req, UrlParams, BodyParams) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req),
        Bindings = cowboy_req:bindings(Req1),
        UrlParamsFound = lists:foldl(fun(Param, Accum) ->
            case maps:get(Param, Bindings, undefined) of
                undefined -> Accum;
                Val -> lists:append(Accum, [{Param, Val}])
            end
        end, [], UrlParams),
        BodyParamsFound = fmke_http_utils:parse_body(BodyParams, Body),
        case BodyParamsFound of
            [] ->
                handle_reply(Mod, Req, {error, bad_req}, false, ?ERR_MISSING_BODY);
            _List ->
                case proplists:get_keys(BodyParamsFound) =:= proplists:get_keys(BodyParams) of
                    true ->
                        %% All body params that were requested have been found
                        Mod:perform_operation(Method, Req1, UrlParamsFound, BodyParamsFound);
                    false ->
                        %% Some body parameters are missing, let Mod decide what to do
                        Mod:perform_operation(Method, Req1, UrlParamsFound, {incomplete, BodyParamsFound})
                end
        end
    catch
        error:ErrReason ->
            handle_reply(Mod, Req, {error, internal}, false, ErrReason);
        _:ExReason ->
            handle_reply(Mod, Req, {error, internal}, false, ExReason)
    end.

handle_reply(_Mod, Req, ok, Success, Result) ->
    cowboy_req:reply(200, ?CONT_TYPE_JSON, ?ENCODE_RESPONSE(Success, Result), Req);

handle_reply(_Mod, Req, {error, bad_req}, false, Result) ->
    cowboy_req:reply(400, ?CONT_TYPE_JSON, ?ENCODE_FAIL(Result), Req);

handle_reply(Mod, Req, {error, internal}, false, Reason) ->
    Method = binary_to_list(cowboy_req:method(Req)),
    Uri = binary_to_list(cowboy_req:path(Req)),
    lager:error(io_lib:format("Internal error in ~p for operation ~p ~p: ~p~n", [Mod, Method, Uri, Reason])),
    cowboy_req:reply(500, ?CONT_TYPE_JSON, ?ENCODE_SRV_ERR, Req).
