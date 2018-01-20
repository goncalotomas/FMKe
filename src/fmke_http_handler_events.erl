-module (fmke_http_handler_events).
% -include ("fmk_http.hrl").
%
% -export([init/2]).
%
% init(Req0, Opts) ->
%   try
%     Method = cowboy_req:method(Req0),
%     HasBody = cowboy_req:has_body(Req0),
%     Req = handle_req(Method, HasBody, Req0),
%     {ok, Req, Opts}
%   catch
%     Err:Reason ->
%       ErrorMessage = io_lib:format("Error ~p:~n~p~n~p~n", [Err, Reason, erlang:get_stacktrace()]),
%       lager:error(ErrorMessage),
%       Req2 = cowboy_req:reply(500, #{}, ErrorMessage, Req0),
%       {ok, Req2, Opts}
%   end.
%
% handle_req(<<"POST">>, true, Req) ->
%     create_event(Req);
% handle_req(<<"POST">>, false, Req) ->
%     cowboy_req:reply(400, #{}, ?ERR_MISSING_BODY, Req);
%
% handle_req(<<"GET">>, true, Req) ->
%     cowboy_req:reply(400, #{}, ?ERR_BODY_IN_A_GET_REQUEST, Req);
% handle_req(<<"GET">>, false, Req) ->
%     get_event(Req).
%
% create_event(Req) ->
%     {ok, Data, _Req2} = cowboy_req:read_body(Req),
%     Json = jsx:decode(Data),
%     EventId = proplists:get_value(<<"id">>, Json),
%     TreatmentId = proplists:get_value(<<"treatment_id">>, Json),
%     StaffMemberId = proplists:get_value(<<"staff_id">>, Json),
%     Timestamp = proplists:get_value(<<"timestamp">>, Json),
%     Description = proplists:get_value(<<"description">>, Json),
%     IntegerId = binary_to_integer(EventId),
%     case IntegerId =< ?MIN_ID of
%         true ->
%             cowboy_req:reply(400, #{}, ?ERR_INVALID_EVENT_ID, Req);
%         false ->
%             IntegerTreatmentId = binary_to_integer(TreatmentId),
%             IntegerStaffId = binary_to_integer(StaffMemberId),
%             StringTimestamp = binary_to_list(Timestamp),
%             StringDescription = binary_to_list(Description),
%             Resp = fmke:create_event(IntegerId, IntegerTreatmentId,
%                                           IntegerStaffId, StringTimestamp, StringDescription),
%             Success = Resp =:= ok,
%             JsonReply =  lists:flatten(io_lib:format(
%                 "{\"success\": \"~p\", \"result\": \"~p\"}",
%                 [Success, Resp]
%             )),
%             cowboy_req:reply(200, #{
%                 <<"content-type">> => <<"application/json">>
%             }, JsonReply, Req)
%     end.
%
% get_event(Req) ->
%     Id = cowboy_req:binding(?BINDING_EVENT_ID, Req, -1),
%     IntegerId = binary_to_integer(Id),
%     case IntegerId =< ?MIN_ID of
%         true ->
%             cowboy_req:reply(400, #{}, ?ERR_INVALID_EVENT_ID, Req);
%         false ->
%             ServerResponse = fmke:get_event_by_id(IntegerId),
%             Success = ServerResponse =/= {error, not_found},
%             JsonReply = case Success of
%                 true ->
%                     lists:flatten(io_lib:format(
%                         ("{\"success\": \"~p\", \"result\": " ++ fmke_proplists:encode(event, ServerResponse) ++ "}"),
%                         [Success]
%                     ));
%                 false ->
%                     lists:flatten(io_lib:format(
%                         "{\"success\": \"~p\", \"result\": \"~p\"}",
%                         [Success, ServerResponse]
%                     ))
%             end,
%             cowboy_req:reply(200, #{
%                 <<"content-type">> => <<"application/json">>
%             }, JsonReply, Req)
%     end.
