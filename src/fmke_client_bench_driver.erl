-module(fmke_client_bench_driver).
%
% -export([new/1,
%          run/4]).
%
% -define (TIMEOUT, 5000).
% -define (MAX_ACTIVE_PRESCRIPTIONS, 1000).
% %% Taken from the lasp header file, this is required
% -define(FAIL_MSG(Str, Args), ?ERROR(Str, Args), lasp_bench_app:stop_or_kill()).
% -define(ERROR(Str, Args), lager:error(Str, Args)).
%
% -record(state,
%   {
%     pid,
%     nodename,
%     numpatients,
%     numstaff,
%     numpharmacies,
%     numprescriptions,
%     numfacilities,
%     fmknode,
%     zipf_size,
%     zipf_skew,
%     zipf_bottom,
%     fmk_server_ip,
%     fmk_server_port,
%     created_prescriptions
%  }).
%
% %% ====================================================================
% %% API
% %% ====================================================================
%
% new(Id) ->
%
%     %% read relevant configuration from config file
%     IPs = lasp_bench_config:get(fmke_server_ips,["127.0.0.1"]),
%     Ports = lasp_bench_config:get(fmke_server_ports,[9090]),
%     NumPatients = lasp_bench_config:get(numpatients, 5000),
%     NumPharmacies = lasp_bench_config:get(numpharmacies, 300),
%     NumFacilities = lasp_bench_config:get(numfacilities, 50),
%     NumPrescriptions = lasp_bench_config:get(numprescriptions, 2000),
%     NumStaff = lasp_bench_config:get(numstaff,250),
%
%     ZipfSize = lasp_bench_config:get(zipf_size, 5000),
%     ZipfSkew = lasp_bench_config:get(zipf_skew, 1),
%     ZipfBottom = 1/(lists:foldl(
%         fun(X,Sum) -> Sum+(1/math:pow(X,ZipfSkew)) end,
%         0,lists:seq(1,ZipfSize))
%     ),
%
%     %% make sure that inets is started...
%     InetBootResult = inets:start(),
%     case checkInetsIsRunning(InetBootResult) of
%         ok -> ok;
%         _ -> ?FAIL_MSG("Driver ~p could not start inets, cannot proceed", ?MODULE)
%     end,
%
%     TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
%     io:format("Target FMKe IP address for client ~p is ~p\n",[Id,TargetNode]),
%     TargetPort = lists:nth((Id rem length(IPs)+1), Ports),
%     io:format("Target FMKe port for client ~p is ~p\n",[Id,TargetPort]),
%
%     Host = list_to_binary(TargetNode),
%     Port = TargetPort,
%
%     %% Seed random number
%     rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
%
%     {ok,
%       #state {
%         pid = Id,
%         numpatients = NumPatients,
%         numpharmacies = NumPharmacies,
%         numstaff = NumStaff,
%         numfacilities = NumFacilities,
%         numprescriptions = NumPrescriptions,
%         zipf_size = ZipfSize,
%         zipf_skew = ZipfSkew,
%         zipf_bottom = ZipfBottom,
%         fmk_server_ip = TargetNode,
%         fmk_server_port = integer_to_list(TargetPort),
%         created_prescriptions = queue:new()
%       }
%     }.
%
% run(create_prescription, _GeneratedKey, _GeneratedValue, State) ->
%     NumPrescriptions = State#state.numprescriptions,
%     NumPharmacies = State#state.numpharmacies,
%     NumStaff = State#state.numstaff,
%     NumPatients = State#state.numpatients,
%
%     %% to avoid conflicting prescription ids
%     %%TODO store created prescriptions in a list inside client state.
%     MinimumId = 10000000,
%     PrescriptionId = rand:uniform(MinimumId)+NumPrescriptions,
%     PatientId = rand:uniform(NumPatients),
%     PrescriberId = rand:uniform(NumStaff),
%     PharmacyId = rand:uniform(NumPharmacies),
%     DatePrescribed = "1/1/2016",
%     Drugs = gen_prescription_drugs(),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = post,
%     Path = "prescriptions",
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%
%     Payload = jsx:encode([
%         {id,PrescriptionId},
%         {patient_id,PatientId},
%         {pharmacy_id,PharmacyId},
%         {prescriber_id,PrescriberId},
%         {drugs,Drugs},
%         {date_prescribed,DatePrescribed}
%     ]),
%
%     Req = {Method, URL, Headers, Payload},
%
%
%     fmk_request(HttpConn, Req, State,
%         fun(JsonResponse, State) ->
%             case proplists:get_value(<<"success">>, JsonResponse) of
%                 true ->
%                     {ok,
%                         State#state {
%                             created_prescriptions = queue:in(PrescriptionId, State#state.created_prescriptions)
%                         }};
%                 _ ->
%                     Reason = proplists:get_value(<<"result">>, JsonResponse),
%                     {error, Reason, State}
%             end
%         end
%         );
%
% run(get_pharmacy_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
%     NumPharmacies = State#state.numpharmacies,
%     PharmacyId = rand:uniform(NumPharmacies),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = get,
%     Path = "pharmacies/" ++ integer_to_list(PharmacyId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = <<>>,
%     Req = {Method, URL, Headers, Payload},
%
%     fmk_request(HttpConn, Req, State);
%
% run(get_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
%     NumPrescriptions = State#state.numprescriptions,
%     PrescriptionId = rand:uniform(NumPrescriptions),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = get,
%     Path = "prescriptions/" ++ integer_to_list(PrescriptionId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = <<>>,
%     Req = {Method, URL, Headers, Payload},
%
%     fmk_request(HttpConn, Req, State);
%
% run(get_staff_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
%     NumStaff = State#state.numstaff,
%     StaffId = rand:uniform(NumStaff),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = get,
%     Path = "staff/" ++ integer_to_list(StaffId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = <<>>,
%     Req = {Method, URL, Headers, Payload},
%
%     fmk_request(HttpConn, Req, State);
%
% run(get_processed_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
%     NumPharmacies = State#state.numpharmacies,
%     PharmacyId = rand:uniform(NumPharmacies),
%
%     %%TODO this is fetching all prescriptions, there is no endpoint to fetch processed prescriptions
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = get,
%     Path = "pharmacies/" ++ integer_to_list(PharmacyId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = <<>>,
%     Req = {Method, URL, Headers, Payload},
%
%     fmk_request(HttpConn, Req, State);
%
% run(get_patient, _GeneratedKey, _GeneratedValue, State) ->
%     NumPatients = State#state.numpatients,
%     PatientId = rand:uniform(NumPatients),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = get,
%     Path = "patients/" ++ integer_to_list(PatientId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = <<>>,
%     Req = {Method, URL, Headers, Payload},
%
%     fmk_request(HttpConn, Req, State);
%
% run(update_prescription, _GeneratedKey, _GeneratedValue, State) ->
%     NumPrescriptions = State#state.numprescriptions,
%     {OldestCreatedPrescription, NewCreatedPrescriptions} = queue:out(State#state.created_prescriptions),
%     PrescriptionId =
%         case OldestCreatedPrescription of
%             {value, Id} -> Id;
%             empty -> rand:uniform(NumPrescriptions)
%         end,
%
%     DateProcessed = get_random_date(),
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = put,
%     Path = "prescriptions/" ++ integer_to_list(PrescriptionId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = jsx:encode([{date_processed,DateProcessed}]),
%     Req = {Method, URL, Headers, Payload},
%
%     Res = fmk_request(HttpConn, Req, State,
%         fun(Json, State) ->
%             case proplists:get_value(<<"success">>, Json) of
%                 true ->
%                     {ok,
%                         State#state {
%                             created_prescriptions = NewCreatedPrescriptions
%                         }};
%                 _ ->
%                     Reason = proplists:get_value(<<"result">>, Json),
%                     {error, Reason, State}
%             end
%         end),
%     case Res of
%         {ok, NewState} ->
%             case queue:len(NewCreatedPrescriptions) > ?MAX_ACTIVE_PRESCRIPTIONS of
%                 true ->
%                     % remove more prescriptions
%                     run(update_prescription, 0, 0, NewState);
%                 false ->
%                     Res
%             end;
%         _ ->
%             Res
%     end;
%
% run(update_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
%     NumPrescriptions = State#state.numprescriptions,
%     PrescriptionId = rand:uniform(NumPrescriptions),
%     Drugs = gen_prescription_drugs(),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = put,
%     Path = "prescriptions/" ++ integer_to_list(PrescriptionId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = jsx:encode([{drugs,Drugs}]),
%     Req = {Method, URL, Headers, Payload},
%
%     Res = fmk_request(HttpConn, Req, State,
%         fun(Json, State) ->
%             case proplists:get_value(<<"success">>, Json) of
%                 true ->
%                     {ok,
%                         State#state {
%                             created_prescriptions = State#state.created_prescriptions
%                         }};
%                 _ ->
%                     Reason = proplists:get_value(<<"result">>, Json),
%                     {error, Reason, State}
%             end
%         end),
%     Res;
%
% run(get_prescription, _GeneratedKey, _GeneratedValue, State) ->
%     NumPrescriptions = State#state.numprescriptions,
%     PrescriptionId = rand:uniform(NumPrescriptions),
%
%     FmkServerAddress = State#state.fmk_server_ip,
%     FmkServerPort = State#state.fmk_server_port,
%     HttpConn = State#state.http_connection,
%     Method = get,
%     Path = "prescriptions/" ++ integer_to_list(PrescriptionId),
%     URL = generate_url(FmkServerAddress,FmkServerPort,Path),
%     Headers = [{<<"Connection">>, <<"keep-alive">>}],
%     Payload = <<>>,
%     Req = {Method, URL, Headers, Payload},
%
%     fmk_request(HttpConn, Req, State).
%
% checkInetsIsRunning(ok) ->
%     ok;
% checkInetsIsRunning({error,{already_started,inets}}) ->
%     ok;
% checkInetsIsRunning(_OtherResult) ->
%     ko.
%
% decode_json(Body) ->
%     try
%         jsx:decode(Body)
%     catch
%         error:Err ->
%             io:format("JSON error ~p~nfor JSON:~n~p~n~p~n~n~n", [Err, Body, erlang:get_stacktrace()]),
%             []
%     end.
%
% generate_url(Address,Port,Path) ->
%     % for debugging:
%     true = io_lib:printable_unicode_list(Address),
%     true = io_lib:printable_unicode_list(Port),
%     true = io_lib:printable_unicode_list(Path),
%   list_to_binary("http://" ++ Address ++ ":" ++ Port ++ "/" ++ Path).
%
% gen_prescription_drugs() ->
%     case rand:uniform(3) of
%         1 -> get_random_drug();
%         2 -> get_random_drug() ++ "," ++ get_random_drug();
%         3 -> get_random_drug() ++ "," ++ get_random_drug() ++ "," ++ get_random_drug();
%         _ -> get_random_drug()
%     end.
%
% get_random_drug() ->
%     integer_to_list(rand:uniform(10)).
%
% get_random_date() ->
%     binary_to_list(base64:encode(crypto:strong_rand_bytes(8))). % 8 character "date"
%
% fmk_request(HttpConn, Req, State) ->
%     fmk_request(HttpConn, Req, State, fun simple_response_handler/2).
%
% fmk_request(_HttpConn, Req, State, Handler) ->
%     {Method, URL, Headers, Payload} = Req,
%     Options = [],
%     case hackney:request(Method, URL,Headers, Payload, Options) of
%         {ok, 200, _RespHeaders, HttpConn} ->
%             {ok, Body} = hackney:body(HttpConn),
%             Json = decode_json(Body),
%             Handler(Json, State);
%         {ok, Status, _RespHeaders, HttpConn} ->
%             {ok, Body} = hackney:body(HttpConn),
%             {error, {request_failed, Status, split_lines(binary_to_list(Body))}, State};
%         {error, Reason} ->
%             {error, {send_request_failed, Reason, Req}, State}
%     end.
%
% split_lines([]) -> [];
% split_lines(S) ->
%     case lists:splitwith(fun(C) -> C /= 10 end, S) of
%         {Start, []} -> [Start];
%         {Start, [10|End]} -> [Start|split_lines(End)]
%     end.
%
% simple_response_handler(Json, State) ->
%     case proplists:get_value(<<"success">>, Json) of
%         true ->
%             {ok, State};
%         _ ->
%             {error, Json, State}
%     end.
