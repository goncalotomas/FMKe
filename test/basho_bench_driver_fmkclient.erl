-module(basho_bench_driver_fmkclient).

-export([new/1,
         run/4]).

-include("../include/basho_bench.hrl").

-define (TIMEOUT, 5000).

-record(state,
  {
    pid,
    nodename,
    numpatients,
    numstaff,
    numpharmacies,
    numprescriptions,
    numfacilities,
    fmknode,
    zipf_size,
    zipf_skew,
    zipf_bottom,
    fmk_server_ip,
    fmk_server_port,
    http_connection,
    created_prescriptions
 }).

%-define(TOURNAMENT_APP, tournament_si_app). Is this needed???

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->

    %% read relevant configuration from config file
    IPs = basho_bench_config:get(fmk_server_ips,["127.0.0.1"]),
    Ports = basho_bench_config:get(fmk_server_ports,[9090]),
    NumPatients = basho_bench_config:get(numpatients, 5000),
    NumPharmacies = basho_bench_config:get(numpharmacies, 300),
    NumFacilities = basho_bench_config:get(numfacilities, 50),
    NumPrescriptions = basho_bench_config:get(numprescriptions, 2000),
    NumStaff = basho_bench_config:get(numstaff,250),

    ZipfSize = basho_bench_config:get(zipf_size, 5000),
    ZipfSkew = basho_bench_config:get(zipf_skew, 1),
    ZipfBottom = 1/(lists:foldl(
        fun(X,Sum) -> Sum+(1/math:pow(X,ZipfSkew)) end,
        0,lists:seq(1,ZipfSize))
    ),

    hackney:start(),

    TargetNode = lists:nth((Id rem length(IPs)+1), IPs),
    io:format("Target FMKe IP address for client ~p is ~p\n",[Id,TargetNode]),
    TargetPort = lists:nth((Id rem length(IPs)+1), Ports),
    io:format("Target FMKe port for client ~p is ~p\n",[Id,TargetPort]),
    Transport = hackney_tcp,
    Host = list_to_binary(TargetNode),
    Port = TargetPort,
    Options = [{pool, default}],
    {ok, ConnRef} = hackney:connect(Transport, Host, Port, Options),

    {ok,
      #state {
        pid = Id,
        numpatients = NumPatients,
        numpharmacies = NumPharmacies,
        numstaff = NumStaff,
        numfacilities = NumFacilities,
        numprescriptions = NumPrescriptions,
        zipf_size = ZipfSize,
        zipf_skew = ZipfSkew,
        zipf_bottom = ZipfBottom,
        fmk_server_ip = TargetNode,
        fmk_server_port = integer_to_list(TargetPort),
        http_connection = ConnRef
      }
    }.

run(create_prescription, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    NumPharmacies = State#state.numpharmacies,
    NumStaff = State#state.numstaff,
    NumPatients = State#state.numpatients,
    NumFacilities = State#state.numfacilities,

    %% to avoid conflicting prescription ids
    %%TODO store created prescriptions in a list inside client state.
    MinimumId = 10000000,
    PrescriptionId = rand:uniform(MinimumId)+NumPrescriptions,
    PatientId = rand:uniform(NumPatients),
    PrescriberId = rand:uniform(NumStaff),
    PharmacyId = rand:uniform(NumPharmacies),
    FacilityId = rand:uniform(NumFacilities),
    DatePrescribed = "1/1/2016",
    Drugs = gen_prescription_drugs(),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = post,
    Path = "prescriptions",
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],

    Payload = jsx:encode([
        {id,PrescriptionId},
        {facility_id,FacilityId},
        {patient_id,PatientId},
        {pharmacy_id,PharmacyId},
        {prescriber_id,PrescriberId},
        {drugs,Drugs},
        {date_prescribed,DatePrescribed}
    ]),

    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_pharmacy_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
    NumPharmacies = State#state.numpharmacies,
    PharmacyId = integer_to_list(rand:uniform(NumPharmacies)),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    Path = "pharmacies/" ++ PharmacyId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    PrescriptionId = integer_to_list(rand:uniform(NumPrescriptions)),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    Path = "prescriptions/" ++ PrescriptionId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_staff_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
    NumStaff = State#state.numstaff,
    StaffId = integer_to_list(rand:uniform(NumStaff)),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    Path = "staff/" ++ StaffId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_processed_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
    NumPharmacies = State#state.numpharmacies,
    _PharmacyId = rand:uniform(NumPharmacies),

    %%TODO this is fetching all prescriptions, there is no endpoint to fetch processed prescriptions
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = list_to_binary("http://" ++ FmkServerAddress ++ ":" ++ FmkServerPort ++ "/patients/1"),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_patient, _GeneratedKey, _GeneratedValue, State) ->
    NumPatients = State#state.numpatients,
    PatientId = integer_to_list(rand:uniform(NumPatients)),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    Path = "patients/" ++ PatientId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(update_prescription, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    PrescriptionId = integer_to_list(rand:uniform(NumPrescriptions)),
    DateProcessed = get_random_date(),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = put,
    Path = "prescriptions/" ++ PrescriptionId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = jsx:encode([{date_processed,DateProcessed}]),
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(update_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    PrescriptionId = integer_to_list(rand:uniform(NumPrescriptions)),
    Drugs = gen_prescription_drugs(),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = put,
    Path = "prescriptions/" ++ PrescriptionId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = jsx:encode([{drugs,Drugs}]),
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_prescription, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    PrescriptionId = integer_to_list(rand:uniform(NumPrescriptions)),

    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    Path = "prescriptions/" ++ PrescriptionId,
    URL = generate_url(FmkServerAddress,FmkServerPort,Path),
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, Body} = hackney:body(HttpConn),
            Json = jsx:decode(Body),
            case proplists:get_value(<<"success">>, Json) of
                <<"true">> ->
                      {ok,State};
                _ ->
                  Reason = proplists:get_value(<<"result">>, Json),
                  {error, Reason, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end.

generate_url(Address,Port,Path) ->
  list_to_binary("http://" ++ Address ++ ":" ++ Port ++ "/" ++ Path).

gen_prescription_drugs() ->
    case rand:uniform(3) of
        1 -> get_random_drug();
        2 -> get_random_drug() ++ "," ++ get_random_drug();
        3 -> get_random_drug() ++ "," ++ get_random_drug() ++ "," ++ get_random_drug();
        _ -> get_random_drug()
    end.

get_random_drug() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))). % 16 characters

get_random_date() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(8))). % 8 character "date"
