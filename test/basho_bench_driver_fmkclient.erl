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
    http_connection
 }).

%-define(TOURNAMENT_APP, tournament_si_app). Is this needed???

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure the path is setup such that we can get at all required modules
    case code:which(fmk_core) of
        non_existing ->
            ?FAIL_MSG("Cannot use FMKe code.\n",[]);
        _ ->
            ok
    end,
    case code:which(patient) of
        non_existing ->
            ?FAIL_MSG("Cannot use patient code.\n",[]);
        _ ->
            ok
    end,
    case code:which(pharmacy) of
        non_existing ->
            ?FAIL_MSG("Cannot use pharmacy code.\n",[]);
        _ ->
            ok
    end,
    case code:which(facility) of
        non_existing ->
            ?FAIL_MSG("Cannot use facility code.\n",[]);
        _ ->
            ok
    end,
    case code:which(staff) of
        non_existing ->
            ?FAIL_MSG("Cannot use staff code.\n",[]);
        _ ->
            ok
    end,
    case code:which(prescription) of
        non_existing ->
            ?FAIL_MSG("Cannot use prescription code.\n",[]);
        _ ->
            ok
    end,
    case code:which(treatment) of
        non_existing ->
            ?FAIL_MSG("Cannot use treatment code.\n",[]);
        _ ->
            ok
    end,
    case code:which(event) of
        non_existing ->
            ?FAIL_MSG("Cannot use event code.\n",[]);
        _ ->
            ok
    end,
    case code:which(hackney) of
        non_existing ->
            ?FAIL_MSG("Cannot use HTTP client code.\n",[]);
        _ ->
            ok
    end,

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
    io:format("TARGET NODE IS ########################~p\n",[TargetNode]),
    TargetPort = lists:nth((Id rem length(IPs)+1), Ports),
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
        fmk_server_port = TargetPort,
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
    MinimumId = 1000000+NumPrescriptions,
    _PrescriptionId = rand:uniform(MinimumId),
    _PatientId = rand:uniform(NumPatients),
    _PrescriberId = rand:uniform(NumStaff),
    _PharmacyId = rand:uniform(NumPharmacies),
    _FacilityId = rand:uniform(NumFacilities),
    _DatePrescribed = "1/1/2016",
    _Drugs = gen_prescription_drugs(),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_pharmacy_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
    NumPharmacies = State#state.numpharmacies,
    _PharmacyId = rand:uniform(NumPharmacies),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    _PrescriptionId = rand:uniform(NumPrescriptions),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_staff_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
    NumStaff = State#state.numstaff,
    _StaffId = rand:uniform(NumStaff),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_processed_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
    NumPharmacies = State#state.numpharmacies,
    _PharmacyId = rand:uniform(NumPharmacies),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_patient, _GeneratedKey, _GeneratedValue, State) ->
    NumPatients = State#state.numpatients,
    _PatientId = rand:uniform(NumPatients),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(update_prescription, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    _PrescriptionId = rand:uniform(NumPrescriptions),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(update_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    _PrescriptionId = rand:uniform(NumPrescriptions),
    _Drugs = gen_prescription_drugs(),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(get_prescription, _GeneratedKey, _GeneratedValue, State) ->
    NumPrescriptions = State#state.numprescriptions,
    _PrescriptionId = rand:uniform(NumPrescriptions),

    %%TODO use right address, port and endpoint
    FmkServerAddress = State#state.fmk_server_ip,
    FmkServerPort = State#state.fmk_server_port,
    HttpConn = State#state.http_connection,
    Method = get,
    URL = <<"http://127.0.0.1:9090/patients/1">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Payload = <<>>,
    Req = {Method, URL, Headers, Payload},

    case hackney:send_request(HttpConn,Req) of
        {ok, _Status, _RespHeaders, HttpConn} ->
            {ok, _Body} = hackney:body(HttpConn),
            {ok,State};
        {error, Reason} ->
            {error, Reason, State}
    end.

gen_prescription_drugs() ->
    case rand:uniform(3) of
        1 -> get_random_drug();
        2 -> [get_random_drug(), get_random_drug()];
        3 -> [get_random_drug(), get_random_drug(), get_random_drug()];
        _ -> get_random_drug()
    end.

get_random_drug() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))). % 16 characters
