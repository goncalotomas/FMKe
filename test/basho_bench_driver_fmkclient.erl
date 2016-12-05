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
    zipf_bottom
 }).

%-define(TOURNAMENT_APP, tournament_si_app). Is this needed???

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    %% Make sure the path is setup such that we can get at all required modules
    case code:which(fmk_core) of
        non_existing ->
            ?FAIL_MSG("Client will not run without access to FMK code in the code path.\n",[]);
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

    FmkNode = basho_bench_config:get(fmk_node, 'fmk@127.0.0.1'),
    NumPatients = basho_bench_config:get(numpatients, 5000),
    NumPharmacies = basho_bench_config:get(numpharmacies, 300),
    NumFacilities = basho_bench_config:get(numfacilities, 50),
    NumPrescriptions = basho_bench_config:get(numprescriptions, 2000),
    NumStaff = basho_bench_config:get(numstaff,250),
    %% prepare node for testing
    MyNodeName = lists:flatten(io_lib:format("client~p@127.0.0.1",[Id])),
    net_kernel:start([list_to_atom(MyNodeName),longnames]),
    erlang:set_cookie(node(),antidote),

    %% check if we can connect to the FMK system using distributed erlang.
    case net_adm:ping(FmkNode) of
      pang ->
          ?FAIL_MSG("There is no FMK node online!",[]);
      pong ->
          ok
    end,

    ZipfSize = basho_bench_config:get(zipf_size, 5000),
    ZipfSkew = basho_bench_config:get(zipf_skew, 1),
    {ok,
      #state {
        pid = Id,
        nodename = MyNodeName,
        numpatients = NumPatients,
        numpharmacies = NumPharmacies,
        numstaff = NumStaff,
        numfacilities = NumFacilities,
        numprescriptions = NumPrescriptions,
        fmknode = FmkNode,
        zipf_size = ZipfSize,
        zipf_skew = ZipfSkew,
        zipf_bottom = 1/(lists:foldl(fun(X,Sum) -> Sum+(1/math:pow(X,ZipfSkew)) end,0,lists:seq(1,ZipfSize)))
      }
    }.

run(create_prescription, _GeneratedKey, _GeneratedValue, State) ->
  FmkNode = State#state.fmknode,
  NumPrescriptions = State#state.numprescriptions,
  NumPharmacies = State#state.numpharmacies,
  NumStaff = State#state.numstaff,
  NumPatients = State#state.numpatients,
  NumFacilities = State#state.numfacilities,
  %% to avoid conflicting prescription ids
  MinimumId = 1000000+NumPrescriptions,
  PrescriptionId = rand:uniform(MinimumId),
  PatientId = rand:uniform(NumPatients),
  PrescriberId = rand:uniform(NumStaff),
  PharmacyId = rand:uniform(NumPharmacies),
  FacilityId = rand:uniform(NumFacilities),
  DatePrescribed = "1/1/2016",
  Drugs = ["Adderall","Amitriptyline"],
  %% call create_prescription
  Result = run_op(FmkNode,create_prescription,[
    PrescriptionId,PatientId,PrescriberId,PharmacyId,FacilityId,DatePrescribed,Drugs
  ]),

  case Result of
    ok -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(get_pharmacy_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
  NumPharmacies = State#state.numpharmacies,
  PharmacyId = rand:uniform(NumPharmacies),
  FmkNode = State#state.fmknode,

  Result = run_op(FmkNode,get_pharmacy_prescriptions,[PharmacyId]),
  case Result of
    [] -> {ok,State};
    [_H|_T] -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(get_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
  NumPrescriptions = State#state.numprescriptions,
  PrescriptionId = rand:uniform(NumPrescriptions),
  FmkNode = State#state.fmknode,

  Prescription = run_op(FmkNode,get_prescription_medication,[PrescriptionId]),
  case Prescription of
    [_H|_T] -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(get_staff_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
  FmkNode = State#state.fmknode,
  NumStaff = State#state.numstaff,
  StaffId = rand:uniform(NumStaff),
  Result = run_op(FmkNode,get_staff_prescriptions,[StaffId]),
  case Result of
    [] -> {ok,State};
    [_H|_T] -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(get_processed_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
  FmkNode = State#state.fmknode,
  NumPharmacies = State#state.numpharmacies,
  PharmacyId = rand:uniform(NumPharmacies),
  Result = run_op(FmkNode,get_processed_pharmacy_prescriptions,[PharmacyId]),
  case Result of
    [] -> {ok,State};
    [_H|_T] -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(get_patient, _GeneratedKey, _GeneratedValue, State) ->
  NumPatients = State#state.numpatients,
  PatientId = rand:uniform(NumPatients),
  FmkNode = State#state.fmknode,

  Patient = run_op(FmkNode,get_patient_by_id,[PatientId]),
  case Patient of
    [_H|_T] -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(update_prescription, _GeneratedKey, _GeneratedValue, State) ->
  NumPrescriptions = State#state.numprescriptions,
  PrescriptionId = rand:uniform(NumPrescriptions),
  FmkNode = State#state.fmknode,
  %% the following operation is idempotent
  Result = run_op(FmkNode,process_prescription,[PrescriptionId]),
  case Result of
    ok -> {ok, State};
    {error,prescription_already_processed} -> {ok, State}; % not an operation related error
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(update_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
  NumPrescriptions = State#state.numprescriptions,
  PrescriptionId = rand:uniform(NumPrescriptions),
  FmkNode = State#state.fmknode,
  Drugs = ["Amoxicillin","Ativan","Atorvastatin"],
  Result = run_op(FmkNode,update_prescription_medication,[PrescriptionId,add_drugs,Drugs]),
  case Result of
    ok -> {ok, State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end;

run(get_prescription, _GeneratedKey, _GeneratedValue, State) ->
  NumPrescriptions = State#state.numprescriptions,
  PrescriptionId = rand:uniform(NumPrescriptions),
  FmkNode = State#state.fmknode,

  Prescription = run_op(FmkNode,get_prescription_by_id,[PrescriptionId]),
  case Prescription of
    [_H|_T] -> {ok,State};
    {badrpc,{'EXIT',{{badmatch,{error,{aborted,_Txn}}},_Trace}}} -> {error, txn_aborted, State};
    Error -> {error, Error, State}
  end.

run_op(FmkNode,Op,Params) ->
  rpc:call(FmkNode,fmk_core,Op,Params).

get_prescription_drugs() ->
    case rand:uniform(3) of
        1 -> get_random_drug();
        2 -> [get_random_drug(), get_random_drug()];
        3 -> [get_random_drug(), get_random_drug(), get_random_drug()];
        _ -> get_random_drug()
    end.

get_random_drug() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).
