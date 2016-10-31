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
    numpharmacies,
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
    %% Make sure the path is setup such that we can get at riak_client
    case code:which(fmk_core) of
        non_existing ->
            ?FAIL_MSG("Client will not run without access to FMK code in the code path.\n",[]);
        _ ->
            ok
    end,

    FmkNode = basho_bench_config:get(fmk_node, 'fmk@127.0.0.1'),
    NumPatients = basho_bench_config:get(numpatients, 5000),
    NumPharmacies = basho_bench_config:get(numpharmacies, 300),
    NumFacilities = basho_bench_config:get(numfacilities, 50),
    %% prepare node for testing
    MyNodeName = lists:flatten(io_lib:format("client~p@127.0.0.1",[Id])),
    net_kernel:start([list_to_atom(MyNodeName),longnames]),
    erlang:set_cookie(node(),antidote),

    %% check if we can connect to the FMK system using distributed erlang.
    case net_adm:ping('fmk@127.0.0.1') of
      pang ->
          ?FAIL_MSG("There is no FMK node online!",[]);
      pong ->
          ok
    end,

    ZipfSize = basho_bench_config:get(zipf_size, 5000),
    ZipfSkew = basho_bench_config:get(zipf_skew, 10),
    {ok,
      #state {
        pid = Id,
        nodename = MyNodeName,
        numpatients = NumPatients,
        numpharmacies = NumPharmacies,
        numfacilities = NumFacilities,
        fmknode = FmkNode,
        zipf_size = ZipfSize,
        zipf_skew = ZipfSkew,
        zipf_bottom = 1/(lists:foldl(fun(X,Sum) -> Sum+(1/math:pow(X,ZipfSkew)) end,0,lists:seq(1,ZipfSize)))
      }
    }.

run(create_prescription, _GeneratedKey, _GeneratedValue, State) ->

  {ok,State};

run(get_pharmacy_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
  NumPharmacies = State#state.numpharmacies,
  PharmacyId = rand:uniform(NumPharmacies),
  FmkNode = State#state.fmknode,

  Pharmacy = run_op(FmkNode,get_pharmacy_by_id,[PharmacyId]), %% TODO change
  case Pharmacy of
    {error, _} -> {error, State};
    _ -> {ok,State}
  end;

run(get_prescriptionMeds, _GeneratedKey, _GeneratedValue, State) ->
  pong = net_adm:ping(State#state.fmknode),
  {ok,State};

run(get_staff_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
  pong = net_adm:ping(State#state.fmknode),
  {ok,State};

run(get_processed_prescriptions, _GeneratedKey, _GeneratedValue, State) ->
  pong = net_adm:ping(State#state.fmknode),
  {ok,State};

run(get_patient, _GeneratedKey, _GeneratedValue, State) ->
  NumPatients = State#state.numpatients,
  PatientId = rand:uniform(NumPatients),
  FmkNode = State#state.fmknode,

  Patient = run_op(FmkNode,get_patient_by_id,[PatientId]),
  case Patient of
    {error, _} -> {error, State};
    _ -> {ok,State}
  end;

run(update_prescription, _GeneratedKey, _GeneratedValue, State) ->
  pong = net_adm:ping(State#state.fmknode),
  {ok,State};

run(update_prescription_medication, _GeneratedKey, _GeneratedValue, State) ->
  pong = net_adm:ping(State#state.fmknode),
  {ok,State};

run(get_prescription, _GeneratedKey, _GeneratedValue, State) ->
  pong = net_adm:ping(State#state.fmknode),
  {ok,State};

run(_, _GeneratedKey, _GeneratedValue, State) ->
  {error, undefined_op}.

run_op(FmkNode,Op,Params) ->
  rpc:call(FmkNode,fmk,Op,Params).
