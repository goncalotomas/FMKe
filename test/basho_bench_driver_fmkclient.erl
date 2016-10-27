-module(basho_bench_driver_fmkclient).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, {
  %% TODO
}).

%-define(TOURNAMENT_APP, tournament_si_app). Is this needed???

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    ?INFO("Starting FMK Client ~p...", [Id]),
    %% Make sure the path is setup such that we can get at riak_client
    case code:which(fmk_core) of
        non_existing ->
            ?FAIL_MSG("Client will not run without access to FMK code in the code path.\n",[]);
        _ ->
            ok
    end,

    FmkNode = basho_bench_config:get(fmk_node, 'fmk@127.0.0.1'),
    %% prepare node for testing
    MyNodeName = lists:flatten(io_lib:format('client~p@127.0.0.1',[Id])),
    net_kernel:start([MyNodeName,longnames]),
    erlang:set_cookie(node(),antidote),

    %% check if antidote is running
    case net_adm:ping('antidote@127.0.0.1') of
      pang ->
          ?FAIL_MSG("There is no Antidote node online!",[]),
      pong ->
          ok
    end,
    %% check if fmk is running
    case net_adm:ping('fmk@127.0.0.1') of
      pang ->
          ?FAIL_MSG("There is no FMK node online!",[]),
      pong ->
          ok
    end,

    ?INFO("Using FMK node ~p...",[FmkNode]),

    GetPharmacyPrescriptionsPercent = basho_bench_config:get(op_percentage_get_pharmacy_prescriptions, 27),
    GetPrescriptionMedsPercent = basho_bench_config:get(op_percentage_get_prescription_medication, 27),
    GetStaffPrescriptionsPercent = basho_bench_config:get(op_percentage_get_staff_prescriptions, 14),
    CreatePrescriptionPercent = basho_bench_config:get(op_percentage_create_prescription, 8),
    GetProcessedPrescriptionsPercent = basho_bench_config:get(op_percentage_get_processed_prescriptions, 7),
    GetPatientPercent = basho_bench_config:get(op_percentage_get_patient, 5),
    UpdatePrescriptionPercent = basho_bench_config:get(op_percentage_update_prescription, 4),
    UpdatePrescriptionMedicationPercent = basho_bench_config:get(op_percentage_update_prescription_medication, 4),
    GetPrescriptionPercent = basho_bench_config:get(op_percentage_get_prescription, 4),


    %% O problema aqui e que ha uma percentagem de operacoes de create prescription mas tambem se
    %% deve seguir uma distribuicao Zipf de receitas (poucos pacientes com muitas receitas, e vice versa)
    %% nao estou a conseguir chegar a uma solucao.
    {ok, ok}.


run(Operation, _KeyGen, _ValueGen, State) ->
  run_op(Operation, []).

run(create_prescription, _1, _2, State) ->
  ok.

run(get_pharmacy_prescriptions, _1, _2, State) ->
  ok.

run(get_prescriptionMeds, _1, _2, State) ->
  ok.

run(get_staff_prescriptions, _1, _2, State) ->
  ok.

run(get_processed_prescriptions, _1, _2, State) ->
  ok.

run(get_patient, _1, _2, State) ->
  ok.

run(update_prescription, _1, _2, State) ->
  ok.

run(update_prescription_medication, _1, _2, State) ->
  ok.

run(get_prescription, _1, _2, State) ->
  ok.

run_op(Op,Params) -> rpc:call('fmk@127.0.0.1',fmk,Op,Params) end.
