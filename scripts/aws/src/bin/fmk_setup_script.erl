#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie antidote -mnesia debug verbose
-mode(compile).
-define(ZIPF_SKEW, 1).
-define(NUMTHREADS, 10).


-record(fmkconfig, {
  numpatients,
  numpharmacies,
  numfacilities,
  numstaff,
  numprescriptions
}).

main([MyNodeRef, FmkNodeRef]) ->
  % io:format("Loading external libs...~n"),
  % true = code:add_pathz(filename:dirname(escript:script_name())
  %                      ++ "/../_build/default/lib/hackney/ebin"),
  % true = code:add_pathz(filename:dirname(escript:script_name())
  %                     ++ "/../_build/default/lib/idna/ebin"),
  % true = code:add_pathz(filename:dirname(escript:script_name())
  %                     ++ "/../_build/default/lib/mimerl/ebin"),
  % true = code:add_pathz(filename:dirname(escript:script_name())
  %                     ++ "/../_build/default/lib/certifi/ebin"),
  % true = code:add_pathz(filename:dirname(escript:script_name())
  %                     ++ "/../_build/default/lib/metrics/ebin"),
  % true = code:add_pathz(filename:dirname(escript:script_name())
  %                     ++ "/../_build/default/lib/ssl_verify_fun/ebin"),
  % io:format("Loaded external libs.~n"),
  DirName = filename:dirname(escript:script_name()),
  FileName = DirName ++ "/basho_bench/examples/fmkclient.config",
  io:format("Checking configuration file ~p~n",[FileName]),
  {ok, FmkConfigProps} = file:consult(FileName),
  FmkConfig = #fmkconfig{
    numpatients = proplists:get_value(numpatients, FmkConfigProps),
    numpharmacies = proplists:get_value(numpharmacies, FmkConfigProps),
    numfacilities = proplists:get_value(numfacilities, FmkConfigProps),
    numstaff = proplists:get_value(numstaff, FmkConfigProps),
    numprescriptions = proplists:get_value(numprescriptions, FmkConfigProps)
  },
  % io:format("initializing hackney HTTP client...~n",[]),
  % hackney:start(),
  % io:format("initialized hackney HTTP client."),
  MyNodeName = list_to_atom(MyNodeRef),
  FmkNode = list_to_atom(FmkNodeRef),
  io:format("client node is ~p.\n", [MyNodeName]),
  io:format("fmk node target set as ~p.\n", [FmkNode]),
  net_kernel:start([MyNodeName, longnames]),
  erlang:set_cookie(node(), antidote),
  %% check if fmk is running
  case net_adm:ping(FmkNode) of
    pang ->
      io:format("cannot connect to fmk.\n", []);
    pong ->
      ok
  end,
  io:format("populating antidote...\n", []),
  add_patients(FmkNode, FmkConfig#fmkconfig.numpatients),
  add_pharmacies(FmkNode, FmkConfig#fmkconfig.numpharmacies),
  add_facilities(FmkNode, FmkConfig#fmkconfig.numfacilities),
  add_staff(FmkNode, FmkConfig#fmkconfig.numstaff),
  add_prescription(FmkNode, FmkConfig#fmkconfig.numprescriptions, FmkConfig),
  io:format("finished populating antidote.\n", []);
main(_) ->
  usage().

usage() ->
  io:format("usage: node_name fmk_node_name\n"),
  halt(1).


parallel_create(Name, First, Last, NumThreads, Fun) ->
  Count = 1 + Last - First,
  PerDivision = Count div NumThreads,
  NumDivisions = Count div PerDivision,
  Divisions = [{First + I * PerDivision, case I + 1 of NumDivisions -> Last; _ ->
    First + (I + 1) * PerDivision - 1 end} || I <- lists:seq(0, NumDivisions - 1)],
  [{F1, L1} | OtherDivisions] = Divisions,
  parallel_create_h(Name, F1, L1, self(), Fun),
  [parallel_create_h(F, L, self(), Fun) || {F, L} <- OtherDivisions],
  [receive {done, F, L} -> ok end || {F, L} <- Divisions],
  ok.

parallel_create_h(Name, First, Last, Pid, Fun) ->
  Count = (1 + Last - First),
  case Count > 0 of
    false -> ok;
    true ->
      spawn(
        fun() ->
          Fun2 =
            fun(I) ->
              case (I - First) rem max(1, Count div 100) of
                0 ->
                  io:format("Creating ~p ~p%~n", [Name, 100 * (I - First) / Count]);
                _ ->
                  ok
              end,
              Fun(I)
            end,
          lists:map(Fun2, lists:seq(First, Last)),
          Pid ! {done, First, Last}
        end)
  end.

parallel_create_h(First, Last, Pid, Fun) ->
  spawn(
    fun() ->
      lists:map(Fun, lists:seq(First, Last)),
      Pid ! {done, First, Last}
    end).



add_pharmacies(FmkNode, Amount) ->
  parallel_create(pharmacies, 1, Amount, ?NUMTHREADS,
    fun(I) ->
      case I rem 3 of
        0 -> run_op(FmkNode, create_pharmacy, [I, "Chai Pharmacy", "Costa da Caparica, Portugal"]);
        1 -> run_op(FmkNode, create_pharmacy, [I, "Carlos Pharmacy", "Costa da Caparica, Portugal"]);
        2 -> run_op(FmkNode, create_pharmacy, [I, "Shanghai Central Pharmacy", "Shanghai, China"])
      end
    end).

add_facilities(FmkNode, Amount) ->
  parallel_create(facilities, 1, Amount, ?NUMTHREADS,
    fun(I) ->
      case I rem 10 of
        0 -> run_op(FmkNode, create_facility, [I, "Amager Hospital", "Amager Island, DK", "Hospital"]);
        1 -> run_op(FmkNode, create_facility, [I, "Bispebjerg Hospital", "Copenhagen, DK", "Hospital"]);
        2 -> run_op(FmkNode, create_facility, [I, "Bornholms Hospital", "Bornholms Island, DK", "Hospital"]);
        3 -> run_op(FmkNode, create_facility, [I, "Gentofte Hospital", "Gentofte, DK", "Hospital"]);
        4 -> run_op(FmkNode, create_facility, [I, "Glostrup Hospital", "Glostrup, DK", "Hospital"]);
        5 -> run_op(FmkNode, create_facility, [I, "Herlev Hospital", "Herlev, DK", "Hospital"]);
        6 -> run_op(FmkNode, create_facility, [I, "Nordsjællands Hospital", "Esbønderup, DK", "Hospital"]);
        7 -> run_op(FmkNode, create_facility, [I, "Privathospitalet Danmark", "Charlottenlund, DK", "Hospital"]);
        8 -> run_op(FmkNode, create_facility, [I, "Rigshospitalet", "Copenhagen, DK", "Hospital"]);
        9 -> run_op(FmkNode, create_facility, [I, "Sct. Hans Hospital", "Zealand Island, DK", "Hospital"])
      end
    end).

add_patients(FmkNode, Amount) ->
  parallel_create(patient, 1, Amount, 10,
    fun(I) ->
      run_op(FmkNode, create_patient, [I, "Phineas Gage", "New Hampshire, United States"])
    end).

add_staff(FmkNode, Amount) ->
  parallel_create(staff, 1, Amount, ?NUMTHREADS,
    fun(I) ->
      run_op(FmkNode, create_staff, [I, "Alexander Fleming", "London, UK", "Pharmacologist"])
    end).

add_prescription(_FmkNode, 0, _FmkConfig) -> ok;
add_prescription(FmkNode, Amount, FmkConfig) when Amount > 0 ->
  ListPatientIds = gen_sequence(FmkConfig#fmkconfig.numpatients, ?ZIPF_SKEW, FmkConfig#fmkconfig.numprescriptions),
  add_prescription_rec(FmkNode, Amount, ListPatientIds, FmkConfig).

add_prescription_rec(_FmkNode, 0, _ListPatients, _FmkConfig) -> ok;
add_prescription_rec(FmkNode, PrescriptionId, ListPatientIds, FmkConfig) ->
  [CurrentId | Tail] = ListPatientIds,
  PharmacyId = rand:uniform(FmkConfig#fmkconfig.numpharmacies),
  PrescriberId = rand:uniform(FmkConfig#fmkconfig.numstaff),
  FacilityId = rand:uniform(FmkConfig#fmkconfig.numfacilities),
  run_op(FmkNode, create_prescription, [PrescriptionId, CurrentId, PrescriberId, PharmacyId, FacilityId, "1/1/2017", ["Acetaminophen"]]),
  add_prescription_rec(FmkNode, PrescriptionId - 1, Tail, FmkConfig).

run_op(FmkNode, create_pharmacy, Params) ->
  [_Id, _Name, _Address] = Params,
  run_rpc_op(FmkNode, create_pharmacy, Params);
run_op(FmkNode, create_facility, Params) ->
  [_Id, _Name, _Address, _Type] = Params,
  run_rpc_op(FmkNode, create_facility, Params);
run_op(FmkNode, create_patient, Params) ->
  [_Id, _Name, _Address] = Params,
  run_rpc_op(FmkNode, create_patient, Params);
run_op(FmkNode, create_staff, Params) ->
  [_Id, _Name, _Address, _Speciality] = Params,
  run_rpc_op(FmkNode, create_staff, Params);
run_op(FmkNode, create_prescription, Params) ->
  [_PrescriptionId, _PatientId, _PrescriberId, _PharmacyId, _FacilityId, _DatePrescribed, _Drugs] = Params,
  run_rpc_op(FmkNode, create_prescription, Params).

run_rpc_op(FmkNode, Op, Params) ->
  ok = case rpc:call(FmkNode, fmk_core, Op, Params) of
         {error, Reason} ->
           io:format("Error in ~p with params ~p\n", [Op, Params]),
           {error, Reason};
         ok -> ok
       end.

gen_sequence(Size, Skew, SequenceSize) ->
  Bottom = 1 / (lists:foldl(fun(X, Sum) -> Sum + (1 / math:pow(X, Skew)) end, 0, lists:seq(1, Size))),
  lists:map(fun(_X) ->
    zipf_next(Size, Skew, Bottom)
            end, lists:seq(1, SequenceSize)).

zipf_next(Size, Skew, Bottom) ->
  Dice = rand:uniform(),
  next(Dice, Size, Skew, Bottom, 0, 1).

next(Dice, _Size, _Skew, _Bottom, Sum, CurrRank) when Sum >= Dice -> CurrRank - 1;
next(Dice, Size, Skew, Bottom, Sum, CurrRank) ->
  NextRank = CurrRank + 1,
  Sumi = Sum + (Bottom / math:pow(CurrRank, Skew)),
  next(Dice, Size, Skew, Bottom, Sumi, NextRank).
