#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie fmke -mnesia debug verbose
-mode(compile).
-define(ZIPF_SKEW, 1).
-define(NUMTHREADS, 2).


-record(fmkeconfig, {
  numpatients,
  numpharmacies,
  numfacilities,
  numstaff,
  numprescriptions
}).

main([Database, ConfigFile, FmkNodeRef]) ->
  DirName = filename:dirname(escript:script_name()),
  {ok, FmkConfigProps} = file:consult(DirName ++ "/../" ++ ConfigFile),
  FmkConfig = #fmkeconfig{
    numpatients = proplists:get_value(numpatients, FmkConfigProps),
    numpharmacies = proplists:get_value(numpharmacies, FmkConfigProps),
    numfacilities = proplists:get_value(numfacilities, FmkConfigProps),
    numstaff = proplists:get_value(numstaff, FmkConfigProps),
    numprescriptions = proplists:get_value(numprescriptions, FmkConfigProps)
  },

  MyNodeName = "fmke_populator@127.0.0.1",
  FmkNode = list_to_atom(FmkNodeRef),
  io:format("Node name set to ~p.\n", [MyNodeName]),
  io:format("Target FMKe node set to ~p.\n", [FmkNode]),
  io:format("The population script is going to create the following entities:~n",[]),
  io:format("-~p patients~n",[FmkConfig#fmkeconfig.numpatients]),
  io:format("-~p pharmacies~n",[FmkConfig#fmkeconfig.numpharmacies]),
  io:format("-~p hospitals~n",[FmkConfig#fmkeconfig.numfacilities]),
  io:format("-~p doctors~n",[FmkConfig#fmkeconfig.numstaff]),
  io:format("-~p prescriptions~n",[FmkConfig#fmkeconfig.numprescriptions]),
  net_kernel:start([MyNodeName, longnames]),
  erlang:set_cookie(node(), fmke),
  %% check if fmkeis running
  case net_adm:ping(FmkNode) of
    pang ->
      io:format("Cannot connect to FMKe.\n", []);
    pong ->
      ok
  end,
  io:format("Populating ~p...\n", [Database]),
  add_patients(FmkNode, FmkConfig#fmkeconfig.numpatients),
  add_pharmacies(FmkNode, FmkConfig#fmkeconfig.numpharmacies),
  add_facilities(FmkNode, FmkConfig#fmkeconfig.numfacilities),
  add_staff(FmkNode, FmkConfig#fmkeconfig.numstaff),
  add_prescription(FmkNode, FmkConfig#fmkeconfig.numprescriptions, FmkConfig),
  io:format("Successfully populated ~p.\n", [Database]);
main(_) ->
  usage().

usage() ->
  io:format("usage: data_store config_file fmke_node\n"),
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
      run_op(FmkNode, create_pharmacy, [I, gen_random_name(), gen_random_address()])
    end).

add_facilities(FmkNode, Amount) ->
  parallel_create(facilities, 1, Amount, ?NUMTHREADS,
    fun(I) ->
      run_op(FmkNode, create_facility, [I, gen_random_name(), gen_random_address(), gen_random_type()])
    end).

add_patients(FmkNode, Amount) ->
  parallel_create(patient, 1, Amount, 10,
    fun(I) ->
      run_op(FmkNode, create_patient, [I, gen_random_name(), gen_random_address()])
    end).

add_staff(FmkNode, Amount) ->
  parallel_create(staff, 1, Amount, ?NUMTHREADS,
    fun(I) ->
      run_op(FmkNode, create_staff, [I, gen_random_name(), gen_random_address(), gen_random_type()])
    end).

add_prescription(_FmkNode, 0, _FmkConfig) -> ok;
add_prescription(FmkNode, Amount, FmkConfig) when Amount > 0 ->
  ListPatientIds = gen_sequence(FmkConfig#fmkeconfig.numpatients, ?ZIPF_SKEW, FmkConfig#fmkeconfig.numprescriptions),
  add_prescription_rec(FmkNode, Amount, ListPatientIds, FmkConfig).

add_prescription_rec(_FmkNode, 0, _ListPatients, _FmkConfig) -> ok;
add_prescription_rec(FmkNode, PrescriptionId, ListPatientIds, FmkConfig) ->
  [CurrentId | Tail] = ListPatientIds,
  PharmacyId = rand:uniform(FmkConfig#fmkeconfig.numpharmacies),
  PrescriberId = rand:uniform(FmkConfig#fmkeconfig.numstaff),
  run_op(FmkNode, create_prescription, [PrescriptionId, CurrentId, PrescriberId, PharmacyId, gen_random_date(), gen_random_drugs()]),
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
  [_PrescriptionId, _PatientId, _PrescriberId, _PharmacyId, _DatePrescribed, _Drugs] = Params,
  run_rpc_op(FmkNode, create_prescription, Params).

run_rpc_op(FmkNode, Op, Params) ->
  ok = case rpc:call(FmkNode, fmke, Op, Params) of
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

gen_random_drugs() ->
    NumDrugs = rand:uniform(2)+1,
    SeqList = lists:seq(1,NumDrugs),
    lists:map(fun(_Elem) -> gen_random_date() end, SeqList).

gen_random_name() ->
    gen_random_string(25).

gen_random_address() ->
    gen_random_string(40).

gen_random_type() ->
    gen_random_string(14).

gen_random_date() ->
    gen_random_string(10).

gen_random_string(NumBytes) when NumBytes > 0 ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(NumBytes))).
