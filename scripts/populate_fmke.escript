#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie fmke -mnesia debug verbose
-mode(compile).
-define(ZIPF_SKEW, 1).
-define(NUMTHREADS, 30).
-define(MAX_RETRIES, 10).

-record(fmkeconfig, {
  numpatients,
  numpharmacies,
  numfacilities,
  numstaff,
  numprescriptions
}).

main([Database, ConfigFile, FmkNodeRef]) ->
  io:format("Running population script with ~p backend.~n",[Database]),
  {ok, Cwd} = file:get_cwd(),
  Filename = Cwd ++ "/config/" ++ ConfigFile,
  io:format("Reading configuration file from ~p...~n",[Filename]),
  FmkNode = list_to_atom(FmkNodeRef),
  io:format("Sending FMKe population ops to ~p.\n", [FmkNode]),
  {ok, FmkConfigProps} = file:consult(Filename),
  FmkConfig = #fmkeconfig{
    numpatients = proplists:get_value(numpatients, FmkConfigProps),
    numpharmacies = proplists:get_value(numpharmacies, FmkConfigProps),
    numfacilities = proplists:get_value(numfacilities, FmkConfigProps),
    numstaff = proplists:get_value(numstaff, FmkConfigProps),
    numprescriptions = proplists:get_value(numprescriptions, FmkConfigProps)
  },

  MyNodeName = "fmke_populator@127.0.0.1",

  io:format("Node name set to ~p.\n", [MyNodeName]),
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
      io:format("[Fatal]: Cannot connect to FMKe.\n");
    pong ->
      io:format("Populating ~p...\n", [Database]),
      case populate_db(FmkNode, FmkConfig) of
        {ok, 0, _} ->
          io:format("Population unsuccessful, please check connectivity to the FMKe server and if the database already contains records from previous benchmarks.~n"),
          halt(1);
        {ok, NumOkOps, NumUnsuccessfulOps} ->
          io:format("Successfully populated ~p (~p insertions out of ~p).~n",
          [Database, NumOkOps, NumOkOps+NumUnsuccessfulOps])
      end
  end;

main(_) ->
  usage().

usage() ->
  io:format("usage: data_store config_file fmke_node\n"),
  halt(1).

populate_db(FmkNode, FmkConfig) ->
  {ok, S1, E1} = add_patients(FmkNode, FmkConfig#fmkeconfig.numpatients),
  {ok, S2, E2} = add_pharmacies(FmkNode, FmkConfig#fmkeconfig.numpharmacies),
  {ok, S3, E3} = add_facilities(FmkNode, FmkConfig#fmkeconfig.numfacilities),
  {ok, S4, E4} = add_staff(FmkNode, FmkConfig#fmkeconfig.numstaff),
  {ok, S5, E5} = add_prescriptions(FmkNode, FmkConfig#fmkeconfig.numprescriptions, FmkConfig),
  {ok, S1 + S2 + S3 + S4 + S5, E1 + E2 + E3 + E4 + E5}.

parallel_create(Name, Amount, Fun) ->
  NumProcs = ?NUMTHREADS,
  Divisions = calculate_divisions(Amount, NumProcs),
  spawn_workers(self(), NumProcs, Divisions, Fun),
  supervisor_loop(Name, 0, Amount).

spawn_workers(Pid, 0, [], Fun) -> ok;
spawn_workers(Pid, ProcsLeft, [H|T], Fun) ->
  spawn(fun() -> lists:map(fun(Id) -> create(Pid, Id, Fun) end, H) end),
  spawn_workers(Pid, ProcsLeft - 1, T, Fun).

supervisor_loop(Name, NumOps, Total) ->
  supervisor_loop(Name, NumOps, Total, {0, 0}).

supervisor_loop(_Name, Total, Total, {Suc, Err}) -> {ok, Suc, Err};
supervisor_loop(Name, NumOps, Total, {Suc, Err}) ->
  receive
    {done, ok, SeqNumber} ->
      CurrentProgress = 100 * SeqNumber / Total,
      CurrentProgTrunc = trunc(CurrentProgress),
      case CurrentProgress == CurrentProgTrunc andalso CurrentProgTrunc rem 10 =:= 0 of
        true ->
          io:format("Creating ~p... ~p%~n", [Name, CurrentProgTrunc]),
          ok;
        false ->
          ok
      end,
      supervisor_loop(Name, NumOps + 1, Total, {Suc + 1, Err});
    {done, {error, Reason}, SeqNumber} ->
      % io:format("Error creating ~p #~p...~n~p~n", [Name, SeqNumber, Reason]),
      supervisor_loop(Name, NumOps + 1, Total, {Suc, Err + 1})
  end.

create(Pid, Id, Fun) ->
  Result = Fun(Id),
  Pid ! {done, Result, Id}.

calculate_divisions(Amount, NumProcs) ->
  AmountPerProc = Amount div NumProcs,
  lists:map(
    fun(ProcNum) ->
      Start = (ProcNum-1) * AmountPerProc + 1,
      End = case ProcNum =:= NumProcs of
        true -> Amount;
        false -> Start + AmountPerProc - 1
      end,
      lists:seq(Start, End)
    end,
    lists:seq(1, NumProcs)).


add_pharmacies(FmkNode, Amount) ->
  parallel_create(pharmacies, Amount,
    fun(I) ->
      run_op(FmkNode, create_pharmacy, [I, gen_random_name(), gen_random_address()])
    end).

add_facilities(FmkNode, Amount) ->
  parallel_create(facilities, Amount,
    fun(I) ->
      run_op(FmkNode, create_facility, [I, gen_random_name(), gen_random_address(), gen_random_type()])
    end).

add_patients(FmkNode, Amount) ->
  parallel_create(patient, Amount,
    fun(I) ->
      run_op(FmkNode, create_patient, [I, gen_random_name(), gen_random_address()])
    end).

add_staff(FmkNode, Amount) ->
  parallel_create(staff, Amount,
    fun(I) ->
      run_op(FmkNode, create_staff, [I, gen_random_name(), gen_random_address(), gen_random_type()])
    end).

add_prescriptions(_FmkNode, 0, _FmkConfig) -> ok;
add_prescriptions(FmkNode, Amount, FmkConfig) when Amount > 0 ->
  io:format("Creating prescriptions...~n"),
  ListPatientIds = gen_sequence(FmkConfig#fmkeconfig.numpatients, ?ZIPF_SKEW, FmkConfig#fmkeconfig.numprescriptions),
  add_prescription_rec(FmkNode, Amount, ListPatientIds, FmkConfig, {0, 0}).

add_prescription_rec(_FmkNode, 0, _ListPatients, _FmkConfig, {Suc, Err}) -> {ok, Suc, Err};
add_prescription_rec(FmkNode, PrescriptionId, ListPatientIds, FmkConfig, {Suc, Err}) ->
  [CurrentId | Tail] = ListPatientIds,
  PharmacyId = rand:uniform(FmkConfig#fmkeconfig.numpharmacies),
  PrescriberId = rand:uniform(FmkConfig#fmkeconfig.numstaff),
  Result = run_op(FmkNode, create_prescription, [PrescriptionId, CurrentId, PrescriberId, PharmacyId, gen_random_date(), gen_random_drugs()]),
  {Suc2, Err2} = case Result of
    ok -> {Suc + 1, Err};
    {error, _Reason} -> {Suc, Err + 1}
  end,
  add_prescription_rec(FmkNode, PrescriptionId - 1, Tail, FmkConfig, {Suc2, Err2}).

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
  run_rpc_op(FmkNode, Op, Params, 0, ?MAX_RETRIES).

run_rpc_op(_FmkNode, Op, Params, MaxTries, MaxTries) ->
    io:format("Error calling ~p(~p), tried ~p times\n", [Op, Params, MaxTries]),
    {error, exceeded_num_retries};
run_rpc_op(FmkNode, Op, Params, CurrentTry, MaxTries) ->
    case rpc:call(FmkNode, fmke, Op, Params) of
      {badrpc,timeout} ->
        run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries);
      {error, Reason} ->
        % io:format("Error ~p in ~p with params ~p\n", [Reason, Op, Params]),
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
    lists:map(fun(_) -> gen_random_name() end, lists:seq(1,NumDrugs)).

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
