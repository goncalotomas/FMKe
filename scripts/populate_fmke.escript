#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie fmke -mnesia debug verbose
-mode(compile).
-define(ZIPF_SKEW, 1).
-define(NUMTHREADS, 30).
-define(DIVERGENCE_TIMEOUT, 10).
-define(MAX_RETRIES, 10).

-record(fmkeconfig, {
  numpatients,
  numpharmacies,
  numfacilities,
  numstaff,
  numprescriptions
}).

main([Database, ConfigFile, Node | []]) ->
  populate(Database, ConfigFile, [Node]);

main([Database, ConfigFile | Nodes = [_H|_T]]) ->
  populate(Database, ConfigFile, Nodes);

main(_) ->
  usage().

usage() ->
  io:format("usage: data_store config_file fmke_node\n"),
  halt(1).

populate(Database, ConfigFile, FMKeNodes) ->
  io:format("Running population script with ~p backend.~n",[Database]),
  {ok, Cwd} = file:get_cwd(),
  Filename = Cwd ++ "/config/" ++ ConfigFile,
  io:format("Reading configuration file from ~p...~n",[Filename]),
  Nodes = lists:map(fun(Node) -> list_to_atom(Node) end, FMKeNodes),
  io:format("Sending FMKe population ops to the following nodes:~n~p~n", [Nodes]),
  {ok, ConfigProps} = file:consult(Filename),
  Config = #fmkeconfig{
    numpatients = proplists:get_value(numpatients, ConfigProps),
    numpharmacies = proplists:get_value(numpharmacies, ConfigProps),
    numfacilities = proplists:get_value(numfacilities, ConfigProps),
    numstaff = proplists:get_value(numstaff, ConfigProps),
    numprescriptions = proplists:get_value(numprescriptions, ConfigProps)
  },

  MyNodeName = "fmke_populator@127.0.0.1",

  io:format("Node name set to ~p.\n", [MyNodeName]),
  io:format("The population script is going to create the following entities:~n",[]),
  io:format("-~p patients~n",[Config#fmkeconfig.numpatients]),
  io:format("-~p pharmacies~n",[Config#fmkeconfig.numpharmacies]),
  io:format("-~p hospitals~n",[Config#fmkeconfig.numfacilities]),
  io:format("-~p doctors~n",[Config#fmkeconfig.numstaff]),
  io:format("-~p prescriptions~n",[Config#fmkeconfig.numprescriptions]),
  net_kernel:start([MyNodeName, longnames]),
  erlang:set_cookie(node(), fmke),
  %% check if all nodes are running and reachable via distributed erlang
  case multi_ping(Nodes) of
    pang ->
      io:format("[Fatal]: Cannot connect to FMKe.\n");
    pong ->
      io:format("Populating ~p...\n", [Database]),
      case populate_db(Nodes, Config) of
        {ok, 0} ->
          io:format("Population unsuccessful, please check if the database already contains records from previous benchmarks.~n"),
          halt(1);
        {ok, Ops} ->
          io:format("Successfully populated ~p (~p insertions).~n", [Database, Ops])
      end
  end.

multi_ping([]) -> pong;
multi_ping([H|T]) ->
  case net_adm:ping(H) of
    pang -> pang;
    pong -> multi_ping(T)
  end.

populate_db(Nodes, Config) ->
  {ok, S1} = add_patients(Nodes, Config#fmkeconfig.numpatients),
  {ok, S2} = add_pharmacies(Nodes, Config#fmkeconfig.numpharmacies),
  {ok, S3} = add_facilities(Nodes, Config#fmkeconfig.numfacilities),
  {ok, S4} = add_staff(Nodes, Config#fmkeconfig.numstaff),
  {ok, S5} = add_prescriptions(Nodes, Config#fmkeconfig.numprescriptions, Config),
  {ok, S1 + S2 + S3 + S4 + S5}.

parallel_create(Name, Amount, Fun) ->
  NumProcs = ?NUMTHREADS,
  Divisions = calculate_divisions(Amount, NumProcs),
  spawn_workers(self(), NumProcs, Divisions, Fun),
  supervisor_loop(Name, 0, Amount).

spawn_workers(_Pid, 0, [], _Fun) -> ok;
spawn_workers(Pid, ProcsLeft, [H|T], Fun) ->
  spawn(fun() -> lists:map(fun(Id) -> create(Pid, Id, Fun) end, H) end),
  spawn_workers(Pid, ProcsLeft - 1, T, Fun).

supervisor_loop(Name, NumOps, Total) ->
  supervisor_loop(Name, NumOps, Total, 0).

supervisor_loop(_Name, Total, Total, Ops) -> {ok, Ops};
supervisor_loop(Name, NumOps, Total, Ops) ->
  receive
    {done, ok, _SeqNumber} ->
      CurrentProgress = 100 * (NumOps + 1) / Total,
      CurrentProgTrunc = trunc(CurrentProgress),
      case CurrentProgress == CurrentProgTrunc andalso CurrentProgTrunc rem 10 =:= 0 of
        true ->
          io:format("Creating ~p... ~p%~n", [Name, CurrentProgTrunc]),
          ok;
        false ->
          ok
      end,
      supervisor_loop(Name, NumOps + 1, Total, Ops + 1)
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

add_pharmacies(Nodes, Amount) ->
  parallel_create(pharmacies, Amount,
    fun(I) ->
      Node = lists:nth(I rem length(Nodes) + 1, Nodes),
      run_op(Node, create_pharmacy, [I, gen_random_name(), gen_random_address()])
    end).

add_facilities(Nodes, Amount) ->
  parallel_create(facilities, Amount,
    fun(I) ->
      Node = lists:nth(I rem length(Nodes) + 1, Nodes),
      run_op(Node, create_facility, [I, gen_random_name(), gen_random_address(), gen_random_type()])
    end).

add_patients(Nodes, Amount) ->
  parallel_create(patients, Amount,
    fun(I) ->
      Node = lists:nth(I rem length(Nodes) + 1, Nodes),
      run_op(Node, create_patient, [I, gen_random_name(), gen_random_address()])
    end).

add_staff(Nodes, Amount) ->
  parallel_create(staff, Amount,
    fun(I) ->
      Node = lists:nth(I rem length(Nodes) + 1, Nodes),
      run_op(Node, create_staff, [I, gen_random_name(), gen_random_address(), gen_random_type()])
    end).

add_prescriptions(_Nodes, 0, _Config) -> ok;
add_prescriptions(Nodes, Amount, Config) when Amount > 0 ->
  io:format("Creating prescriptions...~n"),
  ListPatientIds = gen_sequence(Config#fmkeconfig.numpatients, ?ZIPF_SKEW, Config#fmkeconfig.numprescriptions),
  add_prescription_rec(Nodes, Amount, ListPatientIds, Config, 0).

add_prescription_rec(_Nodes, 0, _PatientIds, _Config, Ops) -> {ok, Ops};
add_prescription_rec(Nodes, PrescriptionId, ListPatientIds, FmkConfig, Ops) ->
  [CurrentId | Tail] = ListPatientIds,
  PharmacyId = rand:uniform(FmkConfig#fmkeconfig.numpharmacies),
  PrescriberId = rand:uniform(FmkConfig#fmkeconfig.numstaff),
  Node = lists:nth(PrescriptionId rem length(Nodes) + 1, Nodes),
  OpArgs = [PrescriptionId, CurrentId, PrescriberId, PharmacyId, gen_random_date(), gen_random_drugs()],
  Result = run_op(Node, create_prescription, OpArgs),
  case divergence_failure(Result) of
      false ->
          ok;
      true ->
          timer:sleep(?DIVERGENCE_TIMEOUT * 1000),
          ok = run_op(Node, create_prescription, OpArgs)
  end,
  add_prescription_rec(Nodes, PrescriptionId - 1, Tail, FmkConfig, Ops + 1).

divergence_failure(ok) -> false;
divergence_failure({error, no_such_patient}) -> true;
divergence_failure({error, no_such_pharmacy}) -> true;
divergence_failure({error, no_such_staff}) -> true.

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
  run_rpc_op(FmkNode, Op, Params, 0, ?MAX_RETRIES, none).

run_rpc_op(_FmkNode, _Op, _Params, _MaxTries, _MaxTries, none) ->
    io:format("FMKe node is reachable but operation timed out. Check status of FMKe node~n", []),
    {error, exceeded_num_retries};
run_rpc_op(_FmkNode, _Op, _Params, _MaxTries, _MaxTries, Error) ->
    case Error of
        nodedown ->
            io:format("FMKe node is down or unreachable. Check link between populator and FMKe~n", []);
        timeout ->
            io:format("FMKe node is reachable but operation timed out. Check status of FMKe node~n", []);
        no_connection ->
            io:format("FMKe is not connected to the database. Please check the link between FMKe and the database.~n", []);
        broken_link ->
            io:format("Connection between FMKe and DB is unstable/broken. Please check the link between FMKe and the database.~n", [])
    end,
    halt(2);
run_rpc_op(FmkNode, Op, Params, CurrentTry, MaxTries, _Error) ->
    case rpc:call(FmkNode, fmke, Op, Params) of
        {badrpc, nodedown} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, nodedown);
        {badrpc, timeout} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, none);
        {badrpc, {'EXIT', {{{badmatch, {error, no_connection}}, _DriverStackTrace}, _AppStackTrace}}} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, no_connection);
        {badrpc, {'EXIT', {noproc, {gen_server, call, _AppStackTrace}}}} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, broken_link);
        {badrpc, {'EXIT', {timeout, {gen_server, call, _AppStackTrace}}}} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, broken_link);
        {badrpc, {'EXIT', {{{badmatch, {error, tcp_closed, _Pid}}, _DriverStackTrace}, {gen_server, call, _AppStackTrace}}}} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, broken_link);
        {badrpc, {'EXIT', {{{badmatch,{error,tcp_closed}}, _DriverStackTrace}, {gen_server,call, _AppStackTrace}}}} ->
            run_rpc_op(FmkNode, Op, Params, CurrentTry + 1, MaxTries, broken_link);
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
