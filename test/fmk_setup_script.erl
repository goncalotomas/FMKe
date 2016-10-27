#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie antidote -mnesia debug verbose
main([String]) ->
      MyNodeName = lists:flatten(io_lib:format('client~p@127.0.0.1',[String])),
      net_kernel:start([MyNodeName,longnames]),
      erlang:set_cookie(node(),antidote),
      %% check if antidote is running
      case net_adm:ping('antidote@127.0.0.1') of
        pang ->
            io:format("cannot connect to antidote.\n", []);
        pong ->
            ok
      end,
      %% check if fmk is running
      case net_adm:ping('fmk@127.0.0.1') of
        pang ->
            io:format("cannot connect to fmk.\n", []);
        pong ->
            ok
      end,
      add_patients(5000),
      add_pharmacies(300),
      add_facilities(50),
      io:format("finished populating database.\n", []);
main(_) ->
    usage().

usage() ->
    io:format("usage: escript fmk_setup_script node_name\n"),
    halt(1).

add_pharmacies(1) -> run_op(create_pharmacy,[1, "Chai Pharmacy","Costa da Caparica, Portugal"]);
add_pharmacies(Amount) ->
  case Amount rem 3 of
    0 -> run_op(create_pharmacy,[Amount, "Chai Pharmacy","Costa da Caparica, Portugal"]);
    1 -> run_op(create_pharmacy,[Amount, "Carlos Pharmacy","Costa da Caparica, Portugal"]);
    2 -> run_op(create_pharmacy,[Amount, "Definitely not an online scam!","Lisbon, Portugal"])
  end,
  add_pharmacies(Amount-1).


add_facilities(1) -> run_op(create_facility,[1, "Nordsjællands Hospital","Esbønderup, DK","Hospital"]);
add_facilities(Amount) ->
  case Amount rem 10 of
    0 -> run_op(create_facility,[Amount, "Amager Hospital","Amager Island, DK","Hospital"]);
    1 -> run_op(create_facility,[Amount, "Bispebjerg Hospital","Copenhagen, DK","Hospital"]);
    2 -> run_op(create_facility,[Amount, "Bornholms Hospital","Bornholms Island, DK","Hospital"]);
    3 -> run_op(create_facility,[Amount, "Gentofte Hospital","Gentofte, DK","Hospital"]);
    4 -> run_op(create_facility,[Amount, "Glostrup Hospital","Glostrup, DK","Hospital"]);
    5 -> run_op(create_facility,[Amount, "Herlev Hospital","Herlev, DK","Hospital"]);
    6 -> run_op(create_facility,[Amount, "Nordsjællands Hospital","Esbønderup, DK","Hospital"]);
    7 -> run_op(create_facility,[Amount, "Privathospitalet Danmark","Charlottenlund, DK","Hospital"]);
    8 -> run_op(create_facility,[Amount, "Rigshospitalet","Copenhagen, DK","Hospital"]);
    9 -> run_op(create_facility,[Amount, "Sct. Hans Hospital","Zealand Island, DK","Hospital"])
  end,
  add_facilities(Amount-1).

add_patients(1) -> run_op(create_patient,[1, "Goncalo Tomas","Caparica, Portugal"]);
add_patients(Amount) ->
  run_op(create_patient,[Amount, "Goncalo Tomas","Caparica, Portugal"]),
  add_patients(Amount-1).

run_op(create_pharmacy,Params) ->
  [_Id,_Name,_Address] = Params,
  run_rpc_op(create_pharmacy,Params);
run_op(create_facility,Params) ->
  [_Id,_Name,_Address,_Type] = Params,
  run_rpc_op(create_facility,Params);
run_op(create_patient,Params) ->
  [_Id,_Name,_Address] = Params,
  run_rpc_op(create_patient,Params).

run_rpc_op(Op,Params) -> ok = rpc:call('fmk@127.0.0.1',fmk_core,Op,Params).
