#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie antidote -mnesia debug verbose
-mode(compile).
-define (NUM_PATIENTS, 10000).
-define (NUM_PHARMACIES, 300).
-define (NUM_FACILITIES, 50).
-define (NUM_STAFF, 250).
-define (NUM_PRESCRIPTIONS, 1000).
-define (ZIPF_SKEW, 1).


main([ClientId, FmkNodeRef]) ->
      MyNodeName = lists:flatten(io_lib:format('client~p@127.0.0.1',[list_to_integer(ClientId)])),
      FmkNode = list_to_atom(FmkNodeRef),
      io:format("client node is ~p.\n",[MyNodeName]),
      io:format("fmk node target set as ~p.\n",[FmkNode]),
      net_kernel:start([MyNodeName,longnames]),
      erlang:set_cookie(node(),antidote),
      %% check if fmk is running
      case net_adm:ping(FmkNode) of
        pang ->
            io:format("cannot connect to fmk.\n", []);
        pong ->
            ok
      end,
      io:format("populating antidote...\n", []),
      add_patients(FmkNode, ?NUM_PATIENTS),
      add_pharmacies(FmkNode, ?NUM_PHARMACIES),
      add_facilities(FmkNode, ?NUM_FACILITIES),
      add_staff(FmkNode, ?NUM_STAFF),
      add_prescription(FmkNode, ?NUM_PRESCRIPTIONS),
      io:format("finished populating antidote.\n", []);
main(_) ->
    usage().

usage() ->
    io:format("usage: client_id fmk_node\n"),
    halt(1).

add_pharmacies(_FmkNode, 0) -> ok;
add_pharmacies(FmkNode, Amount) when Amount > 0 ->
  case Amount rem 3 of
    0 -> run_op(FmkNode, create_pharmacy,[Amount, "Chai Pharmacy","Costa da Caparica, Portugal"]);
    1 -> run_op(FmkNode, create_pharmacy,[Amount, "Carlos Pharmacy","Costa da Caparica, Portugal"]);
    2 -> run_op(FmkNode, create_pharmacy,[Amount, "Shanghai Central Pharmacy","Shanghai, China"])
  end,
  add_pharmacies(FmkNode, Amount-1).

add_facilities(_FmkNode, 0) -> ok;
add_facilities(FmkNode, Amount) when Amount > 0 ->
  case Amount rem 10 of
    0 -> run_op(FmkNode, create_facility,[Amount, "Amager Hospital","Amager Island, DK","Hospital"]);
    1 -> run_op(FmkNode, create_facility,[Amount, "Bispebjerg Hospital","Copenhagen, DK","Hospital"]);
    2 -> run_op(FmkNode, create_facility,[Amount, "Bornholms Hospital","Bornholms Island, DK","Hospital"]);
    3 -> run_op(FmkNode, create_facility,[Amount, "Gentofte Hospital","Gentofte, DK","Hospital"]);
    4 -> run_op(FmkNode, create_facility,[Amount, "Glostrup Hospital","Glostrup, DK","Hospital"]);
    5 -> run_op(FmkNode, create_facility,[Amount, "Herlev Hospital","Herlev, DK","Hospital"]);
    6 -> run_op(FmkNode, create_facility,[Amount, "Nordsjællands Hospital","Esbønderup, DK","Hospital"]);
    7 -> run_op(FmkNode, create_facility,[Amount, "Privathospitalet Danmark","Charlottenlund, DK","Hospital"]);
    8 -> run_op(FmkNode, create_facility,[Amount, "Rigshospitalet","Copenhagen, DK","Hospital"]);
    9 -> run_op(FmkNode, create_facility,[Amount, "Sct. Hans Hospital","Zealand Island, DK","Hospital"])
  end,
  add_facilities(FmkNode, Amount-1).

add_patients(_FmkNode, 0) -> ok;
add_patients(FmkNode, Amount) when Amount > 0 ->
  run_op(FmkNode, create_patient,[Amount, "Phineas Gage","New Hampshire, United States"]),
  add_patients(FmkNode, Amount-1).

add_staff(_FmkNode, 0) -> ok;
add_staff(FmkNode, Amount) when Amount > 0 ->
  run_op(FmkNode, create_staff,[Amount, "Alexander Fleming","London, UK","Pharmacologist"]),
  add_staff(FmkNode, Amount-1).

add_prescription(_FmkNode, 0) -> ok;
add_prescription(FmkNode, Amount) when Amount > 0 ->
  ListPatientIds = gen_sequence(?NUM_PATIENTS,?ZIPF_SKEW,?NUM_PRESCRIPTIONS),
  ?NUM_PRESCRIPTIONS = length(ListPatientIds),
  add_prescription_rec(FmkNode, Amount,ListPatientIds).

add_prescription_rec(_FmkNode, 0,_ListPatients) -> ok;
add_prescription_rec(FmkNode,PrescriptionId,ListPatientIds) ->
  [CurrentId | Tail] = ListPatientIds,
  PharmacyId = rand:uniform(?NUM_PHARMACIES),
  PrescriberId = rand:uniform(?NUM_STAFF),
  FacilityId = rand:uniform(?NUM_FACILITIES),
  run_op(FmkNode,create_prescription,[PrescriptionId, CurrentId, PrescriberId, PharmacyId, FacilityId, "1/1/2017", ["Acetaminophen"]]),
  add_prescription_rec(FmkNode,PrescriptionId-1, Tail).

run_op(FmkNode,create_pharmacy,Params) ->
  [_Id,_Name,_Address] = Params,
  run_rpc_op(FmkNode,create_pharmacy,Params);
run_op(FmkNode,create_facility,Params) ->
  [_Id,_Name,_Address,_Type] = Params,
  run_rpc_op(FmkNode,create_facility,Params);
run_op(FmkNode,create_patient,Params) ->
  [_Id,_Name,_Address] = Params,
  run_rpc_op(FmkNode,create_patient,Params);
run_op(FmkNode,create_staff,Params) ->
  [_Id,_Name,_Address,_Speciality] = Params,
  run_rpc_op(FmkNode,create_staff,Params);
run_op(FmkNode,create_prescription,Params) ->
  [_PrescriptionId,_PatientId,_PrescriberId,_PharmacyId,_FacilityId,_DatePrescribed,_Drugs] = Params,
  run_rpc_op(FmkNode,create_prescription,Params).

run_rpc_op(FmkNode,Op,Params) ->
  ok = case rpc:call(FmkNode,fmk_core,Op,Params) of
    {error, Reason} ->
      io:format("Error in ~p with params ~p\n",[Op,Params]),
      {error, Reason};
    ok -> ok
  end.

gen_sequence(Size,Skew,SequenceSize) ->
  Bottom = 1/(lists:foldl(fun(X,Sum) -> Sum+(1/math:pow(X,Skew)) end,0,lists:seq(1,Size))),
  lists:map(fun(_X)->
    zipf_next(Size,Skew,Bottom)
  end,lists:seq(1,SequenceSize)).

zipf_next(Size,Skew,Bottom) ->
  Dice = rand:uniform(),
  next(Dice,Size,Skew,Bottom,0,1).

next(Dice,_Size,_Skew,_Bottom,Sum,CurrRank) when Sum >= Dice -> CurrRank-1;
next(Dice,Size,Skew,Bottom,Sum,CurrRank) ->
  NextRank = CurrRank +1,
  Sumi = Sum + (Bottom/math:pow(CurrRank,Skew)),
  next(Dice,Size,Skew,Bottom,Sumi,NextRank).
