#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name setup@127.0.0.1 -cookie antidote -mnesia debug verbose
-mode(compile).
-define (NUM_PATIENTS, 10000).
-define (NUM_PHARMACIES, 300).
-define (NUM_FACILITIES, 50).
-define (NUM_STAFF, 250).
-define (NUM_PRESCRIPTIONS, 0).
-define (ZIPF_SKEW, 1).
-define (FMK_NODE, 'fmk@127.0.0.1').


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
      add_patients(?NUM_PATIENTS),
      add_pharmacies(?NUM_PHARMACIES),
      add_facilities(?NUM_FACILITIES),
      add_staff(?NUM_STAFF),
      add_prescription(?NUM_PRESCRIPTIONS),
      io:format("finished populating database.\n", []);
main(_) ->
    usage().

usage() ->
    io:format("usage: escript fmk_setup_script node_name\n"),
    halt(1).

add_pharmacies(0) -> ok;
add_pharmacies(Amount) ->
  case Amount rem 3 of
    0 -> run_op(create_pharmacy,[Amount, "Chai Pharmacy","Costa da Caparica, Portugal"]);
    1 -> run_op(create_pharmacy,[Amount, "Carlos Pharmacy","Costa da Caparica, Portugal"]);
    2 -> run_op(create_pharmacy,[Amount, "Shanghai Central Pharmacy","Shanghai, China"])
  end,
  add_pharmacies(Amount-1).

add_facilities(0) -> ok;
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

add_patients(0) -> ok;
add_patients(Amount) ->
  run_op(create_patient,[Amount, "Phineas Gage","New Hampshire, United States"]),
  add_patients(Amount-1).

add_staff(0) -> ok;
add_staff(Amount) ->
  run_op(create_staff,[Amount, "Alexander Fleming","London, UK","Pharmacologist"]),
  add_staff(Amount-1).

add_prescription(0) -> ok;
add_prescription(Amount) ->
  ListPatientIds = gen_sequence(?NUM_PATIENTS,?ZIPF_SKEW,?NUM_PRESCRIPTIONS),
  ?NUM_PRESCRIPTIONS = length(ListPatientIds),
  add_prescription_rec(Amount,ListPatientIds).

add_prescription_rec(1,ListPatientIds) ->
  [LastId] = ListPatientIds,
  PharmacyId = rand:uniform(?NUM_PHARMACIES),
  PrescriberId = rand:uniform(?NUM_STAFF),
  FacilityId = rand:uniform(?NUM_FACILITIES),
  run_op(create_prescription,[1, LastId, PrescriberId, PharmacyId, FacilityId, "1/1/2017", ["Acetaminophen"]]);
add_prescription_rec(PrescriptionId,ListPatientIds) ->
  [CurrentId | Tail] = ListPatientIds,
  PharmacyId = rand:uniform(?NUM_PHARMACIES),
  PrescriberId = rand:uniform(?NUM_STAFF),
  FacilityId = rand:uniform(?NUM_FACILITIES),
  run_op(create_prescription,[PrescriptionId, CurrentId, PrescriberId, PharmacyId, FacilityId, "1/1/2017", ["Acetaminophen"]]),
  add_prescription_rec(PrescriptionId-1, Tail).

run_op(create_pharmacy,Params) ->
  [_Id,_Name,_Address] = Params,
  run_rpc_op(create_pharmacy,Params);
run_op(create_facility,Params) ->
  [_Id,_Name,_Address,_Type] = Params,
  run_rpc_op(create_facility,Params);
run_op(create_patient,Params) ->
  [_Id,_Name,_Address] = Params,
  run_rpc_op(create_patient,Params);
run_op(create_staff,Params) ->
  [_Id,_Name,_Address,_Speciality] = Params,
  run_rpc_op(create_staff,Params);
run_op(create_prescription,Params) ->
  [_PrescriptionId,_PatientId,_PrescriberId,_PharmacyId,_FacilityId,_DatePrescribed,_Drugs] = Params,
  run_rpc_op(create_prescription,Params).

run_rpc_op(Op,Params) ->
  ok = case rpc:call(?FMK_NODE,fmk_core,Op,Params) of
    {error, _} ->
      io:format("Error in ~p with params ~p\n",[Op,Params]),
      error;
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
