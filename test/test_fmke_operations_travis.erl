-module (test_fmke_operations_travis).
-include("fmk.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-define (STATIC_PATIENT_ID, 1).
-define (STATIC_PATIENT_NAME,"FMKe patient").
-define (STATIC_PATIENT_ADDRESS,"https://github.com/goncalotomas/FMKe").
-define (STATIC_PATIENT_NAME_UPDATED, "FMKe patient updated").
-define (STATIC_PATIENT_ADDRESS_UPDATED,"https://github.com/goncalotomas/FMKe.git").
-endif.

-ifdef(TEST).

connection_test_() ->
    {"Runs a quick ping test between the current test node and the local FMKe node",
    {setup,
      fun start/0,               % setup function
      fun stop/1,                % teardown function
      fun test_connection/1      % instantiator
    }}.

patient_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the patient record"
    " functionality and that serve as unit tests for those operations.",
    {setup,
      fun start/0,                              % setup function
      fun stop/1,                               % teardown function
      fun run_patient_operations/1         % instantiator
    }}.

start() ->
    net_kernel:start(['test_ops_travis@127.0.0.1',longnames]),
    erlang:set_cookie(node(),antidote),
    'fmk@127.0.0.1'.

stop(_FmkeNode) ->
    net_kernel:stop().

test_connection(FmkeNode) ->
    [?_assertEqual(pong, net_adm:ping(FmkeNode))].

run_patient_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_patient(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_patient(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,patient_id_taken},create_static_patient(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #patient{
          id=integer_to_binary(?STATIC_PATIENT_ID),
          name=list_to_binary(?STATIC_PATIENT_NAME),
          address=list_to_binary(?STATIC_PATIENT_ADDRESS),
          prescriptions=[]
        },
        get_static_patient(FmkeNode))
    %% update patient fields
    ,?_assertEqual(ok,update_static_patient(FmkeNode)
    %% get record after update
    ,?_assertEqual(
        #patient{
          id=integer_to_binary(?STATIC_PATIENT_ID),
          name=list_to_binary(?STATIC_PATIENT_NAME_UPDATED),
          address=list_to_binary(?STATIC_PATIENT_ADDRESS_UPDATED),
          prescriptions=[]
        },
        get_static_patient(FmkeNode))
    ].

create_static_patient(FmkeNode) ->
    run_rpc_op(FmkeNode,create_patient,[
      ?STATIC_PATIENT_ID,?STATIC_PATIENT_NAME,?STATIC_PATIENT_ADDRESS
    ]).

get_static_patient(FmkeNode) ->
    run_rpc_op(FmkeNode,get_patient_by_id,[?STATIC_PATIENT_ID]).

update_static_patient(FmkeNode) ->
    run_rpc_op(FmkeNode,update_patient_details,[
      ?STATIC_PATIENT_ID,?STATIC_PATIENT_NAME_UPDATED,?STATIC_PATIENT_ADDRESS_UPDATED
    ]).

run_rpc_op(FmkeNode, Op, Params) ->
    rpc:block_call(FmkeNode, fmk_core, Op, Params).

-endif.
