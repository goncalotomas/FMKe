-module (test_fmke_operations_travis).
-include("fmk.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-define (STATIC_ID, 1).
-define (STATIC_ADDRESS,"https://github.com/goncalotomas/FMKe").
-define (STATIC_ADDRESS_UPDATED,"https://github.com/goncalotomas/FMKe.git").
-define (STATIC_PATIENT_NAME,"FMKe patient").
-define (STATIC_PATIENT_NAME_UPDATED, "FMKe patient updated").
-define (STATIC_PHARMACY_NAME,"FMKe pharmacy").
-define (STATIC_PHARMACY_NAME_UPDATED, "FMKe pharmacy updated").
-define (STATIC_FACILITY_NAME,"FMKe facility").
-define (STATIC_FACILITY_NAME_UPDATED, "FMKe facility updated").
-define (STATIC_FACILITY_TYPE, "FMKe Hospital").
-define (STATIC_FACILITY_TYPE_UPDATED, "FMKe Hospital updated").
-define (STATIC_STAFF_NAME,"FMKe doctor name").
-define (STATIC_STAFF_NAME_UPDATED, "FMKe doctor name updated").
-define (STATIC_STAFF_SPECIALITY, "FMKe doctor speciality").
-define (STATIC_STAFF_SPECIALITY_UPDATED, "FMKe doctor speciality updated").
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

pharmacy_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the pharmacy record"
    " functionality and that serve as unit tests for those operations.",
    {setup,
      fun start/0,                              % setup function
      fun stop/1,                               % teardown function
      fun run_pharmacy_operations/1             % instantiator
    }}.

facility_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the facility record"
    " functionality and that serve as unit tests for those operations.",
    {setup,
      fun start/0,                              % setup function
      fun stop/1,                               % teardown function
      fun run_facility_operations/1             % instantiator
    }}.

staff_operations_test_() ->
    {"Runs a sequential list of operations that expose most of the staff record"
    " functionality and that serve as unit tests for those operations.",
    {setup,
      fun start/0,                              % setup function
      fun stop/1,                               % teardown function
      fun run_staff_operations/1             % instantiator
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
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PATIENT_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          prescriptions=[]
        },
        get_static_patient(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_patient(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #patient{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PATIENT_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          prescriptions=[]
        },
        get_static_patient(FmkeNode))
    ].

run_pharmacy_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_pharmacy(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_pharmacy(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,pharmacy_id_taken},create_static_pharmacy(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #pharmacy{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PHARMACY_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          prescriptions=[]
        },
        get_static_pharmacy(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_pharmacy(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #pharmacy{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_PHARMACY_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          prescriptions=[]
        },
        get_static_pharmacy(FmkeNode))
    ].

run_facility_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_facility(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_facility(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,facility_id_taken},create_static_facility(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #facility{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_FACILITY_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          type=list_to_binary(?STATIC_FACILITY_TYPE)
        },
        get_static_facility(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_facility(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #facility{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_FACILITY_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          type=list_to_binary(?STATIC_FACILITY_TYPE_UPDATED)
        },
        get_static_facility(FmkeNode))
    ].

run_staff_operations(FmkeNode) ->
    [
    %% get unexistant key
    ?_assertEqual({error,not_found},get_static_staff(FmkeNode))
    %% normal create
    ,?_assertEqual(ok,create_static_staff(FmkeNode))
    %% create when record already exists
    ,?_assertEqual({error,staff_id_taken},create_static_staff(FmkeNode))
    %% normal get
    ,?_assertEqual(
        #staff{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_STAFF_NAME),
          address=list_to_binary(?STATIC_ADDRESS),
          speciality=list_to_binary(?STATIC_STAFF_SPECIALITY)
        },
        get_static_staff(FmkeNode))
    %% update record fields
    ,?_assertEqual(ok,update_static_staff(FmkeNode))
    %% get record after update
    ,?_assertEqual(
        #staff{
          id=integer_to_binary(?STATIC_ID),
          name=list_to_binary(?STATIC_STAFF_NAME_UPDATED),
          address=list_to_binary(?STATIC_ADDRESS_UPDATED),
          speciality=list_to_binary(?STATIC_STAFF_SPECIALITY_UPDATED)
        },
        get_static_staff(FmkeNode))
    ].

create_static_patient(FmkeNode) ->
    run_generic_create_op(FmkeNode,patient,[?STATIC_ID,?STATIC_PATIENT_NAME,?STATIC_ADDRESS]).

create_static_pharmacy(FmkeNode) ->
    run_generic_create_op(FmkeNode,pharmacy,[?STATIC_ID,?STATIC_PHARMACY_NAME,?STATIC_ADDRESS]).

create_static_facility(FmkeNode) ->
    run_generic_create_op(FmkeNode,facility,[?STATIC_ID,?STATIC_FACILITY_NAME,?STATIC_ADDRESS,?STATIC_FACILITY_TYPE]).

create_static_staff(FmkeNode) ->
    run_generic_create_op(FmkeNode,staff,[?STATIC_ID,?STATIC_STAFF_NAME,?STATIC_ADDRESS,?STATIC_STAFF_SPECIALITY]).

get_static_patient(FmkeNode) ->
    run_generic_get_op(FmkeNode,patient).

get_static_pharmacy(FmkeNode) ->
    run_generic_get_op(FmkeNode,pharmacy).

get_static_facility(FmkeNode) ->
    run_generic_get_op(FmkeNode,facility).

get_static_staff(FmkeNode) ->
    run_generic_get_op(FmkeNode,staff).

update_static_patient(FmkeNode) ->
    run_generic_update_op(FmkeNode,patient,[?STATIC_ID,?STATIC_PATIENT_NAME_UPDATED,?STATIC_ADDRESS_UPDATED]).

update_static_pharmacy(FmkeNode) ->
    run_generic_update_op(FmkeNode,pharmacy,[?STATIC_ID,?STATIC_PHARMACY_NAME_UPDATED,?STATIC_ADDRESS_UPDATED]).

update_static_facility(FmkeNode) ->
    run_generic_update_op(FmkeNode,facility,[?STATIC_ID,?STATIC_FACILITY_NAME_UPDATED,?STATIC_ADDRESS_UPDATED,?STATIC_FACILITY_TYPE_UPDATED]).

update_static_staff(FmkeNode) ->
    run_generic_update_op(FmkeNode,staff,[?STATIC_ID,?STATIC_STAFF_NAME_UPDATED,?STATIC_ADDRESS_UPDATED,?STATIC_STAFF_SPECIALITY_UPDATED]).

run_generic_create_op(FmkeNode,Entity,Args) ->
    OpAtom = build_create_op(Entity),
    run_rpc_op(FmkeNode,OpAtom,Args).

run_generic_update_op(FmkeNode,Entity,Args) ->
    OpAtom = build_update_op(Entity),
    run_rpc_op(FmkeNode,OpAtom,Args).

run_generic_get_op(FmkeNode,Entity) ->
    OpAtom = build_get_op(Entity),
    run_rpc_op(FmkeNode,OpAtom,[?STATIC_ID]).

build_update_op(Entity) ->
    build_generic_op("update_~p_details",[Entity]).

build_create_op(Entity) ->
    build_generic_op("create_~p",[Entity]).

build_get_op(Entity) ->
    build_generic_op("get_~p_by_id",[Entity]).

build_generic_op(List,Args) ->
    list_to_atom(lists:flatten(io_lib:format(List,Args))).

run_rpc_op(FmkeNode, Op, Params) ->
    rpc:block_call(FmkeNode, fmk_core, Op, Params).

-endif.
