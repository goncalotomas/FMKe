%% Yea, though I walk through the shadow of the valley of death,
%% I shall fear no evil, for thou art with me.
%% Thy rod and thy staff confort me.
-module(fmkserver).
-behaviour(gen_server).
-compile(export_all). %% #############TODO change this

%% Server API, public exports
-export([start/2, stop/1]).

%% Type definitions
-type string() :: [char(),...].
-type key() :: term().
-type reason() :: atom().
-type riak_dt_map() :: riak_dt_map:riak_dt_map().

%% Defines
-define(MAXKEY,1000000).

%% ----------------------------------------------------------------------------
%% Server API
%% ----------------------------------------------------------------------------
start(_Type, _Args) ->
  start().

stop(_State) ->
  ok.

-spec start() -> ok | {error, reason()}.
start() ->
  io:format("Starting FMK Server~n"),
    ok.

%% @doc Adds a Patient to the server
%% Input : Name and address of the patient
%% Output : {ok, UserId}
%%        UserId is the key that can be used later to access the patient record
%% TODO - check if the user already exists to deliver a different error message
-spec create_patient(string(),string()) -> 
  {ok, UserId::key()} | {error, reason()}.
create_patient(name,address) ->
  UserKey = get_random_key(),
  {ok, UserKey}.

%% @doc Adds a Patient to the server
%% Input : Name and address of the patient
%% Output : {ok, UserId}
%%        UserId is the key that can be used later to access the patient record
%% TODO - check if the user already exists to deliver a different error message
-spec create_medical_staff(string(),string(),string(),riak_dt_map()) -> 
  {ok, UserId::key()} | {error, reason()}.
create_medical_staff(_Name,_Address,_Specialization,_TreatmentFacility) ->
  {ok,1}.

%% @doc Adds a Patient to the server
%% Input : Name and address of the patient
%% Output : {ok, UserId}
%%        UserId is the key that can be used later to access the patient record
%% TODO - check if the user already exists to deliver a different error message
-spec create_treatment_facility(string(),string(),string()) -> 
  {ok, UserId::key()} | {error, reason()}.
create_treatment_facility(_Name,_Address,_Type) ->
  {ok,1}.

%% ----------------------------------------------------------------------------
%% Callbacks
%% ----------------------------------------------------------------------------

call_fun(Fun, Param, Server) ->
  rpc:call(Server, antidote, Fun, Param).

get_random_key() ->
  rand:uniform(?MAXKEY).

create_stub_patient() ->
  riak_dt_map:new().

create_stub_medical_staff() ->
  riak_dt_map:new().

create_stub_treatment_facility() ->
  riak_dt_map:new().

create_stub_pharmacy() ->
  riak_dt_map:new().

create_stub_event() ->
  riak_dt_map:new().

create_stub_prescription() ->
  riak_dt_map:new().