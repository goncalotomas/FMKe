-module(fmke_driver_ets).

-behaviour(gen_fmke_kv_driver).
-behaviour(gen_server).

-include("fmke.hrl").

%% gen_server exports
-export ([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%% gen_fmke_driver exports
-export([
  start/1,
  stop/0,
  start_transaction/1,
  commit_transaction/2,
  get/3,
  put/4
]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, fmke_ets).

start(DataModel) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], [DataModel]).

stop() ->
  gen_server:call(?MODULE, stop).

init([DataModel]) ->
  ets:new(?TABLE_NAME, []),
  {ok, DataModel}.

start_transaction(_Opts) ->
  {ok, []}.

commit_transaction(_Context, _Opts) ->
  {ok, []}.

get(Key, Type, _Context) ->
  gen_server:call(?MODULE, {get, Key, Type}).

put(Key, Type, Value, _Context) ->
  ets:insert(?TABLE_NAME, {Key, unpack(nested, Type, Value)}).

handle_call({get, Key, Type}, _From, DataModel) ->
  {Key, Result} = ets:lookup(?TABLE_NAME, Key),
  {reply, pack(DataModel, Result, Type), DataModel};

handle_call({put, Key, Type, Value}, _From, DataModel) ->
  ets:insert(?TABLE_NAME, {Key, unpack(nested, Type, Value)}),
  {reply, ok, DataModel}.

handle_cast(_Msg, State) ->
  {noreply, State}.

pack(_, {Id, Name, Address, Type}, facility) ->
  #facility{id = Id, name = Name, address = Address, type = Type};
pack(_, {Id, Name, Address, Prescriptions}, patient) ->
  #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
pack(_, {Id, Name, Address, Prescriptions}, pharmacy) ->
  #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions};
pack(_, {Id, PatientId, PrescriberId, PharmacyId,
                 DatePrescribed, DateProcessed, Drugs, IsProcessed}, prescription) ->
  #prescription{
      id = Id
      ,patient_id = PatientId
      ,prescriber_id = PrescriberId
      ,pharmacy_id = PharmacyId
      ,date_prescribed = DatePrescribed
      ,date_processed = DateProcessed
      ,drugs = Drugs
      ,is_processed = IsProcessed
  };
pack(_, {Id, Name, Address, Speciality, Prescriptions}, staff) ->
  #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions}.


unpack(_, facility, #facility{id = Id, name = Name, address = Address, type = Type}) ->
  {Id, Name, Address, Type};
unpack(_, patient, #patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
  {Id, Name, Address, Prescriptions};
unpack(_, pharmacy, #pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions}) ->
  {Id, Name, Address, Prescriptions};
unpack(_, prescription, #prescription{id = Id, patient_id = PatientId, prescriber_id = PrescriberId,
                                           pharmacy_id = PharmacyId, date_prescribed = DatePrescribed,
                                           date_processed = DateProcessed, drugs = Drugs,
                                           is_processed = IsProcessed}) ->
  {Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, DateProcessed, Drugs, IsProcessed};
unpack(nested, staff, #staff{id = Id, name = Name, address = Address, speciality = Speciality,
                             prescriptions = Prescriptions}) ->
  {Id, Name, Address, Speciality, Prescriptions}.
