-module(fmke_driver_ets).

-behaviour(gen_fmke_kv_driver).

-include("fmke.hrl").


%% gen_fmke_driver exports
-export([
  start_transaction/1,
  commit_transaction/2,
  get/2,
  put/2
]).

start_transaction(_Opts) ->
    {ok, []}.

commit_transaction(_Context, _Opts) ->
    ok.

get(Keys, Context) ->
    {ok, DataModel} = application:get_env(?APP, data_model),
    {lists:map(fun({Key, Type}) ->
                case ets:lookup(?ETS_TABLE_NAME, Key) of
                    [] ->               {error, not_found};
                    [{Key, Value}] ->   pack(DataModel, Value, Type)
                end
            end, Keys), Context}.

put(Entries, Context) ->
    {lists:map(fun({Key, Type, Value}) ->
                 case Type of
                     prescription_ref ->
                         Val = case ets:lookup(?ETS_TABLE_NAME, Key) of
                             [] ->               [Value];
                             [{Key, List}] ->   [Value | List]
                         end,
                         true = ets:insert(?ETS_TABLE_NAME, {Key, Val}),
                         ok;
                     _Other ->
                         true = ets:insert(?ETS_TABLE_NAME, {Key, unpack(nested, Type, Value)}),
                         ok
                 end
    end, Entries), Context}.

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
    #staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions};
pack(non_nested, List, prescription_ref) ->
    List.

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
