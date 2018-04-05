%%%-------------------------------------------------------------------
%% This helper module exports utility functions that help in comparing
%% the value of FMKe entities coming from different environments.
%%%-------------------------------------------------------------------

-module(fmke_test_utils).

-include("fmke.hrl").

-export ([
    compare_facilities/2
    ,compare_patients/2
    ,compare_pharmacies/2
    ,compare_prescriptions/2
    ,compare_staff/2
    ,search_prescription/2
]).

%% Compares 2 facilities in the form of a list of fields, where the first argument is the expected result.
-spec compare_facilities(Fac1 :: #facility{}, Fac2 :: #facility{}) -> boolean().

compare_facilities(#facility{} = Fac1, #facility{} = Fac2) ->
    integer_compare(Fac1#facility.id, Fac2#facility.id)
    andalso string_compare(Fac1#facility.name, Fac2#facility.name)
    andalso string_compare(Fac1#facility.address, Fac2#facility.address)
    andalso string_compare(Fac1#facility.type, Fac2#facility.type).

%% Compares 2 facilities in the form of a list of fields, where the first argument is the expected result.
-spec compare_patients(Pat1 :: #patient{}, Pat2 :: #patient{}) -> boolean().

compare_patients(#patient{} = Pat1, #patient{} = Pat2) ->
    integer_compare(Pat1#patient.id, Pat2#patient.id)
    andalso string_compare(Pat1#patient.name, Pat2#patient.name)
    andalso string_compare(Pat1#patient.address, Pat2#patient.address)
    andalso list_compare(Pat1#patient.prescriptions, Pat2#patient.prescriptions).

-spec compare_pharmacies(Pharm1 :: #pharmacy{}, Pharm2 :: #pharmacy{}) -> boolean().

compare_pharmacies(#pharmacy{} = Pat1, #pharmacy{} = Pat2) ->
    integer_compare(Pat1#pharmacy.id, Pat2#pharmacy.id)
    andalso string_compare(Pat1#pharmacy.name, Pat2#pharmacy.name)
    andalso string_compare(Pat1#pharmacy.address, Pat2#pharmacy.address)
    andalso list_compare(Pat1#pharmacy.prescriptions, Pat2#pharmacy.prescriptions).

-spec compare_prescriptions(Presc1 :: #prescription{}, Presc2 :: #prescription{}) -> boolean().

compare_prescriptions(#prescription{} = Presc1, #prescription{} = Presc2) ->
    integer_compare(Presc1#prescription.id, Presc2#prescription.id)
    andalso integer_compare(Presc1#prescription.patient_id, Presc2#prescription.patient_id)
    andalso integer_compare(Presc1#prescription.prescriber_id, Presc2#prescription.prescriber_id)
    andalso integer_compare(Presc1#prescription.pharmacy_id, Presc2#prescription.pharmacy_id)
    andalso string_compare(Presc1#prescription.date_prescribed, Presc2#prescription.date_prescribed)
    andalso string_compare(Presc1#prescription.date_processed, Presc2#prescription.date_processed)
    andalso string_compare(Presc1#prescription.is_processed, Presc2#prescription.is_processed)
    andalso list_compare(Presc1#prescription.drugs, Presc2#prescription.drugs).

-spec compare_staff(Staff1 :: #staff{}, Staff2 :: #staff{}) -> boolean().

compare_staff(#staff{} = Staff1, #staff{} = Staff2) ->
    integer_compare(Staff1#staff.id, Staff2#staff.id)
    andalso string_compare(Staff1#staff.name, Staff2#staff.name)
    andalso string_compare(Staff1#staff.address, Staff2#staff.address)
    andalso string_compare(Staff1#staff.speciality, Staff2#staff.speciality)
    andalso list_compare(Staff1#staff.prescriptions, Staff2#staff.prescriptions).

-spec search_prescription(Presc :: #prescription{}, L :: list(#prescription{} | binary())) -> boolean().

search_prescription(Presc, List) ->
    search_prescription(Presc, List, false).

search_prescription(_, [], Accum) ->
    Accum;
search_prescription(P, [H|T], Accum) ->
    cmp_prescs_or_key(P, H) orelse search_prescription(P, T, Accum).

%% helper functions

integer_compare(Int1, Int2) when Int1 =:= Int2 -> true;
integer_compare(Int1, Int2) when is_binary(Int2) -> list_to_binary(integer_to_list(Int1)) =:= Int2.

string_compare(Str1, Str2) when Str1 =:= Str2 -> true;
string_compare(Str1, Str2) when is_binary(Str2) -> list_to_binary(Str1) =:= Str2.

list_compare(L1, L2) ->
    lists:sort(L1) =:= lists:sort(L2) orelse lists:map(fun list_to_binary/1, lists:sort(L1)) =:= lists:sort(L2).

cmp_prescs_or_key(P1, P2) ->
    case gen_key(prescription, presc_id(P1)) =:= P2 of
        true -> true;
        false -> compare_prescriptions(P1, P2)
    end.

gen_key(Entity,Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p",[Entity,Id]))).

presc_id({prescription, Id, _, _, _, _, _, _, _}) when is_integer(Id) -> Id;
presc_id({prescription, Id, _, _, _, _, _, _, _}) when is_binary(Id) -> list_to_integer(binary_to_list(Id));
presc_id([Id, _, _, _, _, _, _, _]) when is_integer(Id) -> Id;
presc_id([Id, _, _, _, _, _, _, _]) when is_binary(Id) -> list_to_integer(binary_to_list(Id)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                     Eunit Tests                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_patient_comparison_test() ->
    ?assert(compare_patients([1, "a", "b", "c", []], [1, "a", "b", "c", []])).

patient_comparison_binary_id_test() ->
    ?assert(compare_patients([1, "a", "b", "c", []], [<<"1">>, "a", "b", "c", []])).

patient_comparison_binary_string_fields_test() ->
    ?assert(compare_patients([1, "a", "b", "c", []], [1, <<"a">>, <<"b">>, <<"c">>, []])).

patient_comparison_all_binary_fields_test() ->
    ?assert(compare_patients([1, "a", "b", "c", []], [<<"1">>, <<"a">>, <<"b">>, <<"c">>, []])).

-endif.
