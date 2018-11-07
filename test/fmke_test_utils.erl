%%%-------------------------------------------------------------------
%% This helper module exports utility functions that help in comparing
%% the value of FMKe entities coming from different environments.
%%%-------------------------------------------------------------------

-module(fmke_test_utils).

-include("fmke.hrl").
-include("fmke_kv.hrl").

-export ([
    compare_facilities/2,
    compare_patients/2,
    compare_pharmacies/2,
    compare_prescriptions/2,
    compare_staff/2,
    search_prescription/2
]).

%% Compares 2 facilities in the form of a list of fields, where the first argument is the expected result.
-spec compare_facilities(Fac1 :: facility(), Fac2 :: facility()) -> boolean().

compare_facilities(#facility{} = Fac1, #facility{} = Fac2) ->
    integer_compare(Fac1#facility.id, Fac2#facility.id)
    andalso string_compare(Fac1#facility.name, Fac2#facility.name)
    andalso string_compare(Fac1#facility.address, Fac2#facility.address)
    andalso string_compare(Fac1#facility.type, Fac2#facility.type).

%% Compares 2 facilities in the form of a list of fields, where the first argument is the expected result.
-spec compare_patients(Pat1 :: patient(), Pat2 :: patient()) -> boolean().

compare_patients(#patient{} = Pat1, #patient{} = Pat2) ->
    integer_compare(Pat1#patient.id, Pat2#patient.id)
    andalso string_compare(Pat1#patient.name, Pat2#patient.name)
    andalso string_compare(Pat1#patient.address, Pat2#patient.address)
    andalso list_compare(Pat1#patient.prescriptions, Pat2#patient.prescriptions).

-spec compare_pharmacies(Pharm1 :: pharmacy(), Pharm2 :: pharmacy()) -> boolean().

compare_pharmacies(#pharmacy{} = Pat1, #pharmacy{} = Pat2) ->
    integer_compare(Pat1#pharmacy.id, Pat2#pharmacy.id)
    andalso string_compare(Pat1#pharmacy.name, Pat2#pharmacy.name)
    andalso string_compare(Pat1#pharmacy.address, Pat2#pharmacy.address)
    andalso list_compare(Pat1#pharmacy.prescriptions, Pat2#pharmacy.prescriptions).

-spec compare_prescriptions(Presc1 :: prescription(), Presc2 :: prescription()) -> boolean().

compare_prescriptions(#prescription{} = Presc1, #prescription{} = Presc2) ->
    integer_compare(Presc1#prescription.id, Presc2#prescription.id)
    andalso integer_compare(Presc1#prescription.patient_id, Presc2#prescription.patient_id)
    andalso integer_compare(Presc1#prescription.prescriber_id, Presc2#prescription.prescriber_id)
    andalso integer_compare(Presc1#prescription.pharmacy_id, Presc2#prescription.pharmacy_id)
    andalso date_compare(Presc1#prescription.date_prescribed, Presc2#prescription.date_prescribed)
    andalso date_compare(Presc1#prescription.date_processed, Presc2#prescription.date_processed)
    andalso string_compare(Presc1#prescription.is_processed, Presc2#prescription.is_processed)
    andalso list_compare(Presc1#prescription.drugs, Presc2#prescription.drugs).

-spec compare_staff(Staff1 :: staff(), Staff2 :: staff()) -> boolean().

compare_staff(#staff{} = Staff1, #staff{} = Staff2) ->
    integer_compare(Staff1#staff.id, Staff2#staff.id)
    andalso string_compare(Staff1#staff.name, Staff2#staff.name)
    andalso string_compare(Staff1#staff.address, Staff2#staff.address)
    andalso string_compare(Staff1#staff.speciality, Staff2#staff.speciality)
    andalso list_compare(Staff1#staff.prescriptions, Staff2#staff.prescriptions).

date_compare(Date1, Date2) ->
    string_compare(Date1, Date2)
    orelse integer_compare(calendar:rfc3339_to_system_time(Date1 ++ "T00:00:00"), Date2)
    orelse integer_compare(convert_date_to_cassandra_time(Date1 ++ "T00:00:00"), Date2).

convert_date_to_cassandra_time(Date) ->
    erlcass_time:date_from_epoch(calendar:rfc3339_to_system_time(Date)).

-spec search_prescription(Presc :: prescription(), L :: list(prescription() | binary())) -> boolean().

search_prescription(Presc, List) ->
    search_prescription(Presc, List, false).

search_prescription(_, [], Accum) ->
    Accum;
search_prescription(P, [H|T], Accum) ->
    cmp_prescs_or_key(P, H) orelse search_prescription(P, T, Accum).

%% helper functions

integer_compare(Int1, Int2) when Int1 =:= Int2 -> true;
integer_compare(Int1, Int2) when is_integer(Int1), is_binary(Int2) -> list_to_binary(integer_to_list(Int1)) =:= Int2;
integer_compare(_, _) -> false.

string_compare(Str1, Str2) when Str1 =:= Str2 -> true;
string_compare(Str1, Str2) when is_list(Str1), is_binary(Str2) -> list_to_binary(Str1) =:= Str2;
string_compare(_, _) -> false.

list_compare(L1, L2) ->
    lists:sort(L1) =:= lists:sort(L2) orelse
    try
        lists:map(fun list_to_binary/1, lists:sort(L1)) =:= lists:sort(L2)
    catch _:_ ->
        false
    end.

cmp_prescs_or_key(P1, P2) ->
    cmp_presc_id(P1, P2)
    orelse cmp_prec_key(P1, P2)
    orelse cmp_presc_rec(P1, P2).

cmp_presc_id(P1, P2) ->
    presc_id(P1) =:= P2.

cmp_prec_key(P1, P2) ->
    gen_key(prescription, presc_id(P1)) =:= P2.

cmp_presc_rec(P1, P2) ->
    case is_record(P2, prescription) of
        true -> compare_prescriptions(P1, P2);
        false -> compare_prescriptions(P1, fmke_json:decode(prescription, P2))
    end.

gen_key(Entity, Id) ->
    list_to_binary(lists:flatten(io_lib:format("~p_~p", [Entity, Id]))).

presc_id({prescription, Id, _, _, _, _, _, _, _}) when is_integer(Id) -> Id;
presc_id({prescription, Id, _, _, _, _, _, _, _}) when is_binary(Id) -> list_to_integer(binary_to_list(Id));
presc_id([{A, _B} | _T] = PropList) when is_binary(A) ->
    Val = proplists:get_value(<<"prescriptionId">>, PropList),
    case is_binary(Val) of
        true -> list_to_integer(binary_to_list(Val));
        false -> Val
    end;
presc_id([Id, _, _, _, _, _, _, _]) when is_integer(Id) -> Id;
presc_id([Id, _, _, _, _, _, _, _]) when is_binary(Id) -> list_to_integer(binary_to_list(Id));
presc_id(Id) when is_integer(Id) ->
    Id.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                     Eunit Tests                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

facility_comparison_test_() ->
    [
        identity_facility_comparison(),
        facility_comparison_binary_id(),
        facility_comparison_binary_string_fields(),
        facility_comparison_all_binary_fields(),
        facility_comparison_wrong_id(),
        facility_comparison_wrong_name(),
        facility_comparison_wrong_address(),
        facility_comparison_wrong_type()
    ].

identity_facility_comparison() ->
    Facility = #facility{id = 1, name = "a", address = "b", type = "s"},
    ?_assert(compare_facilities(Facility, Facility)).

facility_comparison_binary_id() ->
    One = #facility{id = 1, name = "a", address = "b", type = "s"},
    Two = #facility{id = <<"1">>, name = "a", address = "b", type = "s"},
    ?_assert(compare_facilities(One, Two)).

facility_comparison_binary_string_fields() ->
    One = #facility{id = 1, name = "a", address = "b", type = "s"},
    Two = #facility{id = 1, name = <<"a">>, address = <<"b">>, type = <<"s">>},
    ?_assert(compare_facilities(One, Two)).

facility_comparison_all_binary_fields() ->
    One = #facility{id = 1, name = "a", address = "b", type = "s"},
    Two = #facility{id = <<"1">>, name = <<"a">>, address = <<"b">>, type = <<"s">>},
    ?_assert(compare_facilities(One, Two)).

facility_comparison_wrong_id() ->
    One = #facility{id = 1, name = "a", address = "b", type = "s"},
    Two = #facility{id = <<"2">>, name = <<"a">>, address = <<"b">>, type = <<"s">>},
    ?_assert(not compare_facilities(One, Two)).

facility_comparison_wrong_name() ->
    One = #facility{id = 1, name = "d", address = "b", type = "s"},
    Two = #facility{id = <<"1">>, name = <<"a">>, address = <<"b">>, type = <<"s">>},
    ?_assert(not compare_facilities(One, Two)).

facility_comparison_wrong_address() ->
    One = #facility{id = 1, name = "a", address = "Z", type = "s"},
    Two = #facility{id = <<"1">>, name = <<"a">>, address = <<"b">>, type = <<"s">>},
    ?_assert(not compare_facilities(One, Two)).

facility_comparison_wrong_type() ->
    One = #facility{id = 1, name = "a", address = "b", type = "x"},
    Two = #facility{id = <<"1">>, name = <<"a">>, address = <<"b">>, type = <<"s">>},
    ?_assert(not compare_facilities(One, Two)).

patient_comparison_test_() ->
    [
        identity_patient_comparison(),
        patient_comparison_binary_id(),
        patient_comparison_binary_string_fields(),
        patient_comparison_all_binary_fields(),
        patient_comparison_with_single_nested_prescription(),
        patient_comparison_unordered_nested_prescriptions(),
        patient_comparison_with_single_prescription_ref(),
        patient_comparison_unordered_prescriptions_refs(),
        patient_comparison_wrong_id(),
        patient_comparison_wrong_name(),
        patient_comparison_wrong_address(),
        patient_comparison_missing_prescriptions(),
        patient_comparison_missing_prescription_refs(),
        patient_comparison_wrong_prescriptions(),
        patient_comparison_wrong_prescription_refs()
    ].

identity_patient_comparison() ->
    Patient = #patient{id = 1, name = "a", address = "b", prescriptions = []},
    ?_assert(compare_patients(Patient, Patient)).

patient_comparison_binary_id() ->
    One = #patient{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #patient{id = <<"1">>, name = "a", address = "b", prescriptions = []},
    ?_assert(compare_patients(One, Two)).

patient_comparison_binary_string_fields() ->
    One = #patient{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #patient{id = 1, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(compare_patients(One, Two)).

patient_comparison_all_binary_fields() ->
    One = #patient{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(compare_patients(One, Two)).

patient_comparison_with_single_nested_prescription() ->
    Presc = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Presc]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc]},
    ?_assert(compare_patients(One, Two)).

patient_comparison_unordered_nested_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 2, pharmacy_id = 4, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Presc2, Presc1]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc1, Presc2]},
    ?_assert(compare_patients(One, Two)).

patient_comparison_with_single_prescription_ref() ->
    Ref = <<"prescription_345">>,
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Ref]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref]},
    ?_assert(compare_patients(One, Two)).

patient_comparison_unordered_prescriptions_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Ref1, Ref2, Ref3]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref3, Ref1, Ref2]},
    ?_assert(compare_patients(One, Two)).

patient_comparison_wrong_id() ->
    One = #patient{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #patient{id = <<"2">>, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(not compare_patients(One, Two)).

patient_comparison_wrong_name() ->
    One = #patient{id = 1, name = "d", address = "b", prescriptions = []},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(not compare_patients(One, Two)).

patient_comparison_wrong_address() ->
    One = #patient{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"bbbb">>, prescriptions = []},
    ?_assert(not compare_patients(One, Two)).

patient_comparison_missing_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 2, pharmacy_id = 4, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Presc2]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc1, Presc2]},
    ?_assert(not compare_patients(One, Two)).

patient_comparison_missing_prescription_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Ref1, Ref2, Ref3]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref3, Ref2]},
    ?_assert(not compare_patients(One, Two)).

patient_comparison_wrong_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 2, pharmacy_id = 4, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Presc1, Presc2#prescription{id = 33333}]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc1, Presc2]},
    ?_assert(not compare_patients(One, Two)).

patient_comparison_wrong_prescription_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #patient{id = 1, name = "a", address = "b", prescriptions = [Ref1, Ref3]},
    Two = #patient{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref3, Ref2]},
    ?_assert(not compare_patients(One, Two)).

pharmacy_comparison_test_() ->
    [
        identity_pharmacy_comparison(),
        pharmacy_comparison_binary_id(),
        pharmacy_comparison_binary_string_fields(),
        pharmacy_comparison_all_binary_fields(),
        pharmacy_comparison_with_single_nested_prescription(),
        pharmacy_comparison_unordered_nested_prescriptions(),
        pharmacy_comparison_with_single_prescription_ref(),
        pharmacy_comparison_unordered_prescriptions_refs(),
        pharmacy_comparison_wrong_id(),
        pharmacy_comparison_wrong_name(),
        pharmacy_comparison_wrong_address(),
        pharmacy_comparison_missing_prescriptions(),
        pharmacy_comparison_missing_prescription_refs(),
        pharmacy_comparison_wrong_prescriptions(),
        pharmacy_comparison_wrong_prescription_refs()
    ].

identity_pharmacy_comparison() ->
    Pharmacy = #pharmacy{id = 1, name = "a", address = "b", prescriptions = []},
    ?_assert(compare_pharmacies(Pharmacy, Pharmacy)).

pharmacy_comparison_binary_id() ->
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #pharmacy{id = <<"1">>, name = "a", address = "b", prescriptions = []},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_binary_string_fields() ->
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #pharmacy{id = 1, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_all_binary_fields() ->
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_with_single_nested_prescription() ->
    Presc = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Presc]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc]},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_unordered_nested_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 3, pharmacy_id = 3, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Presc2, Presc1]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc1, Presc2]},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_with_single_prescription_ref() ->
    Ref = <<"prescription_345">>,
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Ref]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref]},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_unordered_prescriptions_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Ref1, Ref2, Ref3]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref3, Ref1, Ref2]},
    ?_assert(compare_pharmacies(One, Two)).

pharmacy_comparison_wrong_id() ->
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #pharmacy{id = <<"2">>, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(not compare_pharmacies(One, Two)).

pharmacy_comparison_wrong_name() ->
    One = #pharmacy{id = 1, name = "d", address = "b", prescriptions = []},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = []},
    ?_assert(not compare_pharmacies(One, Two)).

pharmacy_comparison_wrong_address() ->
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = []},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"bbbb">>, prescriptions = []},
    ?_assert(not compare_pharmacies(One, Two)).

pharmacy_comparison_missing_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 3, pharmacy_id = 3, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Presc2]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc1, Presc2]},
    ?_assert(not compare_pharmacies(One, Two)).

pharmacy_comparison_missing_prescription_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Ref1, Ref2, Ref3]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref3, Ref2]},
    ?_assert(not compare_pharmacies(One, Two)).

pharmacy_comparison_wrong_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 3, pharmacy_id = 3, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Presc1, Presc2#prescription{id = 33333}]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Presc1, Presc2]},
    ?_assert(not compare_pharmacies(One, Two)).

pharmacy_comparison_wrong_prescription_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #pharmacy{id = 1, name = "a", address = "b", prescriptions = [Ref1, Ref3]},
    Two = #pharmacy{id = <<"1">>, name = <<"a">>, address = <<"b">>, prescriptions = [Ref3, Ref2]},
    ?_assert(not compare_pharmacies(One, Two)).

prescription_comparison_test_() ->
    [
        identity_prescription_comparison(),
        prescription_comparison_binary_id(),
        prescription_comparison_binary_patient_id(),
        prescription_comparison_binary_pharmacy_id(),
        prescription_comparison_binary_prescriber_id(),
        prescription_comparison_binary_string_fields(),
        prescription_comparison_all_binary_fields(),
        prescription_comparison_wrong_id(),
        prescription_comparison_wrong_patient_id(),
        prescription_comparison_wrong_pharmacy_id(),
        prescription_comparison_wrong_prescriber_id(),
        prescription_comparison_wrong_date_prescribed(),
        prescription_comparison_wrong_date_processed(),
        prescription_comparison_wrong_is_processed(),
        prescription_comparison_wrong_drugs(),
        prescription_comparison_missing_drugs()
    ].

identity_prescription_comparison() ->
    Prescription = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(Prescription, Prescription)).

prescription_comparison_binary_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = <<"1">>, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(One, Two)).

prescription_comparison_binary_patient_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = <<"2">>, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(One, Two)).

prescription_comparison_binary_pharmacy_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = <<"3">>, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(One, Two)).

prescription_comparison_binary_prescriber_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = <<"4">>,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(One, Two)).

prescription_comparison_binary_string_fields() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                        drugs = [<<"Rupafin">>, <<"Ibuprofen">>], date_prescribed = <<"02/04/2018">>,
                        date_processed = <<"06/04/2018">>, is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(One, Two)).

prescription_comparison_all_binary_fields() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = <<"1">>, patient_id = <<"2">>, pharmacy_id = <<"3">>, prescriber_id = <<"4">>,
                        drugs = [<<"Rupafin">>, <<"Ibuprofen">>], date_prescribed = <<"02/04/2018">>,
                        date_processed = <<"06/04/2018">>, is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(compare_prescriptions(One, Two)).

prescription_comparison_wrong_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = <<"2">>, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_patient_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = <<"3">>, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_pharmacy_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = <<"4">>, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_prescriber_id() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = <<"5">>,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_date_prescribed() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2019",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 5,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_date_processed() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 5,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = <<"06/04/2019">>, is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_is_processed() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 5,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_wrong_drugs() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 5,
                          drugs = ["Acetaminophen", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_NOT_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).

prescription_comparison_missing_drugs() ->
    One = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Two = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 5,
                          drugs = ["Rupafin"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    ?_assert(not compare_prescriptions(One, Two)).


staff_comparison_test_() ->
    [
        identity_staff_comparison(),
        staff_comparison_binary_id(),
        staff_comparison_binary_string_fields(),
        staff_comparison_all_binary_fields(),
        staff_comparison_with_single_nested_prescription(),
        staff_comparison_unordered_nested_prescriptions(),
        staff_comparison_with_single_prescription_ref(),
        staff_comparison_unordered_prescriptions_refs(),
        staff_comparison_wrong_id(),
        staff_comparison_wrong_name(),
        staff_comparison_wrong_address(),
        staff_comparison_wrong_speciality(),
        staff_comparison_missing_prescriptions(),
        staff_comparison_missing_prescription_refs(),
        staff_comparison_wrong_prescriptions(),
        staff_comparison_wrong_prescription_refs()
    ].

identity_staff_comparison() ->
    Staff = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = []},
    ?_assert(compare_staff(Staff, Staff)).

staff_comparison_binary_id() ->
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = []},
    Two = #staff{id = <<"1">>, name = "a", address = "b", speciality = "s", prescriptions = []},
    ?_assert(compare_staff(One, Two)).

staff_comparison_binary_string_fields() ->
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = []},
    Two = #staff{id = 1, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = []},
    ?_assert(compare_staff(One, Two)).

staff_comparison_all_binary_fields() ->
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = []},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = []},
    ?_assert(compare_staff(One, Two)).

staff_comparison_with_single_nested_prescription() ->
    Presc = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Presc]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = [Presc]},
    ?_assert(compare_staff(One, Two)).

staff_comparison_unordered_nested_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 3, pharmacy_id = 3, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Presc2, Presc1]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>,
                 prescriptions = [Presc1, Presc2]},
    ?_assert(compare_staff(One, Two)).

staff_comparison_with_single_prescription_ref() ->
    Ref = <<"prescription_345">>,
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Ref]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = [Ref]},
    ?_assert(compare_staff(One, Two)).

staff_comparison_unordered_prescriptions_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Ref1, Ref2, Ref3]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>,
                 prescriptions = [Ref3, Ref1, Ref2]},
    ?_assert(compare_staff(One, Two)).

staff_comparison_wrong_id() ->
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = []},
    Two = #staff{id = <<"2">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = []},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_wrong_name() ->
    One = #staff{id = 1, name = "d", address = "b", speciality = "s", prescriptions = []},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = []},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_wrong_address() ->
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = []},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"bbbb">>, speciality = <<"s">>, prescriptions = []},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_wrong_speciality() ->
    One = #staff{id = 1, name = "a", address = "b", speciality = "x", prescriptions = []},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = []},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_missing_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 3, pharmacy_id = 3, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Presc2]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>,
                 prescriptions = [Presc1, Presc2]},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_missing_prescription_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Ref1, Ref2, Ref3]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = [Ref3, Ref2]},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_wrong_prescriptions() ->
    Presc1 = #prescription{id = 1, patient_id = 2, pharmacy_id = 3, prescriber_id = 4,
                          drugs = ["Rupafin", "Ibuprofen"], date_prescribed = "02/04/2018",
                          date_processed = "06/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    Presc2 = #prescription{id = 3, patient_id = 3, pharmacy_id = 3, prescriber_id = 5,
                           drugs = ["Rupafin", "Epinephrine"], date_prescribed = "03/04/2018",
                           date_processed = "07/04/2018", is_processed = ?PRESCRIPTION_PROCESSED_VALUE},
    One = #staff{id = 1, name = "a", address = "b", speciality = "s",
                 prescriptions = [Presc1, Presc2#prescription{id = 33333}]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>,
                 prescriptions = [Presc1, Presc2]},
    ?_assert(not compare_staff(One, Two)).

staff_comparison_wrong_prescription_refs() ->
    Ref1 = <<"prescription_345">>,
    Ref2 = <<"prescription_512">>,
    Ref3 = <<"prescription_101">>,
    One = #staff{id = 1, name = "a", address = "b", speciality = "s", prescriptions = [Ref1, Ref3]},
    Two = #staff{id = <<"1">>, name = <<"a">>, address = <<"b">>, speciality = <<"s">>, prescriptions = [Ref3, Ref2]},
    ?_assert(not compare_staff(One, Two)).


-endif.
