-define(ANTIDOTE, 'antidote@127.0.0.1').
%% Useful shortcut macros
-define (BCOUNTER, riak_dt_bcounter).
-define (COUNTER, riak_dt__counter).
-define (GSET, riak_dt_gset).
-define (LWWREG, riak_dt_lwwreg).
-define (MAP, antidote_crdt_map).
-define (NESTED_MAP, riak_dt_map).
-define (MVREG, riak_dt_mvreg).
-define (ORSET, antidote_crdt_orset).
-define (RGA, riak_dt_rga).

%% Patient macros
-define (PATIENT_ID, patient_id).
-define (PATIENT_ID_CRDT, riak_dt_gcounter).
-define (PATIENT_NAME, patient_name).
-define (PATIENT_NAME_CRDT, riak_dt_lwwreg).
-define (PATIENT_ADDRESS, patient_address).
-define (PATIENT_ADDRESS_CRDT, riak_dt_lwwreg).
-define (PATIENT_TREATMENTS, patient_treatments).
-define (PATIENT_TREATMENTS_CRDT, riak_dt_map).
-define (PATIENT_PRESCRIPTIONS, patient_prescriptions).
-define (PATIENT_PRESCRIPTIONS_CRDT, riak_dt_map).
-define (PATIENT_EVENTS, patient_events).
-define (PATIENT_EVENTS_CRDT, riak_dt_map).

%% Pharmacy macros
-define (PHARMACY_ID, pharmacy_id).
-define (PHARMACY_ID_CRDT, riak_dt_gcounter).
-define (PHARMACY_NAME, pharmacy_name).
-define (PHARMACY_NAME_CRDT, riak_dt_lwwreg).
-define (PHARMACY_ADDRESS, patient_address).
-define (PHARMACY_ADDRESS_CRDT, riak_dt_lwwreg).
-define (PHARMACY_PRESCRIPTIONS, patient_prescriptions).
-define (PHARMACY_PRESCRIPTIONS_CRDT, riak_dt_map).

%% Medical Staff macros
-define (STAFF_ID, staff_id).
-define (STAFF_ID_CRDT, riak_dt_gcounter).
-define (STAFF_NAME, staff_name).
-define (STAFF_NAME_CRDT, riak_dt_lwwreg).
-define (STAFF_ADDRESS, staff_address).
-define (STAFF_ADDRESS_CRDT, riak_dt_lwwreg).
-define (STAFF_SPECIALITY, staff_speciality).
-define (STAFF_SPECIALITY_CRDT, riak_dt_lwwreg).
-define (STAFF_PRESCRIPTIONS, staff_prescriptions).
-define (STAFF_PRESCRIPTIONS_CRDT, riak_dt_map).
-define (STAFF_TREATMENTS, staff_treatments).
-define (STAFF_TREATMENTS_CRDT, riak_dt_map).

%% FMK macros
-define (FMK_FACILITY_NAME_INDEX, <<"facility_name_index">>).
-define (FMK_PHARMACY_NAME_INDEX, <<"pharmacy_name_index">>).
-define (FMK_PATIENT_NAME_INDEX, <<"patient_name_index">>).
-define (FMK_STAFF_NAME_INDEX, <<"staff_name_index">>).

%% Test macros
-define(TEST_COUNTER_TYPE, riak_dt_pncounter).
-define(TEST_COUNTER_KEY, 'fmk_counter_test').
-define(TEST_MAP_KEY, 'fmk_map_test').
-define(TEST_NESTED_MAP_KEY, 'fmk_nested_map_test').