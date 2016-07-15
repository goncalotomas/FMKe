-define(ANTIDOTE, 'antidote@127.0.0.1').
%% Useful shortcut macros
-define (BCOUNTER, riak_dt_bcounter).
-define (COUNTER, riak_dt__counter).
-define (GSET, riak_dt_gset).
-define (LWWREG, riak_dt_lwwreg).
-define (MAP, antidote_crdt_map).
-define (NESTED_MAP, riak_dt_map).
-define (MVREG, riak_dt_mvreg).
-define (ORSET, riak_dt_orset).
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

%% FMK macros
-define (FMK_PATIENTS, fmk_patients).
-define (FMK_PRESCRIPTIONS, fmk_prescriptions).
-define (FMK_TREATMENT_FACILITIES, fmk_facilities).
-define (FMK_MEDICAL_STAFF, fmk_staff).

%% Test macros
-define(TEST_COUNTER_TYPE, riak_dt_pncounter).
-define(TEST_COUNTER_KEY, 'fmk_counter_test').
-define(TEST_MAP_KEY, 'fmk_map_test').
-define(TEST_NESTED_MAP_KEY, 'fmk_nested_map_test').