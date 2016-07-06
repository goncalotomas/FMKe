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

%% System key definitions
-define (FMK_PATIENTS, fmk_patients).
-define (FMK_PRESCRIPTIONS, fmk_prescriptions).
-define (FMK_TREATMENT_FACILITIES, fmk_facilities).
-define (FMK_MEDICAL_STAFF, fmk_staff).

%% Test macros
-define(TEST_COUNTER_TYPE, riak_dt_pncounter).
-define(TEST_COUNTER_KEY, 'fmk_counter_test').
-define(TEST_MAP_KEY, 'fmk_map_test').
-define(TEST_NESTED_MAP_KEY, 'fmk_nested_map_test').