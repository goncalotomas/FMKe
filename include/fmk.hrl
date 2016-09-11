-define(ANTIDOTE, 'antidote@127.0.0.1').
%% Useful shortcut macros
-define (BCOUNTER, riak_dt_bcounter).
-define (GCOUNTER, riak_dt_gcounter).
-define (GSET, riak_dt_gset).
-define (LWWREG, riak_dt_lwwreg).
-define (MAP, antidote_crdt_map).
-define (NESTED_MAP, riak_dt_map).
-define (MVREG, riak_dt_mvreg).
-define (ORSET, antidote_crdt_orset).
-define (RGA, riak_dt_rga).

%% Patient macros
-define (PATIENT_ID, <<"patient_id">>).
-define (PATIENT_ID_CRDT, riak_dt_gcounter).
-define (PATIENT_NAME, <<"patient_name">>).
-define (PATIENT_NAME_CRDT, riak_dt_lwwreg).
-define (PATIENT_ADDRESS, <<"patient_address">>).
-define (PATIENT_ADDRESS_CRDT, riak_dt_lwwreg).
-define (PATIENT_TREATMENTS, <<"patient_treatments">>).
-define (PATIENT_TREATMENTS_CRDT, riak_dt_map).
-define (PATIENT_PRESCRIPTIONS, <<"patient_prescriptions">>).
-define (PATIENT_PRESCRIPTIONS_CRDT, riak_dt_map).
-define (PATIENT_EVENTS, <<"patient_events">>).
-define (PATIENT_EVENTS_CRDT, riak_dt_map).

%% Pharmacy macros
-define (PHARMACY_ID, <<"pharmacy_id">>).
-define (PHARMACY_ID_CRDT, riak_dt_gcounter).
-define (PHARMACY_NAME, <<"pharmacy_name">>).
-define (PHARMACY_NAME_CRDT, riak_dt_lwwreg).
-define (PHARMACY_ADDRESS, <<"patient_address">>).
-define (PHARMACY_ADDRESS_CRDT, riak_dt_lwwreg).
-define (PHARMACY_PRESCRIPTIONS, <<"pharmacy_prescriptions">>).
-define (PHARMACY_PRESCRIPTIONS_CRDT, riak_dt_map).

%% Prescription macros
-define (PRESCRIPTION_ID, <<"prescription_id">>).
-define (PRESCRIPTION_ID_CRDT, riak_dt_gcounter).
-define (PRESCRIPTION_PATIENT_ID, <<"prescription_patient_id">>).
-define (PRESCRIPTION_PATIENT_ID_CRDT, riak_dt_gcounter).
-define (PRESCRIPTION_PRESCRIBER_ID, <<"prescription_prescriber_id">>).
-define (PRESCRIPTION_PRESCRIBER_ID_CRDT, riak_dt_gcounter).
-define (PRESCRIPTION_PHARMACY_ID, <<"prescription_pharmacy_id">>).
-define (PRESCRIPTION_PHARMACY_ID_CRDT, riak_dt_gcounter).
-define (PRESCRIPTION_FACILITY_ID, <<"prescription_facility_id">>).
-define (PRESCRIPTION_FACILITY_ID_CRDT, riak_dt_gcounter).
-define (PRESCRIPTION_DATE_PRESCRIBED, <<"prescription_date_prescribed">>).
-define (PRESCRIPTION_DATE_PRESCRIBED_CRDT, riak_dt_lwwreg).
-define (PRESCRIPTION_IS_PROCESSED, <<"prescription_is_processed">>).
-define (PRESCRIPTION_IS_PROCESSED_CRDT, riak_dt_lwwreg).
-define (PRESCRIPTION_DATE_PROCESSED, <<"prescription_date_processed">>).
-define (PRESCRIPTION_DATE_PROCESSED_CRDT, riak_dt_lwwreg).
-define (PRESCRIPTION_DRUGS, <<"prescription_drugs">>).
-define (PRESCRIPTION_DRUGS_CRDT, riak_dt_orset).

%% Treatment macros
-define (TREATMENT_ID, <<"treatment_id">>).
-define (TREATMENT_ID_CRDT, riak_dt_gcounter).
-define (TREATMENT_PATIENT_ID, <<"treatment_patient_id">>).
-define (TREATMENT_PATIENT_ID_CRDT, riak_dt_gcounter).
-define (TREATMENT_PRESCRIBER_ID, <<"treatment_prescriber_id">>).
-define (TREATMENT_PRESCRIBER_ID_CRDT, riak_dt_gcounter).
-define (TREATMENT_FACILITY_ID, <<"treatment_facility_id">>).
-define (TREATMENT_FACILITY_ID_CRDT, riak_dt_gcounter).
-define (TREATMENT_DATE_PRESCRIBED, <<"treatment_date_prescribed">>).
-define (TREATMENT_DATE_PRESCRIBED_CRDT, riak_dt_lwwreg).
-define (TREATMENT_HAS_ENDED, <<"treatment_has_ended">>).
-define (TREATMENT_HAS_ENDED_CRDT, riak_dt_lwwreg).
-define (TREATMENT_DATE_ENDED, <<"treatment_date_ended">>).
-define (TREATMENT_DATE_ENDED_CRDT, riak_dt_lwwreg).
-define (TREATMENT_PRESCRIPTIONS, <<"treatment_prescriptions">>).
-define (TREATMENT_PRESCRIPTIONS_CRDT, riak_dt_map).
-define (TREATMENT_EVENTS, <<"treatment_events">>).
-define (TREATMENT_EVENTS_CRDT, riak_dt_map).

%% Medical Staff macros
-define (STAFF_ID, <<"staff_id">>).
-define (STAFF_ID_CRDT, riak_dt_gcounter).
-define (STAFF_NAME, <<"staff_name">>).
-define (STAFF_NAME_CRDT, riak_dt_lwwreg).
-define (STAFF_ADDRESS, <<"staff_address">>).
-define (STAFF_ADDRESS_CRDT, riak_dt_lwwreg).
-define (STAFF_SPECIALITY, <<"staff_speciality">>).
-define (STAFF_SPECIALITY_CRDT, riak_dt_lwwreg).
-define (STAFF_PRESCRIPTIONS, <<"staff_prescriptions">>).
-define (STAFF_PRESCRIPTIONS_CRDT, riak_dt_map).
-define (STAFF_TREATMENTS, <<"staff_treatments">>).
-define (STAFF_TREATMENTS_CRDT, riak_dt_map).

%% Facility macros
-define (FACILITY_ID, <<"facility_id">>).
-define (FACILITY_ID_CRDT, riak_dt_gcounter).
-define (FACILITY_NAME, <<"facility_name">>).
-define (FACILITY_NAME_CRDT, riak_dt_lwwreg).
-define (FACILITY_ADDRESS, <<"facility_address">>).
-define (FACILITY_ADDRESS_CRDT, riak_dt_lwwreg).
-define (FACILITY_TYPE, <<"facility_type">>).
-define (FACILITY_TYPE_CRDT, riak_dt_lwwreg).
-define (FACILITY_PRESCRIPTIONS, <<"facility_prescriptions">>).
-define (FACILITY_PRESCRIPTIONS_CRDT, riak_dt_map).
-define (FACILITY_TREATMENTS, <<"facility_treatments">>).
-define (FACILITY_TREATMENTS_CRDT, riak_dt_map).

%% Event macros
-define (EVENT_ID, <<"event_id">>).
-define (EVENT_ID_CRDT, riak_dt_gcounter).
-define (EVENT_PATIENT_ID, <<"event_patient_id">>).
-define (EVENT_PATIENT_ID_CRDT, riak_dt_gcounter).
-define (EVENT_DESCRIPTION, <<"event_description">>).
-define (EVENT_DESCRIPTION_CRDT, riak_dt_lwwreg).
-define (EVENT_TIMESTAMP, <<"event_timestamp">>).
-define (EVENT_TIMESTAMP_CRDT, riak_dt_lwwreg).
-define (EVENT_STAFF_ID, <<"event_staff_id">>).
-define (EVENT_STAFF_ID_CRDT, riak_dt_gcounter).

%% FMK Index macros
-define (FMK_FACILITY_NAME_INDEX, <<"facility_name_index">>).
-define (FMK_PHARMACY_NAME_INDEX, <<"pharmacy_name_index">>).
-define (FMK_PATIENT_NAME_INDEX, <<"patient_name_index">>).
-define (FMK_STAFF_NAME_INDEX, <<"staff_name_index">>).

%% Test macros
-define(TEST_COUNTER_TYPE, riak_dt_pncounter).
-define(TEST_COUNTER_KEY, fmk_counter_test).
-define(TEST_MAP_KEY, 'fmk_map_test').
-define(TEST_NESTED_MAP_KEY, 'fmk_nested_map_test').