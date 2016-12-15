%%-define(ANTIDOTE, 'antidote@127.0.0.1').
-define(DEFAULT_ANTIDOTE_PORT, "8087").
-define(DEFAULT_ANTIDOTE_ADDRESS, "127.0.0.1").
-define (DEFAULT_FMKE_HTTP_PORT, "9090").
-define (APP, fmke).
-define (VAR_ANTIDOTE_PB_PID, antidote_pb_pid).
-define (VAR_ANTIDOTE_PB_ADDRESS, antidote_pb_address).
-define (VAR_ANTIDOTE_PB_PORT, antidote_pb_port).
%% Useful shortcut macros
-define (BCOUNTER, antidote_crdt_bcounter).
-define (counter, antidote_crdt_counter).
-define (GSET, antidote_crdt_gset).
-define (LWWREG, antidote_crdt_lwwreg).
-define (MAP, antidote_crdt_gmap).
-define (NESTED_MAP, antidote_crdt_gmap).
-define (NESTED_RMAP, antidote_crdt_map_rr).
-define (MVREG, antidote_crdt_mvreg).
-define (ORSET, antidote_crdt_orset).
-define (RGA, antidote_crdt_rga).

%% Patient macros
-define (PATIENT_ID, <<"patient_id">>).
-define (PATIENT_ID_CRDT, antidote_crdt_lwwreg).
-define (PATIENT_NAME, <<"patient_name">>).
-define (PATIENT_NAME_CRDT, antidote_crdt_lwwreg).
-define (PATIENT_ADDRESS, <<"patient_address">>).
-define (PATIENT_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (PATIENT_TREATMENTS, <<"patient_treatments">>).
-define (PATIENT_TREATMENTS_CRDT, antidote_crdt_gmap).
-define (PATIENT_PRESCRIPTIONS, <<"patient_prescriptions">>).
-define (PATIENT_PRESCRIPTIONS_CRDT, antidote_crdt_gmap).
-define (PATIENT_EVENTS, <<"patient_events">>).
-define (PATIENT_EVENTS_CRDT, antidote_crdt_gmap).

%% Pharmacy macros
-define (PHARMACY_ID, <<"pharmacy_id">>).
-define (PHARMACY_ID_CRDT, antidote_crdt_lwwreg).
-define (PHARMACY_NAME, <<"pharmacy_name">>).
-define (PHARMACY_NAME_CRDT, antidote_crdt_lwwreg).
-define (PHARMACY_ADDRESS, <<"patient_address">>).
-define (PHARMACY_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (PHARMACY_PRESCRIPTIONS, <<"pharmacy_prescriptions">>).
-define (PHARMACY_PRESCRIPTIONS_CRDT, antidote_crdt_gmap).

%% Prescription macros
-define (PRESCRIPTION_ID, <<"prescription_id">>).
-define (PRESCRIPTION_ID_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_PATIENT_ID, <<"prescription_patient_id">>).
-define (PRESCRIPTION_PATIENT_ID_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_PRESCRIBER_ID, <<"prescription_prescriber_id">>).
-define (PRESCRIPTION_PRESCRIBER_ID_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_PHARMACY_ID, <<"prescription_pharmacy_id">>).
-define (PRESCRIPTION_PHARMACY_ID_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_FACILITY_ID, <<"prescription_facility_id">>).
-define (PRESCRIPTION_FACILITY_ID_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_DATE_PRESCRIBED, <<"prescription_date_prescribed">>).
-define (PRESCRIPTION_DATE_PRESCRIBED_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_IS_PROCESSED, <<"prescription_is_processed">>).
-define (PRESCRIPTION_IS_PROCESSED_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_DATE_PROCESSED, <<"prescription_date_processed">>).
-define (PRESCRIPTION_DATE_PROCESSED_CRDT, antidote_crdt_mvreg).
-define (PRESCRIPTION_DRUGS, <<"prescription_drugs">>).
-define (PRESCRIPTION_DRUGS_CRDT, antidote_crdt_orset).
-define (PRESCRIPTION_NOT_PROCESSED, "prescription_not_processed").
-define (PRESCRIPTION_PROCESSED, "prescription_processed").

%% Treatment macros
-define (TREATMENT_ID, <<"treatment_id">>).
-define (TREATMENT_ID_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_PATIENT_ID, <<"treatment_patient_id">>).
-define (TREATMENT_PATIENT_ID_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_PRESCRIBER_ID, <<"treatment_prescriber_id">>).
-define (TREATMENT_PRESCRIBER_ID_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_FACILITY_ID, <<"treatment_facility_id">>).
-define (TREATMENT_FACILITY_ID_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_DATE_PRESCRIBED, <<"treatment_date_prescribed">>).
-define (TREATMENT_DATE_PRESCRIBED_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_HAS_ENDED, <<"treatment_has_ended">>).
-define (TREATMENT_HAS_ENDED_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_DATE_ENDED, <<"treatment_date_ended">>).
-define (TREATMENT_DATE_ENDED_CRDT, antidote_crdt_lwwreg).
-define (TREATMENT_PRESCRIPTIONS, <<"treatment_prescriptions">>).
-define (TREATMENT_PRESCRIPTIONS_CRDT, antidote_crdt_gmap).
-define (TREATMENT_EVENTS, <<"treatment_events">>).
-define (TREATMENT_EVENTS_CRDT, antidote_crdt_gmap).
-define (TREATMENT_ONGOING, "ongoing_treatment").
-define (TREATMENT_ENDED, "finished_treatment").

%% Medical Staff macros
-define (STAFF_ID, <<"staff_id">>).
-define (STAFF_ID_CRDT, antidote_crdt_lwwreg).
-define (STAFF_NAME, <<"staff_name">>).
-define (STAFF_NAME_CRDT, antidote_crdt_lwwreg).
-define (STAFF_ADDRESS, <<"staff_address">>).
-define (STAFF_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (STAFF_SPECIALITY, <<"staff_speciality">>).
-define (STAFF_SPECIALITY_CRDT, antidote_crdt_lwwreg).
-define (STAFF_PRESCRIPTIONS, <<"staff_prescriptions">>).
-define (STAFF_PRESCRIPTIONS_CRDT, antidote_crdt_map_rr).
-define (STAFF_TREATMENTS, <<"staff_treatments">>).
-define (STAFF_TREATMENTS_CRDT, antidote_crdt_gmap).

%% Facility macros
-define (FACILITY_ID, <<"facility_id">>).
-define (FACILITY_ID_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_NAME, <<"facility_name">>).
-define (FACILITY_NAME_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_ADDRESS, <<"facility_address">>).
-define (FACILITY_ADDRESS_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_TYPE, <<"facility_type">>).
-define (FACILITY_TYPE_CRDT, antidote_crdt_lwwreg).
-define (FACILITY_PRESCRIPTIONS, <<"facility_prescriptions">>).
-define (FACILITY_PRESCRIPTIONS_CRDT, antidote_crdt_gmap).
-define (FACILITY_TREATMENTS, <<"facility_treatments">>).
-define (FACILITY_TREATMENTS_CRDT, antidote_crdt_gmap).

%% Event macros
-define (EVENT_ID, <<"event_id">>).
-define (EVENT_ID_CRDT, antidote_crdt_lwwreg).
-define (EVENT_PATIENT_ID, <<"event_patient_id">>).
-define (EVENT_PATIENT_ID_CRDT, antidote_crdt_lwwreg).
-define (EVENT_DESCRIPTION, <<"event_description">>).
-define (EVENT_DESCRIPTION_CRDT, antidote_crdt_lwwreg).
-define (EVENT_TIMESTAMP, <<"event_timestamp">>).
-define (EVENT_TIMESTAMP_CRDT, antidote_crdt_lwwreg).
-define (EVENT_STAFF_ID, <<"event_staff_id">>).
-define (EVENT_STAFF_ID_CRDT, antidote_crdt_counter).

%% FMK Index macros
-define (FMK_FACILITY_NAME_INDEX, <<"facility_name_index">>).
-define (FMK_PHARMACY_NAME_INDEX, <<"pharmacy_name_index">>).
-define (FMK_PATIENT_NAME_INDEX, <<"patient_name_index">>).
-define (FMK_STAFF_NAME_INDEX, <<"staff_name_index">>).

%% Type specification borrowed from antidote
-type txid() :: {pid(), antidote:txid()}.
-type reason() :: antidote:reason().
-type snapshot_time() :: antidote:snapshot_time().
-type bound_object() :: antidote:bound_object().
-type op_name() :: antidote:op_name().
-type op_param() :: antidote:op_param().
-type crdt() :: term().
-type crdt_op() :: any().
-type field() :: binary().
-type map_field_op() ::  {remove, field()}.
-type map_field_update() :: {update, field(), crdt_op()}.
-type map_op() :: {update, {[map_field_update() | map_field_op()], actorordot()}}.
-type actorordot() :: antidote_crdt:actor() | antidote_crdt:dot().
-type object_bucket() :: {field(), crdt(), term()}.
-type id() :: non_neg_integer().
-define (MAP_UPDATE_OP,update).

%% Test macros
-define(TEST_COUNTER_TYPE, antidote_crdt_pncounter).
-define(TEST_COUNTER_KEY, fmk_counter_test).
-define(TEST_MAP_KEY, 'fmk_map_test').
-define(TEST_NESTED_MAP_KEY, 'fmk_nested_map_test').
