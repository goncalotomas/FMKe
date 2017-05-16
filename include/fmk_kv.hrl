%%%-------------------------------------------------------------------
%%% @author goncalotomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Mar 2017 11:18
%%%-------------------------------------------------------------------
-author("goncalotomas").

%% FMKe entity keys
-define (PATIENT_ID_KEY, <<"patient_id">>).
-define (PATIENT_NAME_KEY, <<"patient_name">>).
-define (PATIENT_ADDRESS_KEY, <<"patient_address">>).
-define (PATIENT_TREATMENTS_KEY, <<"patient_treatments">>).
-define (PATIENT_PRESCRIPTIONS_KEY, <<"patient_prescriptions">>).
-define (PATIENT_EVENTS_KEY, <<"patient_events">>).

%% Pharmacy macros
-define (PHARMACY_ID_KEY, <<"pharmacy_id">>).
-define (PHARMACY_NAME_KEY, <<"pharmacy_name">>).
-define (PHARMACY_ADDRESS_KEY, <<"pharmacy_address">>).
-define (PHARMACY_PRESCRIPTIONS_KEY, <<"pharmacy_prescriptions">>).

%% Prescription macros
-define (PRESCRIPTION_ID_KEY, <<"prescription_id">>).
-define (PRESCRIPTION_PATIENT_ID_KEY, <<"prescription_patient_id">>).
-define (PRESCRIPTION_PRESCRIBER_ID_KEY, <<"prescription_prescriber_id">>).
-define (PRESCRIPTION_PHARMACY_ID_KEY, <<"prescription_pharmacy_id">>).
-define (PRESCRIPTION_FACILITY_ID_KEY, <<"prescription_facility_id">>).
-define (PRESCRIPTION_DATE_PRESCRIBED_KEY, <<"prescription_date_prescribed">>).
-define (PRESCRIPTION_IS_PROCESSED_KEY, <<"prescription_is_processed">>).
-define (PRESCRIPTION_DATE_PROCESSED_KEY, <<"prescription_date_processed">>).
-define (PRESCRIPTION_DRUGS_KEY, <<"prescription_drugs">>).
-define (PRESCRIPTION_NOT_PROCESSED_VALUE, <<"prescription_not_processed">>).
-define (PRESCRIPTION_PROCESSED_VALUE, <<"prescription_processed">>).

%% Treatment macros
-define (TREATMENT_ID_KEY, <<"treatment_id">>).
-define (TREATMENT_PATIENT_ID_KEY, <<"treatment_patient_id">>).
-define (TREATMENT_PRESCRIBER_ID_KEY, <<"treatment_prescriber_id">>).
-define (TREATMENT_FACILITY_ID_KEY, <<"treatment_facility_id">>).
-define (TREATMENT_DATE_PRESCRIBED_KEY, <<"treatment_date_prescribed">>).
-define (TREATMENT_HAS_ENDED_KEY, <<"treatment_has_ended">>).
-define (TREATMENT_DATE_ENDED_KEY, <<"treatment_date_ended">>).
-define (TREATMENT_PRESCRIPTIONS_KEY, <<"treatment_prescriptions">>).
-define (TREATMENT_EVENTS_KEY, <<"treatment_events">>).
-define (TREATMENT_ONGOING_KEY, "ongoing_treatment").
-define (TREATMENT_ENDED_KEY, "finished_treatment").

%% Medical Staff macros
-define (STAFF_ID_KEY, <<"staff_id">>).
-define (STAFF_NAME_KEY, <<"staff_name">>).
-define (STAFF_ADDRESS_KEY, <<"staff_address">>).
-define (STAFF_SPECIALITY_KEY, <<"staff_speciality">>).
-define (STAFF_PRESCRIPTIONS_KEY, <<"staff_prescriptions">>).
-define (STAFF_TREATMENTS_KEY, <<"staff_treatments">>).

%% Facility macros
-define (FACILITY_ID_KEY, <<"facility_id">>).
-define (FACILITY_NAME_KEY, <<"facility_name">>).
-define (FACILITY_ADDRESS_KEY, <<"facility_address">>).
-define (FACILITY_TYPE_KEY, <<"facility_type">>).
-define (FACILITY_PRESCRIPTIONS_KEY, <<"facility_prescriptions">>).
-define (FACILITY_TREATMENTS_KEY, <<"facility_treatments">>).

%% Event macros
-define (EVENT_ID_KEY, <<"event_id">>).
-define (EVENT_PATIENT_ID_KEY, <<"event_patient_id">>).
-define (EVENT_DESCRIPTION_KEY, <<"event_description">>).
-define (EVENT_TIMESTAMP_KEY, <<"event_timestamp">>).
-define (EVENT_STAFF_ID_KEY, <<"event_staff_id">>).