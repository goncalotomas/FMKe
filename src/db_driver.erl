-module(db_driver).
-include("fmk.hrl").

-behaviour(fmke_driver).

-export([
    start_transaction/0,
    create_patient/3,
    create_pharmacy/3,
    create_facility/4,
    create_staff/4,
    create_prescription/7,
    create_prescription/8,
    create_event/5,
    create_treatment/5,
    create_treatment/6,
    get_event_by_id/1,
    get_facility_by_id/1,
    get_facility_treatments/1,
    get_patient_by_id/1,
    get_pharmacy_by_id/1,
    get_processed_pharmacy_prescriptions/1,
    get_pharmacy_prescriptions/1,
    get_prescription_by_id/1,
    get_prescription_medication/1,
    get_staff_by_id/1,
    get_staff_prescriptions/1,
    get_staff_treatments/1,
    get_treatment_by_id/1,
    process_prescription/2,
    update_patient_details/3,
    update_pharmacy_details/3,
    update_facility_details/4,
    update_staff_details/4,
    update_prescription_medication/3
    ]).

