-module(fmkclient).

-export(
  [start_link/0
  ,add_patient/0
  ,add_treatment/0
  ,add_prescription/0
  ,add_event/0
  ,add_medical_staff/0
  ,add_treatment_facility/0
  ,add_pharmacy/0
  ]).



%% ----------------------------------------------------------------------------
%% Client API
%% ----------------------------------------------------------------------------
start_link() -> 
  gen_server:start_link(?MODULE, %% callback module
    [], %% parameters to pass to init 
    []). %% debugging options

add_patient() -> ok.

add_treatment() -> ok.

add_prescription() -> ok.

add_event() -> ok.

add_medical_staff() -> ok.

add_treatment_facility() -> ok.

add_pharmacy() -> ok.