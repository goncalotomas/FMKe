%% This module is a passthrough adapter for FMKe, and it is used for benchmark executions with "complete" drivers.
%% Complete drivers are driver modules that implement the entire FMKe API. Since this type of implementation allows
%% developers to be fully aware of the context and operation that is being performed, this allows them to fully take
%% advantage of any relevant database features (e.g. indexes) to maximize performance.
%% Ideally, every FMKe driver would be written using this approach in order to get the fairest result comparison
%% possible.
%% However, we acknowledge there is some complexity associated with implementing the driver this way, and we provide an
%% alternative, easier approach where only a minimal set of basic operations such as get/put are required to be
%% implemented.
%% There are some databases with both types of drivers implemented, and this is because we want to measure the impact in
%% performance that implementing a driver with only a generic interface. We hope to publish this comparison as well as
%% separated performance rankings for all evaluated storage systems.
-module(fmke_pt_adapter).

-behaviour(gen_fmke_adapter).

%% gen_server callbacks
-export ([
  init/1,
  handle_cast/2,
  handle_call/3
]).

%% gen_fmke_adapter callbacks
-export([
    start/1,
    stop/0,
    create_patient/3,
    create_pharmacy/3,
    create_facility/4,
    create_staff/4,
    create_prescription/6,
    get_facility_by_id/1,
    get_patient_by_id/1,
    get_pharmacy_by_id/1,
    get_processed_pharmacy_prescriptions/1,
    get_pharmacy_prescriptions/1,
    get_prescription_by_id/1,
    get_prescription_medication/1,
    get_staff_by_id/1,
    get_staff_prescriptions/1,
    process_prescription/2,
    update_patient_details/3,
    update_pharmacy_details/3,
    update_facility_details/4,
    update_staff_details/4,
    update_prescription_medication/3
  ]).

start(Driver) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Driver], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([[Driver, _DataModel]]) ->
    {ok, Driver}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({create_patient, Id, Name, Address}, _From, Driver) ->
    {reply, Driver:create_patient(Id, Name, Address), Driver};

handle_call({create_pharmacy, Id, Name, Address}, _From, Driver) ->
    {reply, Driver:create_pharmacy(Id, Name, Address), Driver};

handle_call({create_facility, Id, Name, Address, Type}, _From, Driver) ->
    {reply, Driver:create_facility(Id, Name, Address, Type), Driver};

handle_call({create_staff, Id, Name, Address, Speciality}, _From, Driver) ->
    {reply, Driver:create_staff(Id, Name, Address, Speciality), Driver};

handle_call({create_prescription, Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs}, _From, Driver) ->
    {reply, Driver:create_prescription(Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs), Driver};

handle_call({get_facility_by_id, Id}, _From, Driver) ->
    {reply, Driver:get_facility_by_id(Id), Driver};

handle_call({get_patient_by_id, Id}, _From, Driver) ->
    {reply, Driver:get_patient_by_id(Id), Driver};

handle_call({get_pharmacy_by_id, Id}, _From, Driver) ->
    {reply, Driver:get_pharmacy_by_id(Id), Driver};

handle_call({get_pharmacy_prescriptions, Id}, _From, Driver) ->
    {reply, Driver:get_pharmacy_prescriptions(Id), Driver};

handle_call({get_processed_pharmacy_prescriptions, Id}, _From, Driver) ->
    {reply, Driver:get_processed_pharmacy_prescriptions(Id), Driver};

handle_call({get_prescription_by_id, Id}, _From, Driver) ->
    {reply, Driver:get_prescription_by_id(Id), Driver};

handle_call({get_prescription_medication, Id}, _From, Driver) ->
    {reply, Driver:get_prescription_medication(Id), Driver};

handle_call({get_staff_by_id, Id}, _From, Driver) ->
    {reply, Driver:get_staff_by_id(Id), Driver};

handle_call({get_staff_prescriptions, Id}, _From, Driver) ->
    {reply, Driver:get_staff_prescriptions(Id), Driver};

handle_call({update_patient_details, Id, Name, Address}, _From, Driver) ->
    {reply, Driver:update_patient_details(Id, Name, Address), Driver};

handle_call({update_pharmacy_details, Id, Name, Address}, _From, Driver) ->
    {reply, Driver:update_pharmacy_details(Id, Name, Address), Driver};

handle_call({update_facility_details, Id, Name, Address, Type}, _From, Driver) ->
    {reply, Driver:update_facility_details(Id, Name, Address, Type), Driver};

handle_call({update_staff_details, Id, Name, Address, Speciality}, _From, Driver) ->
    {reply, Driver:update_staff_details(Id, Name, Address, Speciality), Driver};

handle_call({update_prescription_medication, Id, Operation, Drugs}, _From, Driver) ->
    {reply, Driver:update_prescription_medication(Id, Operation, Drugs), Driver};

handle_call({process_prescription, Id, Date}, _From, Driver) ->
    {reply, Driver:process_prescription(Id, Date), Driver}.

%% Application callbacks

create_patient(Id, Name, Address) ->
    gen_server:call(?MODULE, {create_patient, Id, Name, Address}).

create_pharmacy(Id, Name, Address) ->
    gen_server:call(?MODULE, {create_pharmacy, Id, Name, Address}).

create_facility(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {create_facility, Id, Name, Address, Type}).

create_staff(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {create_staff, Id, Name, Address, Speciality}).

create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    gen_server:call(?MODULE,
        {create_prescription, PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs}
    ).

get_patient_by_id(Id) ->
    gen_server:call(?MODULE, {get_patient_by_id, Id}).

get_facility_by_id(Id) ->
    gen_server:call(?MODULE, {get_facility_by_id, Id}).

get_pharmacy_by_id(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_by_id, Id}).

get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {get_prescription_by_id, Id}).

get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_prescriptions, Id}).

get_processed_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_processed_pharmacy_prescriptions, Id}).

get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {get_prescription_medication, Id}).

get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {get_staff_by_id, Id}).

get_staff_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_staff_prescriptions, Id}).

update_patient_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update_patient_details, Id, Name, Address}).

update_pharmacy_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update_pharmacy_details, Id, Name, Address}).

update_facility_details(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {update_facility_details, Id, Name, Address, Type}).

update_staff_details(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {update_staff_details, Id, Name, Address, Speciality}).

update_prescription_medication(Id, Operation, Drugs) ->
    gen_server:call(?MODULE, {update_prescription_medication, Id, Operation, Drugs}).

process_prescription(Id, Date) ->
    gen_server:call(?MODULE, {process_prescription, Id, Date}).
