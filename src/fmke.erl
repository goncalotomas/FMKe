%% TODO redefine types (we will in the future support databases without CRDTs)
-module(fmke).
-include("fmke.hrl").

-behaviour(gen_server).

%%-----------------------------------------------------------------------------
%% Public API for FMK Core
%%-----------------------------------------------------------------------------
-export([
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
    get_staff_treatments/1,
    process_prescription/2,
    update_patient_details/3,
    update_pharmacy_details/3,
    update_facility_details/4,
    update_staff_details/4,
    update_prescription_medication/3
  ]).

-export ([start/1, stop/0]).

%% gen_server callbacks
-export([
  init/1,
  handle_cast/2,
  handle_call/3
]).

%% TODO switch to stateful modules
-define (DB_DRIVER(), fmke_config:get(driver)).
-define (SERVER, ?MODULE).

start(InitParams) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitParams, []).

stop() ->
    gen_server:call(?MODULE, stop).

init(InitParams) ->
    Driver = proplists:get_value(driver, InitParams),
    Driver:start(InitParams),
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

%%-----------------------------------------------------------------------------
%% Create functions - no transactional context
%%-----------------------------------------------------------------------------

%% Adds a patient to the FMK system, needing only an ID, Name and Address.
%% A check is done to determine if a patient with the given ID already exists,
%% and if so the operation fails.
-spec create_patient(id(), string(), string()) -> ok | {error, reason()}.
create_patient(Id, Name, Address) ->
    gen_server:call(?MODULE, {create_patient, Id, Name, Address}).

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet been seen.
-spec create_pharmacy(id(), string(), string()) -> ok | {error, reason()}.
create_pharmacy(Id, Name, Address) ->
    gen_server:call(?MODULE, {create_pharmacy, Id, Name, Address}).

%% Adds a facility to the FMK-- system if the ID for the facility has not yet been seen.
-spec create_facility(id(), string(), string(), string()) -> ok | {error, reason()}.
create_facility(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {create_facility, Id, Name, Address, Type}).

%% Adds a staff member to the FMK-- system if the ID for the member has not yet been seen.
-spec create_staff(id(), string(), string(), string()) -> ok | {error, reason()}.
create_staff(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {create_staff, Id, Name, Address, Speciality}).

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy. The prescription also includes the prescription date and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    gen_server:call(?MODULE,
        {create_prescription, PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs}
    ).

%%-----------------------------------------------------------------------------
%% Read functions - no transactional context
%%-----------------------------------------------------------------------------

%% Fetches a patient by ID.
-spec get_patient_by_id(id()) -> [crdt()] | {error, reason()}.
get_patient_by_id(Id) ->
    gen_server:call(?MODULE, {get_patient_by_id, Id}).

%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> [crdt()] | {error, reason()}.
get_facility_by_id(Id) ->
    gen_server:call(?MODULE, {get_facility_by_id, Id}).

%% Fetches a pharmacy by ID.
-spec get_pharmacy_by_id(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_by_id(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_by_id, Id}).

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> [crdt()] | {error, reason()}.
get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {get_prescription_by_id, Id}).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_prescriptions, Id}).

-spec get_processed_pharmacy_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_processed_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_processed_pharmacy_prescriptions, Id}).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [crdt()] | {error, reason()}.
get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {get_prescription_medication, Id}).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> [crdt()] | {error, reason()}.
get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {get_staff_by_id, Id}).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> [crdt()] | {error, reason()}.
get_staff_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_staff_prescriptions, Id}).

%% Fetches a list of treatments given a certain staff member ID.
-spec get_staff_treatments(id()) -> [crdt()] | {error, reason()}.
get_staff_treatments(_Id) ->
    erlang:error(not_implemented).

%%-----------------------------------------------------------------------------
%% Update functions - no transactional context
%%-----------------------------------------------------------------------------

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(), string(), string()) -> ok | {error, reason()}.
update_patient_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update_patient_details, Id, Name, Address}).

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(), string(), string()) -> ok | {error, reason()}.
update_pharmacy_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update_pharmacy_details, Id, Name, Address}).

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_facility_details(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {update_facility_details, Id, Name, Address, Type}).

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_staff_details(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {update_staff_details, Id, Name, Address, Speciality}).

-spec update_prescription_medication(id(), atom(), [string()]) -> ok | {error, reason()}.
update_prescription_medication(Id, Operation, Drugs) ->
    gen_server:call(?MODULE, {update_prescription_medication, Id, Operation, Drugs}).

process_prescription(Id, Date) ->
    gen_server:call(?MODULE, {process_prescription, Id, Date}).
