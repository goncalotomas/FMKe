%% TODO redefine types (we will in the future support databases without CRDTs)
-module(fmke).
-include("fmke.hrl").

-behaviour(gen_server).

%%-----------------------------------------------------------------------------
%% Public API for FMK Core
%%-----------------------------------------------------------------------------
-export([
    start_link/1,
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

%% gen_server callbacks
-export([
  init/1,
  handle_cast/2,
  handle_call/3
]).

-define (SERVER, ?MODULE).

start_link(_Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    % {ok, Adapter} = application:get_env(?APP, adapter),
    {ok, ConnPoolSize} = application:get_env(?APP, connection_pool_size),
    {ok, Addresses} = application:get_env(?APP, database_addresses),
    {ok, Ports} = application:get_env(?APP, database_ports),
    {ok, Database} = application:get_env(?APP, target_database),
    {Driver, DriverImpl} = get_driver_setup(Database),
    % TODO this is going to be removed in a future release, since drivers will not need these parameters
    ok = application:set_env(?APP, driver, Driver),
    ok = application:set_env(?APP, simplified_driver, DriverImpl),
    ok = application:set_env(?APP, db_conn_hostnames, Addresses),
    ok = application:set_env(?APP, db_conn_ports, Ports),
    ok = application:set_env(?APP, db_conn_pool_size, ConnPoolSize),
    Driver:start([]),
    {ok, Driver}.

get_driver_setup(Database) when is_list(Database) ->
    get_driver_setup(list_to_atom(Database));

get_driver_setup(Database) when is_atom(Database) ->
    %% TODO maybe I should find a better way to do this... later.
    DriverSetups = #{
      antidote => {fmke_kv_driver, fmke_db_driver_antidote},
      antidote_norm => {fmke_db_driver_antidote_norm, undefined},
      redis => {fmke_kv_driver, fmke_db_driver_redis},
      riak => {fmke_kv_driver, fmke_db_driver_riak_kv},
      riak_kv => {fmke_kv_driver, fmke_db_driver_riak_kv},
      riak_norm => {fmke_db_driver_riak_kv_norm, undefined},
      riak_kv_norm => {fmke_db_driver_riak_kv_norm, undefined}
    },
    case maps:find(Database, DriverSetups) of
        {ok, Value} -> Value;
        error -> {error, not_supported, Database}
    end.

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
