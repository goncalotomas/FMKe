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
    get_status/0,
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

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init([Adapter]) ->
    lager:info("~p will use the ~p adapter~n", [?MODULE, Adapter]),
    {ok, Adapter}.

get_status() ->
    Status = is_alive(fmke),
    ConnManagerStatus = is_alive(fmke_db_conn_manager),
    WebServerStatus = is_alive(cowboy_sup),
    {ok, Pools} = application:get_env(?APP, pools),
    PoolStatuses = lists:map(
                        fun(Pool) ->
                            PoolUp = is_alive(Pool),
                            {PoolStatus, CurrPoolSize, CurrOverflow, _Monitors} = gen_server:call(Pool, status),
                            [
                                {pool_is_up, PoolUp}, {pool_status, PoolStatus},
                                {worker_pool_size, CurrPoolSize}, {current_overflow, CurrOverflow}
                            ]
                        end, Pools),
    PoolDetails = lists:zip(Pools, PoolStatuses),
    {ok, TargetDatabase} = application:get_env(?APP, target_database),
    {ok, HttpPort} = application:get_env(?APP, http_port),
    {ok, ConnPoolSize} = application:get_env(?APP, connection_pool_size),
    {ok, Addresses} = application:get_env(?APP, database_addresses),
    {ok, Ports} = application:get_env(?APP, database_ports),
    [{fmke_up, Status}, {connection_manager_up, ConnManagerStatus}, {web_server_up, WebServerStatus},
     {target_database, TargetDatabase}, {connection_pool_size, ConnPoolSize}, {http_port, HttpPort},
     {database_addresses, lists:map(fun list_to_binary/1, Addresses)}, {database_ports, Ports}, {pools, PoolDetails}].

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({create_patient, Id, Name, Address}, _From, Adapter) ->
    {reply, Adapter:create_patient(Id, Name, Address), Adapter};

handle_call({create_pharmacy, Id, Name, Address}, _From, Adapter) ->
    {reply, Adapter:create_pharmacy(Id, Name, Address), Adapter};

handle_call({create_facility, Id, Name, Address, Type}, _From, Adapter) ->
    {reply, Adapter:create_facility(Id, Name, Address, Type), Adapter};

handle_call({create_staff, Id, Name, Address, Speciality}, _From, Adapter) ->
    {reply, Adapter:create_staff(Id, Name, Address, Speciality), Adapter};

handle_call({create_prescription, Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs}, _From, Adapter) ->
    {reply, Adapter:create_prescription(Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs), Adapter};

handle_call({get_facility_by_id, Id}, _From, Adapter) ->
    {reply, Adapter:get_facility_by_id(Id), Adapter};

handle_call({get_patient_by_id, Id}, _From, Adapter) ->
    {reply, Adapter:get_patient_by_id(Id), Adapter};

handle_call({get_pharmacy_by_id, Id}, _From, Adapter) ->
    {reply, Adapter:get_pharmacy_by_id(Id), Adapter};

handle_call({get_pharmacy_prescriptions, Id}, _From, Adapter) ->
    {reply, Adapter:get_pharmacy_prescriptions(Id), Adapter};

handle_call({get_processed_pharmacy_prescriptions, Id}, _From, Adapter) ->
    {reply, Adapter:get_processed_pharmacy_prescriptions(Id), Adapter};

handle_call({get_prescription_by_id, Id}, _From, Adapter) ->
    {reply, Adapter:get_prescription_by_id(Id), Adapter};

handle_call({get_prescription_medication, Id}, _From, Adapter) ->
    {reply, Adapter:get_prescription_medication(Id), Adapter};

handle_call({get_staff_by_id, Id}, _From, Adapter) ->
    {reply, Adapter:get_staff_by_id(Id), Adapter};

handle_call({get_staff_prescriptions, Id}, _From, Adapter) ->
    {reply, Adapter:get_staff_prescriptions(Id), Adapter};

handle_call({update_patient_details, Id, Name, Address}, _From, Adapter) ->
    {reply, Adapter:update_patient_details(Id, Name, Address), Adapter};

handle_call({update_pharmacy_details, Id, Name, Address}, _From, Adapter) ->
    {reply, Adapter:update_pharmacy_details(Id, Name, Address), Adapter};

handle_call({update_facility_details, Id, Name, Address, Type}, _From, Adapter) ->
    {reply, Adapter:update_facility_details(Id, Name, Address, Type), Adapter};

handle_call({update_staff_details, Id, Name, Address, Speciality}, _From, Adapter) ->
    {reply, Adapter:update_staff_details(Id, Name, Address, Speciality), Adapter};

handle_call({update_prescription_medication, Id, Operation, Drugs}, _From, Adapter) ->
    {reply, Adapter:update_prescription_medication(Id, Operation, Drugs), Adapter};

handle_call({process_prescription, Id, Date}, _From, Adapter) ->
    {reply, Adapter:process_prescription(Id, Date), Adapter}.

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
-spec get_patient_by_id(id()) -> patient() | {error, reason()}.
get_patient_by_id(Id) ->
    gen_server:call(?MODULE, {get_patient_by_id, Id}).

%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> facility() | {error, reason()}.
get_facility_by_id(Id) ->
    gen_server:call(?MODULE, {get_facility_by_id, Id}).

%% Fetches a pharmacy by ID.
-spec get_pharmacy_by_id(id()) -> pharmacy() | {error, reason()}.
get_pharmacy_by_id(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_by_id, Id}).

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> prescription() | {error, reason()}.
get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {get_prescription_by_id, Id}).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> list(prescription() | binary()) | {error, reason()}.
get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_pharmacy_prescriptions, Id}).

-spec get_processed_pharmacy_prescriptions(id()) -> list(prescription() | binary()) | {error, reason()}.
get_processed_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_processed_pharmacy_prescriptions, Id}).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [binary()] | {error, reason()}.
get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {get_prescription_medication, Id}).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> staff() | {error, reason()}.
get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {get_staff_by_id, Id}).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> list(prescription() | binary()) | {error, reason()}.
get_staff_prescriptions(Id) ->
    gen_server:call(?MODULE, {get_staff_prescriptions, Id}).

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

%%-----------------------------------------------------------------------------
%% Helper functions
%%-----------------------------------------------------------------------------

is_alive(Proc) ->
    undefined =/= whereis(Proc).
