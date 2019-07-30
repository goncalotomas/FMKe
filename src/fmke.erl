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

init([none]) ->
    lager:info("~p is booting in passthrough mode (no adapter)", [?MODULE]),
    {ok, undefined};
init([Adapter]) ->
    lager:info("~p will use the ~p adapter~n", [?MODULE, Adapter]),
    {ok, Adapter}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_status, _From, Adapter) ->
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
    EnvOpts = [driver, target_database, database_addresses, database_ports, connection_pool_size, http_port, pools],
    EnvVals = lists:map(
        fun(Opt) ->
            case application:get_env(?APP, Opt) of
                {ok, Val} ->
                    {Opt, Val};
                undefined ->
                    {Opt, undefined}
            end
        end, EnvOpts),
    Reply = [
        {fmke_up, is_alive(fmke)},
        {connection_manager_up, is_alive(fmke_db_conn_manager)},
        {web_server_up, is_alive(cowboy_sup)},
        {pool_details, PoolDetails}
    ] ++ EnvVals,
    {reply, Reply, Adapter};

handle_call(Op, From, State) ->
    Worker = poolboy:checkout(handlers),
    gen_server:cast(Worker, {Op, From}),
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Create functions - no transactional context
%%-----------------------------------------------------------------------------

%% Adds a patient to the FMK system, needing only an ID, Name and Address.
%% A check is done to determine if a patient with the given ID already exists,
%% and if so the operation fails.
-spec create_patient(id(), string(), string()) -> ok | {error, reason()}.
create_patient(Id, Name, Address) ->
    gen_server:call(?MODULE, {create, patient, [Id, Name, Address]}).

%% Adds a pharmacy to the FMK-- system if the ID for the pharmacy has not yet been seen.
-spec create_pharmacy(id(), string(), string()) -> ok | {error, reason()}.
create_pharmacy(Id, Name, Address) ->
    gen_server:call(?MODULE, {create, pharmacy, [Id, Name, Address]}).

%% Adds a facility to the FMK-- system if the ID for the facility has not yet been seen.
-spec create_facility(id(), string(), string(), string()) -> ok | {error, reason()}.
create_facility(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {create, facility, [Id, Name, Address, Type]}).

%% Adds a staff member to the FMK-- system if the ID for the member has not yet been seen.
-spec create_staff(id(), string(), string(), string()) -> ok | {error, reason()}.
create_staff(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {create, staff, [Id, Name, Address, Speciality]}).

%% Creates a prescription that is associated with a pacient, prescriber (medicall staff),
%% pharmacy. The prescription also includes the prescription date and the list of drugs that should be administered.
-spec create_prescription(id(), id(), id(), id(), string(), [crdt()]) -> ok | {error, reason()}.
create_prescription(PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs) ->
    gen_server:call(?MODULE,
        {create, prescription, [PrescriptionId, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}
    ).

%%-----------------------------------------------------------------------------
%% Read functions - no transactional context
%%-----------------------------------------------------------------------------

%% Fetches a patient by ID.
-spec get_patient_by_id(id()) -> patient() | {error, reason()}.
get_patient_by_id(Id) ->
    gen_server:call(?MODULE, {read, patient, Id}).

%% Fetches a facility by id.
-spec get_facility_by_id(id()) -> facility() | {error, reason()}.
get_facility_by_id(Id) ->
    gen_server:call(?MODULE, {read, facility, Id}).

%% Fetches a pharmacy by ID.
-spec get_pharmacy_by_id(id()) -> pharmacy() | {error, reason()}.
get_pharmacy_by_id(Id) ->
    gen_server:call(?MODULE, {read, pharmacy, Id}).

%% Fetches a prescription by ID.
-spec get_prescription_by_id(id()) -> prescription() | {error, reason()}.
get_prescription_by_id(Id) ->
    gen_server:call(?MODULE, {read, prescription, Id}).

%% Fetches a list of prescriptions given a certain pharmacy ID.
-spec get_pharmacy_prescriptions(id()) -> list(prescription() | binary()) | {error, reason()}.
get_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {read, pharmacy, Id, prescriptions}).

-spec get_processed_pharmacy_prescriptions(id()) -> list(prescription() | binary()) | {error, reason()}.
get_processed_pharmacy_prescriptions(Id) ->
    gen_server:call(?MODULE, {read, pharmacy, Id, processed_prescriptions}).

%% Fetches prescription medication by ID.
-spec get_prescription_medication(id()) -> [binary()] | {error, reason()}.
get_prescription_medication(Id) ->
    gen_server:call(?MODULE, {read, prescription, Id, [drugs]}).

%% Fetches a staff member by ID.
-spec get_staff_by_id(id()) -> staff() | {error, reason()}.
get_staff_by_id(Id) ->
    gen_server:call(?MODULE, {read, staff, Id}).

%% Fetches a list of prescriptions given a certain staff member ID.
-spec get_staff_prescriptions(id()) -> list(prescription() | binary()) | {error, reason()}.
get_staff_prescriptions(Id) ->
    gen_server:call(?MODULE, {read, staff, Id, prescriptions}).

%%-----------------------------------------------------------------------------
%% Update functions - no transactional context
%%-----------------------------------------------------------------------------

%% Updates the personal details of a patient with a certain ID.
-spec update_patient_details(id(), string(), string()) -> ok | {error, reason()}.
update_patient_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update, patient, [Id, Name, Address]}).

%% Updates the details of a pharmacy with a certain ID.
-spec update_pharmacy_details(id(), string(), string()) -> ok | {error, reason()}.
update_pharmacy_details(Id, Name, Address) ->
    gen_server:call(?MODULE, {update, pharmacy, [Id, Name, Address]}).

%% Updates the details of a facility with a certain ID.
-spec update_facility_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_facility_details(Id, Name, Address, Type) ->
    gen_server:call(?MODULE, {update, facility, [Id, Name, Address, Type]}).

%% Updates the details of a staff member with a certain ID.
-spec update_staff_details(id(), string(), string(), string()) -> ok | {error, reason()}.
update_staff_details(Id, Name, Address, Speciality) ->
    gen_server:call(?MODULE, {update, staff, [Id, Name, Address, Speciality]}).

-spec update_prescription_medication(id(), atom(), [string()]) -> ok | {error, reason()}.
update_prescription_medication(Id, _Operation, Drugs) ->
    gen_server:call(?MODULE, {update, prescription, Id, {drugs, add, Drugs}}).

process_prescription(Id, Date) ->
    gen_server:call(?MODULE, {update, prescription, Id, {date_processed, Date}}).

%%-----------------------------------------------------------------------------
%% Helper functions
%%-----------------------------------------------------------------------------

is_alive(Proc) ->
    undefined =/= whereis(Proc).

get_status() ->
    gen_server:call(?MODULE, get_status).
