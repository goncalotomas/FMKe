-module(gen_fmke_adapter).

-include ("fmke.hrl").

-type context() :: term().

-callback start(Driver::atom()) -> {ok, pid()} | {error, term()}.
-callback stop() -> {ok, term()} | {error, term()}.


%%-----------------------------------------------------------------------------
%% Create Operations
%%-----------------------------------------------------------------------------


-callback create_patient(Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_pharmacy(Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_facility(Id::id(), Name::string(), Address::string(), Type::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_staff(Id::id(), Name::string(), Address::string(), Speciality::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_prescription(PrescriptionId::id(), PatientId::id(), PrescriberId::id(),
      PharmacyId::id(), DatePrescribed::string(), Drugs::list(crdt())) ->
  {ok | {error, Reason::term()}, Context::context()}.


%%-----------------------------------------------------------------------------
%% Get Operations
%%-----------------------------------------------------------------------------


-callback get_facility_by_id(Id::id()) ->
  {{ok, Object::facility()} | {error, Reason::term()}, Context::context()}.

-callback get_patient_by_id(Id::id()) ->
  {{ok, Object::patient()} | {error, Reason::term()}, Context::context()}.

-callback get_pharmacy_by_id(Id::id()) ->
  {{ok, Object::pharmacy()} | {error, Reason::term()}, Context::context()}.

-callback get_prescription_by_id(Id::id()) ->
  {{ok, Object::prescription()} | {error, Reason::term()}, Context::context()}.

-callback get_staff_by_id(Id::id()) ->
  {{ok, Object::staff()} | {error, Reason::term()}, Context::context()}.

-callback get_processed_pharmacy_prescriptions(Id::id()) ->
  {{ok, ListObjects::list(prescription() | binary())} | {error, Reason::term()}, Context::context()}.

-callback get_pharmacy_prescriptions(Id::id()) ->
  {{ok, ListObjects::list(prescription() | binary())} | {error, Reason::term()}, Context::context()}.

-callback get_staff_prescriptions(Id::id()) ->
  {{ok, ListObjects::list(prescription() | binary())} | {error, Reason::term()}, Context::context()}.


%%-----------------------------------------------------------------------------
%% Update Operations
%%-----------------------------------------------------------------------------


-callback process_prescription(Id::id(), DateProcessed::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_patient_details(Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_pharmacy_details(Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_facility_details(Id::id(), Name::string(), Address::string(), Type::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_staff_details(Id::id(), Name::string(), Address::string(), Speciality::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_prescription_medication(Id::id(), Operation::atom(), Drugs::list(crdt())) ->
  {ok | {error, Reason::term()}, Context::context()}.
