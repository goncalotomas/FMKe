%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(fmke_gen_driver).

-type id() :: non_neg_integer().
-type context() :: term().
-type crdt() :: term().

-callback start_link(term()) -> {ok, pid()} | {error, term()}.
-callback stop(pid()) -> ok | {error, term()}.

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

% -callback create_event(EventId::id(), TreatmentId::id(), StaffMemberId::id(), Timestamp::string(),
%       Description::string()) ->
%   {ok | {error, Reason::term()}, Context::context()}.
%
% -callback create_treatment(TreatmentId::id(), PatientId::id(), StaffId::id(), FacilityId::id(),
%       DateStarted::string()) ->
%   {ok | {error, Reason::term()}, Context::context()}.

%%-----------------------------------------------------------------------------
%% Get Operations
%%-----------------------------------------------------------------------------

% -callback get_event_by_id(Id::id()) ->
%   {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_facility_by_id(Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_patient_by_id(Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_pharmacy_by_id(Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_prescription_by_id(Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_staff_by_id(Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

% -callback get_treatment_by_id(Id::id()) ->
%   {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

% -callback get_facility_treatments(Id::id()) ->
%   {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_processed_pharmacy_prescriptions(Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_pharmacy_prescriptions(Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_staff_prescriptions(Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

% -callback get_staff_treatments(Id::id()) ->
%   {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

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
