%% This module documents the callbacks that an FMKE driver for a Key-Value Store must implement.
%%
%% A brief explanation about FMKe adapters and drivers:
%%
%% An adapter is an Erlang module that implements the complete FMKe callback set, but that is able to make assumptions
%% about the data model, connection pool or any other configurable parameter. Adapters don't communicate directly with
%% client libraries for databases, but instead do it through drivers.
%%
%% A driver is a simple wrapper over a database's client library that exposes a common interface to all databases.
%% When implementing a driver it is necessary to implement additional logic required to maintain correct application
%% state, such as keeping track of previously read values within a transaction. Failure to implement the additional
%% logic may result in anomalies which will should be documented. The performance to correctness trade-off is common
%% in these types of storage systems, and the documentation of the presented anomalies along with performance values
%% is paramount.
%%
%% Since adapters do not make assumptions about the capabilities of the database, the drivers will need to export
%% callbacks related to transactions (e.g. start_transaction/1, commit_transaction/1). These functions are expected to
%% return opaque state that is passed in to further operations, meaning that you can add contextual information by
%% returning {ok, term()}, or just {ok, []} if there is no need for context in order to perform the operations.
%%
%% Drivers might also need to set up additional components and state for themselves, which is why the start/0
%% hooks exist. In these functions you may open a pool of connections to the database (but for that purpose you can
%% already use the fmke_db_conn_manager module), create an ETS table for caching results, etc.
%% Conversely, the stop/0 function will allow you to terminate gracefully and perform any teardown you feel necessary.
%%
%% The get and put functions that drivers need to implement contain extra parameters in order to give operation context
%% to the drivers. This is to avoid all possible overhead from using a generic approach (for instance, having to derive
%% which entity is being obtained from the key passed in get/3, if you used separate buckets in the database for each
%% one) as well as trying to provide optimal compatibility with other storage systems that may require extra context to
%% perform operations.
-module(gen_fmke_kv_driver).

-include("fmke.hrl").

-type value() :: term().
-type context() :: term().
-type options() :: list({atom(), term()}).
-type txn_result() :: ok | {error, term()}.
% -type data_model() :: nested | non_nested.

%% ---------------------------------------------------------------------------------------------------------------------
%% Setup and teardown callbacks
%% ---------------------------------------------------------------------------------------------------------------------

%% Startup hook that provides information about whether the current benchmark execution is using a normalized or nested
%% data layout. It is the driver's responsability to implement the logic for both data layouts, although the code should
%% not change significantly between them. (See example below)
%% A typical way of storing objects in CRDT databases would be to nest every field inside a top level record, which has
%% so far proved to have worse performance, since each CRDT state size will increase over multiple operations.
%% Furthermore, application records such as patients will need to store their associated prescriptions, which are
%% separate entities/records, further increasing CRDT state size. One way to bypass this is to store a reference to the
%% prescription key inside the patient, and we consider this to be a "normalized" (non-nested) data layout.
%% Implementing a driver may be done for a single data layout, ignoring the other completely. When test executions are
%% run, only valid data model implementations are considered for performance results.
% -callback start(DataModel::data_model()) -> {error, term()} | {ok, pid()}.

%% Teardown hook, called when the application is stopped.
% -callback stop() -> ok.

%% ---------------------------------------------------------------------------------------------------------------------
%% Transactional support callbacks
%% ---------------------------------------------------------------------------------------------------------------------

%% Starts a transaction while providing some context of the type of operations that are going to be performed.
%% A proplist of options (Options) is passed in, with the following values considered valid options:
%% {entity, Entity :: entity()}  ->
%%      The following operations that are going to be performed in this transaction only concern one entity, Entity.
%%
%% Returns any erlang term containing the state that is required by the driver to execute each operation related with a
%% transaction. It is common for the returned state to include a Pid that contains a connection to the database and
%% possibly identifier(s) for the transaction. Any erlang term is considered valid and will be passed in to subsequent
%% operations related to the same transaction.
-callback start_transaction(Options::options()) -> {ok, OperationContext::context()}.

%% Signals the end of a transaction, passing in the current operation context as well as a list of options that
%% currently serves no purpose. A typical implementation of commit_transaction includes calling commit_transaction on
%% client library (if supported) and returning the Pid to the connection pool.
%%
%% See some implementations in the fmke_db_adapter_driver_antidote.erl and fmke_db_adapter_driver_riak.erl modules.
-callback commit_transaction(OperationContext::context(), Options::options()) -> Result::txn_result().

%% ---------------------------------------------------------------------------------------------------------------------
%% Key value callbacks
%% ---------------------------------------------------------------------------------------------------------------------

%% get/2 - Fetches a list of keys from the database.
%% To provide context, some information about the entity being retrieved is included, and additionally the operation
%% context is also passed in from a previous get/3, put/4, or start_transaction/1.
%%
%% Returns a triple with {ok, GetResult, NextOperationContext} if the operation was executed successfully or
%% {error, Reason, NextOperationContext} otherwise.
-callback get(list({Key::key(), Type::entity()}), OperationContext::context()) ->
    {list(app_record() | {error, term()}), context()}.

%% put/3 - Adds a list of key-value entries to the database.
%% To provide context, some information about the each entry being added is included, and additionally the operation
%% context is also passed in from a previous get/3, put/4, or start_transaction/1.
%%
%% Returns a pair with {list(put_results()), NextOperationContext} if the operation was executed successfully or
%% {error, Reason, NextOperationContext} otherwise.
%%
%% The Key to be written is passed in binary string format, as that is currently universally supported by all libraries.
%% The Value to be written is a value that the driver is able to recognize, which means that the adapters need to pass
%% valid values that the drivers are able to recognize and convert to a proper internal representation.
%%
%% A more in-depth explanation of what key() and value() should be:
%% 1. key() is a binary string representation of the key that is going to be written.
%% 2. value() is either an application record (in which case it is considered that every field is supposed to stay under
%% the same key, )
-callback put(list({Key::key(), Type::entity(), Value::value()}), OperationContext::context()) ->
    {list(ok | {error, term()}), context()}.
