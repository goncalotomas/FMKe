-module(fmke_db_conn_manager).

-include("fmke.hrl").

-behaviour(gen_server).

-define (SERVER, ?MODULE).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3]).

%% conn_manager API
-export([checkout/0, checkin/1]).

-record(state, {
    queue :: queue:queue(),
    pid_owners :: map()
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, Pools} = application:get_env(?APP, pools),
    {ok, #state{queue = queue:from_list(Pools), pid_owners = #{}}}.

checkout() ->
    gen_server:call(?MODULE, checkout).

checkin(Pid) ->
    gen_server:call(?MODULE, {checkin, Pid}).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(checkout, _From, State) ->
    #state{pid_owners = PidOwners,
           queue = Queue} = State,
    Pool = queue:head(Queue),
    Pid = poolboy:checkout(Pool),
    MapState = maps:put(Pid, Pool, PidOwners),
    QueueState = queue:snoc(queue:tail(Queue), Pool),
    {reply, Pid, #state{queue = QueueState, pid_owners = MapState}};

handle_call({checkin, Pid}, _From, State) ->
    #state{pid_owners = PidOwners,
           queue = Queue} = State,
    Owner = maps:get(Pid, PidOwners, queue:head(Queue)),
    MapState = maps:remove(Pid, PidOwners),
    Result = poolboy:checkin(Owner, Pid),
    {reply, Result, State#state{pid_owners = MapState}};

handle_call(_Msg, _From, State) ->
    {noreply, State}.
