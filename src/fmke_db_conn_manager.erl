-module(fmke_db_conn_manager).

-include("fmke.hrl").

-behaviour(gen_server).

-define (SERVER, ?MODULE).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2]).

%% conn_manager API
-export([checkout/0, checkin/1]).

-record(state, {
    queue :: queue:queue(),
    pid_owners :: map()
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
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
    {{value, Pool}, Q1} = queue:out(Queue),
    Pid = poolboy:checkout(Pool),
    true = link(Pid),
    {reply, Pid, #state{queue = queue:in(Pool, Q1), pid_owners = maps:put(Pid, Pool, PidOwners)}};

handle_call({checkin, Pid}, _From, State) ->
    #state{pid_owners = PidOwners} = State,
    case maps:get(Pid, PidOwners, no_such_pid) of
        no_such_pid ->
            lager:error("Cannot find owner of pid ~p~n", [Pid]),
            {reply, no_such_pid, State};
        Owner ->
            MapState = maps:remove(Pid, PidOwners),
            Result = poolboy:checkin(Owner, Pid),
            {reply, Result, State#state{pid_owners = MapState}}
    end.

terminate(Reason, State) ->
    #state{pid_owners = PidOwners,
           queue = Queue} = State,
    lager:critical("fmke db connection manager going down, reported reason: ~p~n", [Reason]),
    lager:error("fmke db connection manager crashing with next queue '~p' and ~p pids checked out.~n",
                [Queue, maps:size(PidOwners)]),
    ok.

handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{pid_owners = PidOwners} = State,
    MapState = maps:remove(Pid, PidOwners),
    {noreply, State#state{pid_owners = MapState}}.
