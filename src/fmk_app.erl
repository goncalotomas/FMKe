%%%-------------------------------------------------------------------
%% @doc fmk public API
%% @end
%%%-------------------------------------------------------------------

-module('fmk_app').

-behaviour(application).

%% Application callbacks
-export([init/2
		,start/2
        ,stop/1]).

%% Type definitions
-type string() :: [char(),...].
-type key() :: term().
-type reason() :: atom().

%% Defines
--define(MAXKEY,1000000).

%%====================================================================
%% API
%%====================================================================

-spec start() -> ok | {error, reason()}.
start() ->
	io:format("Starting FMK Server~n"),
	

%% @doc Initializes the application by setting a magic cookie.
init(Nodename, Cookie) ->
    case net_kernel:start([Nodename, longnames]) of
        {ok, _} ->
            erlang:set_cookie(node(), Cookie);
        {error, Reason} ->
           {error, Reason}
    end.


%%--------------------------------------------------------------------

%% @doc Creates a new patient
%% Input : Name and address of the patient
%% Output : {ok, UserId}
%%           UserId is the key that can be used later to access user's profile
%% TODO - check if the user already exists to deliver a different error message
-spec add_patient(Name::string(),Address::string()) -> {ok, } | {error, reason()}.
add_patient(name,address) ->
	UserKey = get_random_key(),
	Actor = self(),
	case call_fun(append, [UserKey, ?PROFILETYPE, {{assign, Name}, Actor}], Server) of
        {ok, _} ->
            {ok, UserKey};
        Result -> 
            lager:info(" ERROR : ~p", [Result]),
            {error, add_patient_failed}
    end.

-spec create_user(Name::string(), Server::term()) -> {ok, UserId::key()}.
create_user(Name, Server) ->
    RandKey = get_random_key(1000),
    UserKey = RandKey, %% TODO make key unique
    Actor = self(),
    case call_fun(append, [UserKey, ?PROFILETYPE, {{assign, Name}, Actor}], Server) of
        {ok, _} ->
            {ok, UserKey};
        Result -> 
            lager:info(" ERROR : ~p", [Result]),
            {error, create_user_failed}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

call_fun(Fun, Param, Server) ->
     rpc:call(Server, antidote, Fun, Param).

get_random_key() ->
	rand:uniform(MAXKEY).