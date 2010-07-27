
-module(store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(DYNA_CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10}, [?DYNA_CHILD(store_event_manager, worker, []), ?CHILD(store_db, worker, []), ?CHILD(store_authenticator, worker, [])]} }.

