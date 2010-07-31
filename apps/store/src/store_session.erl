%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Generice session server, delegates to a store_session_behaviour module
%%% @end
%%% Created : 31 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_session).

-behaviour(gen_server).

%% API
-export([start_link/1, get/2, put/3, clear/1, new/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Env) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Env, []).

%%--------------------------------------------------------------------
%% @doc stops the server
%% @spec stop() -> 
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc calls the callback module's new/0 function
%% @spec new() -> session_id()
%% @end
%%--------------------------------------------------------------------
new() ->
    gen_server:call(?SERVER, new).

%%--------------------------------------------------------------------
%% @doc clears the session for sessionId
%% @spec clear(session_id()) -> ok
%% @end
%%--------------------------------------------------------------------
clear(SessionId) ->
    gen_server:call(?SERVER, {clear, SessionId}).

%%--------------------------------------------------------------------
%% @doc gets Key from SessionId session
%% @spec get(SessionId, Key) -> term() | undefined
%% @end
%%--------------------------------------------------------------------
get(SessionId, Key) ->
    gen_server:call(?SERVER, {get, SessionId, Key}).

%%--------------------------------------------------------------------
%% @doc puts Value for Key in SessionId session
%% @spec put(SessionId, Key, Value) -> term() | undefined
%% @end
%%--------------------------------------------------------------------
put(SessionId, Key, Value) ->
    gen_server:call(?SERVER, {put, SessionId, Key, Value}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Env) ->
    Session = proplists:get_value(session_module, Env),
    SessionEnv = proplists:get_value(session_env, Env),
    true = is_valid(Session:module_info(exports)),
    ok = Session:init(SessionEnv),
    {ok, {session, Session}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(new, _From, {session, Session}=State) ->
    SessionId = Session:new(),
    {reply, SessionId, State};
handle_call({clear, SessionId}, _From, {session, Session}=State) ->
    ok = Session:clear(SessionId),
    {reply, ok, State};
handle_call({get, SessionId, Key}, _From, {session, Session}=State) ->
    Val = Session:get(SessionId, Key),
    {reply, Val, State};
handle_call({put, SessionId, Key, Val}, _From, {session, Session}=State) ->
    ok = Session:put(SessionId, Key, Val),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Checks that the passed module exports the required functions
%% @spec is_valid([export()]) -> true | false
%% @end
%%--------------------------------------------------------------------
is_valid([]) ->
    exit(bad_mod);
is_valid(Exports) ->
    store_util:is_valid_behaviour(store_session_behaviour, Exports).
