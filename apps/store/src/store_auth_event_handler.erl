%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Handles authentication events (like success, failure) 
%%% @TODO store these events in mnesia or a log file
%%% @TODO figure out events and the handling strategy
%%% @end
%%% Created : 18 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_auth_event_handler).

-behaviour(gen_event).
%% API
-export([register/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(FAILED_LOGIN_LIMIT, 3).

-include("store.hrl").


%%%===================================================================
%%% API
%%%===================================================================
register() ->
    store_event_manager:add_handler(?MODULE).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, ?FAILED_LOGIN_LIMIT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({fail, unknown_user, Username}, State) ->
    error_logger:info_msg("Failed login from unknown user ~p~n", [Username]),
    {ok, State};
handle_event({fail, bad_paasword, User}, State) ->
    %% updated fail count, last failed date and check to see if we should lock and write to db
    error_logger:info_msg("Failed login from user user ~p. Bad password~n",  [User#user.username]),
    {ok, State};
handle_event({fail,locked, User}, State) ->
    error_logger:info_msg("Failed login from user user ~p.Locked account~n",  [User#user.username]),
    {ok, State};
handle_event({fail, admin_locked, User}, State) ->
    error_logger:info_msg("Failed login from user user ~p. Admin locked account~n",  [User#user.username]),
    {ok, State};
handle_event({success, User}, State) ->
    %% update last_login date, reset failed couunt and write to db
    error_logger:info_msg("Login success ~p~n", [User#user.username]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
