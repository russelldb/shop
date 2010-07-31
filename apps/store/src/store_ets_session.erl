%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Uses ets to store session information
%%% @end
%%% Created : 25 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_ets_session).

-behaviour(store_session_behaviour).

-define(TAB, session).

%% API
-export([get/2, put/3, clear/1, new/0, init/1]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc new starts a new session 
%% @spec init(Env::proplist()) -> ok
%% @end
%%--------------------------------------------------------------------
init(Env) ->
    ?TAB = ets:new(?TAB, [set, private, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @doc new starts a new session 
%% @spec new() -> session_id()
%% @end
%%--------------------------------------------------------------------
new() ->
    SessionId = crypto:sha(term_to_binary({store_util:now(), erlang:make_ref()})),
    TS = store_util:now(),
    true = ets:insert(?TAB, {SessionId, [{ts, TS}]}),
    SessionId.

%%--------------------------------------------------------------------
%% @doc clears the session completely
%% @spec clear(SessionId) -> ok
%% @end
%%--------------------------------------------------------------------
clear(SessionId) ->
    true = ets:delete(?TAB, SessionId),
    ok.

%%--------------------------------------------------------------------
%% @doc adds value Value under Key to SessionId session
%% @spec put(SessionId, Key, Value) -> ok
%% @end
%%--------------------------------------------------------------------
put(SessionId, Key, Value) ->
    case ets:lookup(?TAB, SessionId) of
	[] ->
	    exit(no_session);
	[{SessionId, Session}] ->
	    Session2 = [{Key, Value}|Session],
	    true = ets:insert(?TAB, {SessionId, Session2})
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc gets value for Key of SessionId session
%% @spec get(SessionId, Key) -> Value | undefined
%% @end
%%--------------------------------------------------------------------
get(SessionId, Key) ->
    case ets:lookup(?TAB, SessionId) of
	[] ->
	    exit(no_session);
	[{SessionId, Session}] ->
	    proplists:get_value(Key, Session)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
