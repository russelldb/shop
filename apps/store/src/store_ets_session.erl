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
-export([get_value/2, set_value/3, clear/1, new/0, init/1]).

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
    Key = crypto:sha(term_to_binary({store_util:now(), erlang:make_ref()})),
    TS = store_util:now(),
    true = ets:insert(?TAB, {Key, [{ts, TS}]}),
    Key.

%%--------------------------------------------------------------------
%% @doc clears the session completely
%% @spec clear(SessionId) -> ok
%% @end
%%--------------------------------------------------------------------
clear(Key) ->
    Key = crypto:sha(term_to_binary({store_util:now(), erlang:make_ref()})),
    TS = store_util:now(),
    true = ets:delete(?TAB, Key),
    ok.

%%--------------------------------------------------------------------
%% @doc adds value Value under Key to SessionId session
%% @spec set_value(SessionId, Key, Value) -> ok
%% @end
%%--------------------------------------------------------------------
set_value(SessionId, Key, Value) ->
    exit(not_implemeted).

%%--------------------------------------------------------------------
%% @doc gets value for Key of SessionId session
%% @spec get_value(SessionId, Key) -> Value | undefined
%% @end
%%--------------------------------------------------------------------
get_value(Key, Value) ->
    exit(not_implemeted).

%%%===================================================================
%%% Internal functions
%%%===================================================================
