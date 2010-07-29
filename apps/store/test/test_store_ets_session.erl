%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% tests the ets impl of session behaviour
%%% @end
%%% Created : 26 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_ets_session).

-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% @doc tests init creates the table
%% @spec
%% @end
%%--------------------------------------------------------------------
init_test_() ->
    {setup, fun() ->
		    store_ets_session:init([]) end,
     fun(_X) -> teardown(session) end,
     [?_assert(contains_session_table(ets:all()))]}.

%%--------------------------------------------------------------------
%% @doc tests new creates new sessions in the table
%% @spec
%% @end
%%--------------------------------------------------------------------
new_test_() ->
    {setup, fun() ->
		    store_ets_session:init([]) end,
     fun(_X) -> teardown(session) end,
     fun(X) -> generate_new(X) end}.

%%--------------------------------------------------------------------
%% @doc generates the tests for the session:new
%% @spec
%% @end
%%--------------------------------------------------------------------
generate_new(_X) ->
    Key = store_ets_session:new(),
    Key2 = store_ets_session:new(),
    [{Key, Session1}] = ets:lookup(session, Key),
    [{Key2, Session2}] = ets:lookup(session, Key2),
    TS1 = proplists:get_value(ts, Session1),
    TS2 = proplists:get_value(ts, Session2),
    [?_assertNot(Key =:= Key2), ?_assert(TS1 < store_util:now()), ?_assert(TS2 < store_util:now())].

%%--------------------------------------------------------------------
%% @doc tests clear removes the session
%% @spec
%% @end
%%--------------------------------------------------------------------
clear_test_() ->
    {setup, fun() ->
		    store_ets_session:init([])  end,
     fun(_X) -> teardown(session) end,
     fun(X) -> generate_clear(X) end}.

%%--------------------------------------------------------------------
%% @doc generates the tests for the session:clear
%% @spec
%% @end
%%--------------------------------------------------------------------
generate_clear(_X) ->
    Key = store_ets_session:new(),
    [{Key, Session}] = ets:lookup(session, Key),
    OK = store_ets_session:clear(Key),
    StillSession =  ets:lookup(session, Key),
    [?_assertMatch(ok, OK), ?_assertMatch([], StillSession), ?_assert(length(Session) > 0)].

%%--------------------------------------------------------------------
%% @doc tests set adds value to a session
%% @spec
%% @end
%%--------------------------------------------------------------------
set_test_() ->
    {setup, fun() ->
		    store_ets_session:init([])  end,
     fun(_X) -> teardown(session) end,
     fun(X) -> generate_set(X) end}.

%%--------------------------------------------------------------------
%% @doc generates the tests for the session:clear
%% @spec
%% @end
%%--------------------------------------------------------------------
generate_set(_X) ->
    Key = store_ets_session:new(),
    ok = store_ets_session:put(Key, test_key, test_value),
    [{Key, Session}] = ets:lookup(session, Key),
    [?_assertMatch(test_value, proplists:get_value(test_key, Session))].

%%%===================================================================
%%% Internal functions
%%%===================================================================
teardown(_X) ->
    delete([session]).

delete([]) ->
    ok;
delete([H|T]) ->
    ets:delete(H),
    delete(T).

contains_session_table([]) ->
    false;
contains_session_table([session|_]) ->
    true;
contains_session_table([_H|T]) ->
    contains_session_table(T).

