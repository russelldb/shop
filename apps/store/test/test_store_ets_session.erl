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
     fun(X) -> teardown(session) end,
     [?_assert(contains(ets:all()))]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
teardown(X) ->
    delete([session]).

delete([]) ->
    ok;
delete([H|T]) ->
    ets:delete(H),
    delete(T).

contains([]) ->
    false;
contains([session|_]) ->
    true;
contains([H|T]) ->
    contains(T).

