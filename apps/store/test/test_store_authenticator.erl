%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Eunit test for store_authenticator gen_server, checks that store_authenticator calls back 
%%% On to configured module
%%% @end
%%% Created : 12 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_authenticator).

-behaviour(store_authenticator_behaviour).

-export([authenticate/2]).

-include_lib("eunit/include/eunit.hrl").

-include ("store.hrl").

teardown() ->
    store_authenticator:stop(), 
    timer:sleep(500).

init_test_() ->
    {setup, fun() -> no_op end, fun(_) -> teardown() end,
     [ ?_assertMatch({ok, _}, store_authenticator:start_link([{auth_module, ?MODULE}])) ]}.

authenticate_test_() ->
    {setup, fun() ->
		    {ok, _} = store_authenticator:start_link([{auth_module, ?MODULE}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch( {ok, you_totally_called_me}, store_authenticator:authenticate("bob", "password"))]}.


authenticate(_Username, _Password) ->
    {ok, you_totally_called_me}.

