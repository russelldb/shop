%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Eunit test for store_db gen_server, checks that store_db calls back 
%%% On to configured module
%%% @end
%%% Created : 12 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_db).

-export([add_item/1, add_item/2, fetch_all_items/0]).

-include_lib("eunit/include/eunit.hrl").

-include ("store.hrl").

setup() ->
   ok.

teardown(_) ->
    store_db:stop().

init_test_() ->
     {setup, fun() -> setup() end, fun(X) -> teardown(X) end,
     [ ?_assertMatch({ok, Pid}, store_db:start_link([{db_module, test_store_db}])) ]}.

add_item(_Item) ->
    ok.

add_item(_Item, _Options) ->
    ok.

fetch_all_items() ->
    ok.

