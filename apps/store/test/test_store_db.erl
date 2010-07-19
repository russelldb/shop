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

teardown() ->
    store_db:stop(), 
    timer:sleep(500).

init_test_() ->
    {setup, fun() -> no_op end, fun(_) -> teardown() end,
     [ ?_assertMatch({ok, _}, store_db:start_link([{db_module, test_store_db}])) ]}.

add_item_test_() ->
    {setup, fun() ->
		    {ok, _} = store_db:start_link([{db_module, test_store_db}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch( add_item, store_db:add_item(#item{}))]}.

add_item_and_opts_test_() ->
    {setup, fun() ->
		    {ok, _} = store_db:start_link([{db_module, test_store_db}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch(add_item_and_opts, store_db:add_item(#item{}, [#item_option{}]))]}.

fetch_all_items_test_() ->
    {setup, fun() ->
		    {ok, _} = store_db:start_link([{db_module, test_store_db}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch( [fetched, all, items], store_db:fetch_all_items())]}.

add_item(_Item) ->
    add_item.

add_item(_Item, _Options) ->
    add_item_and_opts.

fetch_all_items() ->
    [fetched, all, items].

