%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Tests that the id generation works
%%% @end
%%% Created :  3 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_mnesia_db).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include ("store.hrl").


%%% Initialize database and tables. 
setup() ->
    store_test_db:setup(),
    %% set up the default values for the default tables.
    store_test_db:write(#counter{type=item, count=0}),
    store_test_db:write(#counter{type=item_option, count=0}),
    ok.

teardown(_X) ->
    store_test_db:teardown().

%%% Test that id generation works
id_test_() ->
    {setup, fun() -> setup() end, fun(X) -> teardown(X) end,
     [?_assertEqual(1, do_counter(item)),
      ?_assertEqual(2, do_counter(item)),
      ?_assertEqual(1, do_counter(item_option)),
      ?_assertEqual(2, stop_start_run(fun() -> do_counter(item_option) end))]}.


%%% Test that saving an item gets it stored (with an id)
add_item_test_() ->
    {setup,  fun() -> setup() end, fun(X) -> teardown(X) end, 
     fun generate_add_item_tests/1}.

generate_add_item_tests(_X) ->
    Item = #item{name="Haggis", description="A bag of offal", price=1299},
    ok = store_mnesia_db:add_item(Item),
    Items = q(qlc:q([X || X <- mnesia:table(item)])),
    Item1 = hd(Items),
    [?_assert(length(Items) =:= 1), ?_assert(is_integer(Item1#item.id) =:= true)].

%%% Test that saving an two items gets them unique ids
add_items_test_() ->
    {setup,  fun() -> setup() end, fun(X) -> teardown(X) end, 
     fun generate_add_items_tests/1}.

generate_add_items_tests(_X) ->
    Item = #item{name="Haggis", description="A bag of offal", price=1299},
    Item2 = #item{name="Breeks", description="Like long shorts, or short longs", price=1000},
    ok = store_mnesia_db:add_item(Item),
    ok = store_mnesia_db:add_item(Item2),
    Items = q(qlc:q([X || X <- mnesia:table(item)])),
    RItem = hd(Items),
    RItem2 = hd(tl(Items)),
    [?_assert(length(Items) =:= 2), ?_assert(is_integer(RItem#item.id) =:= true), ?_assert(is_integer(RItem2#item.id) =:= true),
     ?_assert(RItem#item.id =/= RItem2#item.id)].


%%% Test that saving two duplicate items gets an error
add_duplicate_items_test_() ->
    {setup,  fun() -> setup() end, fun(X) -> teardown(X) end, 
     fun generate_add_duplicate_items_tests/1}.

generate_add_duplicate_items_tests(_X) ->
    Item = #item{name="Haggis", description="A bag of offal", price=1299},
    Item2 = #item{name="Haggis", description="A bag of offal", price=1299},
    store_mnesia_db:add_item(Item),
    [?_assertError(duplicate, store_mnesia_db:add_item(Item2))].


%%% Test that saving an item *with* item options gets item stored (with an id) and options get item id
add_item_option_test_() ->
    {setup,  fun() -> setup() end, fun(X) -> teardown(X) end, 
     fun generate_add_item_options_tests/1}.

generate_add_item_options_tests(_X) ->
    Item = #item{name="Haggis", description="A bag of offal", price=1299},
    ItemColourOption = #item_option{name="colour", value="blue", inventory=99},
    ItemSizeOption =  #item_option{name="size", value="large"},
    store_mnesia_db:add_item(Item, [ItemColourOption, ItemSizeOption]),
    Items = q(qlc:q([X || X <- mnesia:table(item)])),
    Item1 = hd(Items),
    ItemId = Item1#item.id,
    ItemOptions = q(qlc:q([X || X <- mnesia:table(item_option), X#item_option.item_id =:= ItemId])),
    ItemOption1 = hd(ItemOptions),
    ItemOption2 = hd(tl(ItemOptions)),
    [ ?_assert(length(ItemOptions) =:= 2),
      ?_assertMatch(ItemId, ItemOption1#item_option.item_id),
      ?_assertMatch(ItemId, ItemOption2#item_option.item_id)].

%%% Tests that items and options in the db get retrieved
get_all_items_test_() ->
    {setup,  fun() -> setup() end, fun(X) -> teardown(X) end, 
     fun generate_get_all_items_tests/1}.

generate_get_all_items_tests(_X) ->
    Item = #item{id=1, name="Haggis", description="A bag of offal", price=1299},
    ItemColourOption = #item_option{item_id=1, id=1, name="colour", value="blue", inventory=99},
    ItemSizeOption =  #item_option{item_id=1, id=2, name="size", value="large"},

    Item2= #item{id=2, name="Breeks", description="Long shorts/short longs", price=10000},
    Item2ColourOption = #item_option{item_id=2, id=3, name="colour", value="blue", inventory=99},
    Item2SizeOption =  #item_option{item_id=2, id=4, name="size", value="large"},
   
    do(fun() ->
	       mnesia:write(Item),
	       mnesia:write(ItemColourOption),
	       mnesia:write(ItemSizeOption),
	       mnesia:write(Item2),
	       mnesia:write(Item2ColourOption),
	       mnesia:write(Item2SizeOption)
       end),

    Items = store_mnesia_db:fetch_all_items(),

    [?_assert(length(Items) =:= 2), 
     ?_assertMatch([{item, _, _}|_], Items),
     ?_assert(contains(Items, {Item, [ItemColourOption, ItemSizeOption]})), 
     ?_assert(contains(Items, {Item2, [Item2ColourOption, Item2SizeOption]}))].




%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
contains([], _) ->
    false;
contains([{item, Item, O}|_], {Item, Opts}) ->
    contains_opts(O, Opts);
contains([_|T], ItemAndOpts) ->
    contains(T, ItemAndOpts).

contains_opts(Opts, MyOpts) when length(Opts) =/= length(MyOpts) ->
    false;
contains_opts(Opts, MyOpts) ->
    is_empty([ X || X <- MyOpts,
	    lists:member(X, Opts) =:= false]).

is_empty([])->
    true;
is_empty(_) ->
    false.
    

do(F) ->
    {atomic, Val} =  mnesia:transaction (F),
    Val.

do_counter(Table) ->
    do(fun() -> store_mnesia_db:object_counter(Table) end).

stop_start_run(F) ->
    mnesia:stop(),
    mnesia:start(),
    timer:sleep(1000), %%This gives mnesia time to get up again
    F().


q(Q) ->
    F = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

