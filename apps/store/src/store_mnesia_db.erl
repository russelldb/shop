%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% backend for store that uses mnesia to persist item data
%%% @end
%%% Created : 10 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_mnesia_db).

-behaviour(store_db_behaviour).

-export([object_counter/1, add_item/1, add_item/2, fetch_all_items/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("store.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc ***MUST*** be run as part of a transaction. 
%% increments and returns a row counter fof given table
%% @spec object_counter(table()) -> int().
%% @end
%%--------------------------------------------------------------------
object_counter(Name) -> %% Called from within a transaction 
    [OldRecord] = mnesia:read(counter, Name, write), 
    Count = OldRecord#counter.count + 1, 
    NewRecord = OldRecord#counter{ count = Count }, 
    mnesia:write(NewRecord), 
    Count.

%%--------------------------------------------------------------------
%% @doc adds the given item to the database (convenience for add_item/2)
%% @spec add_item(item()) -> ok.
%% @end
%%--------------------------------------------------------------------
add_item(Item) when is_record(Item, item) ->
   add_item(Item, []).

%%--------------------------------------------------------------------
%% @doc adds the given item, and its options to the database
%% @spec add_item(item(), [item_option()]) -> ok.
%% @end
%%--------------------------------------------------------------------
add_item(Item, ItemOptions) when is_record(Item, item), is_list(ItemOptions) ->
    F = fun() -> 
		write_item(Item, ItemOptions)
	end,
    case  mnesia:transaction (F) of
	{atomic, Val} -> Val;
	{aborted, Reason} -> erlang:error(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc get all the items for the store
%% @spec fetch_all_items() -> [{item, item{}, [item_option()]}]
%% @end
%%--------------------------------------------------------------------
fetch_all_items() ->
    F = fun() ->
		Items = qlc:e(qlc:q([X || X <- mnesia:table(item)])),
		fetch_options_for_items(Items, [])
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.
    

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc builds results tuple for items, **MUST** run in transaction
%% @spec fetch_options_for_items([item()], acc()) -> [{item, item(), [item_option()]}]
%% @end
%%--------------------------------------------------------------------
fetch_options_for_items([], Acc) ->
    lists:reverse(Acc);
fetch_options_for_items([Item|Rest], Acc) ->
    Options = qlc:e(qlc:q([X || X <- mnesia:table(item_option), X#item_option.item_id =:= Item#item.id ])),
    fetch_options_for_items(Rest, [{item, Item, Options}|Acc]).

%%--------------------------------------------------------------------
%% @doc *MUST* be run in a transaction, writes the list of options for
%% The give item id
%% @spec write_item_options([item_option()], id()) -> ok
%% @end
%%--------------------------------------------------------------------
write_item_options([], _) ->
    ok;
write_item_options([Opt|Rest], ItemId) ->
    Id = object_counter(item_option),
    Opt1 = Opt#item_option{item_id=ItemId, id=Id},
    mnesia:write(Opt1),
    write_item_options(Rest, ItemId).

%%--------------------------------------------------------------------
%% @doc get_by_name internal function to get all items with name Name
%% @spec get_by_name(string()) -> [item()] | []
%% @end
%%--------------------------------------------------------------------
get_by_name(Name) when is_list(Name) ->
   qlc:e(qlc:q([X || X <- mnesia:table(item), X#item.name =:= Name ])). 

%%--------------------------------------------------------------------
%% @doc **MUST** be run in a transaction. Checks for dups and writes item (and options)
%% @spec
%% @end
%%--------------------------------------------------------------------
write_item(Item, ItemOptions) ->
    case get_by_name(Item#item.name) of 
	[] -> 
	    Id = object_counter(item),
	    Item1 = Item#item{id=Id},
	    mnesia:write(Item1),		
	    write_item_options( ItemOptions, Id );
       	L when is_list(L), length(L) > 0 -> mnesia:abort(duplicate)
    end.
