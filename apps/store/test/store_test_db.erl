%%% File    : store_test_db.erl
%%% Author  : Russell Brown <russell@ossme.net>
%%% Description : Common DB test operations
%%% Created : Mon  3 Aug 2009 15:10:39 BST by Russell Brown <russell@ossme.net>
-module (store_test_db).

-include ("store.hrl").

-export ([setup/0, teardown/0, write/1, create_tables/0, clear_tables/0]).

-define(TABLES, [item, item_option, counter, user]).

%%% Initialize database
setup () ->
    mnesia:create_schema ([node()]),
    mnesia:start(),
    create_tables(),
    clear_tables(),
    mnesia:wait_for_tables([?TABLES], 1000).

create_table(item) ->
    mnesia:create_table (item, [{attributes,  record_info(fields, item)}, {disc_copies, [node()]}, {type, bag}]);
create_table(item_option) ->
    mnesia:create_table (item_option, [{attributes,  record_info(fields, item_option)}, {disc_copies, [node()]}]);
create_table(counter) ->
    mnesia:create_table (counter, [{attributes,  record_info(fields, counter)}, {disc_copies, [node()]}]);
create_table(user) ->
    mnesia:create_table (user, [{attributes,  record_info(fields, user)}, {disc_copies, [node()]}, {type, set}]).

create_tables() ->
    create_tables(?TABLES).

create_tables([]) -> ok;
create_tables([Table|Rest]) ->
    create_table(Table),
    create_tables(Rest).

clear_tables() ->
    clear_tables(?TABLES).

clear_tables([]) ->
     ok;
clear_tables([Table|Rest]) ->
    clear_table(Table),
    clear_tables(Rest).

clear_table(Table) ->
    mnesia:clear_table(Table).

teardown() -> 
	mnesia:stop(),
	mnesia:delete_schema([node()]).


write (Row) ->
    F = fun() ->
                mnesia:write (Row)
        end,
    mnesia:transaction (F).

