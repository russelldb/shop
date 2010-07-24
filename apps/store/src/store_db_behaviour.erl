%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% The behavior for a store_db
%%% @end
%%% Created : 24 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_db_behaviour).

%% API
-export([behaviour_info/1]).

-define(STORE_API, [{add_item, 1}, {add_item, 2}, {fetch_all_items, 0}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    ?STORE_API.
