%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% defines api for session modules
%%% @end
%%% Created : 25 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_session_behaviour).

%% API
-export([behaviour_info/1]).

-define(API, [{get_value, 2}, {put, 3}, {clear, 1}, {new, 0}, {init, 1}]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc the functions that make up the behaviour
%% @spec behaviour_info(callbacks) -> [{Fun::atom(), Arity::int()}]
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    ?API.
