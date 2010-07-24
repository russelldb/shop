%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% The behavior for a store_authenticator
%%% @end
%%% Created : 24 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_authenticator_behaviour).

%% API
-export([behaviour_info/1]).

-define(API, [{authenticate, 2}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    ?API.
