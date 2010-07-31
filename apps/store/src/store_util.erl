%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Some collected utils to stop cut and paste code
%%% @end
%%% Created : 24 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_util).

%% API
-export([now/0, is_valid_behaviour/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc milliseconds since epoch (an int time stamp)
%% @spec now() -> int()
%% @end
%%--------------------------------------------------------------------
now() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

%%--------------------------------------------------------------------
%% @doc Checks that the passed module exports the required functions
%% @spec is_valid([export()]) -> true | false
%% @end
%%--------------------------------------------------------------------
is_valid_behaviour(_Behaviour, []) ->
    exit(bad_mod);
is_valid_behaviour(Behaviour, Exports) ->
    lists:all( fun(X) -> lists:member(X, Exports) end, Behaviour:behaviour_info(callbacks)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
