%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Tests the gen_server store_session modeule
%%% @end
%%% Created : 31 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_session).

-behaviour(store_session_behaviour).

%% API
-export([get/2, put/3, clear/1, new/0, init/1]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc stops the server
%% @spec teardown() -> ok
%% @end
%%--------------------------------------------------------------------
teardown() ->
    store_session:stop(), 
    timer:sleep(500),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init_test_() ->
    {setup, fun() -> no_op end, fun(_) -> teardown() end,
     [ ?_assertMatch({ok, _}, store_session:start_link([{session_module, ?MODULE}, {session_env, [{hi, there}]}])) ]}.

clear_test_() ->
    {setup, fun() ->
		    {ok, _} = store_session:start_link([{session_module, ?MODULE}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch(ok, store_session:clear(sesh_id))]}.

get_test_() ->
    {setup, fun() ->
		    {ok, _} = store_session:start_link([{session_module, ?MODULE}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch( get_value, store_session:get(sesh_id, key))]}.

put_test_() ->
    {setup, fun() ->
		    {ok, _} = store_session:start_link([{session_module, ?MODULE}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch( ok, store_session:put(sesh_id, key, val))]}.

new_test_() ->
    {setup, fun() ->
		    {ok, _} = store_session:start_link([{session_module, ?MODULE}]) end,
     fun(_) ->
	     teardown() end,
     [?_assertMatch( new_called, store_session:new())]}.

init(_Env) ->
    ok.

clear(_SessionId) ->
    ok.

get(_SessionId, _Key) ->
    get_value.

put(_SessionId, _Key, _Val) ->
    ok.

new() ->
    new_called.



