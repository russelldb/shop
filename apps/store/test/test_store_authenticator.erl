%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Use mnesia to store users and auth info
%%% @end
%%% Created : 17 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_authenticator).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include ("store.hrl").


%%% Initialize database and tables. 
setup() ->
    store_test_db:setup(),
    ok.

teardown(_X) ->
    store_test_db:teardown().


%%--------------------------------------------------------------------
%% @doc tests that auth works (and fails)
%% @spec
%% @end
%%--------------------------------------------------------------------
auth_test_() ->
    Username = "user",
    Password = "password",
    LUsername = "LockedUser",
    ALUsername = "AdminLockedUser",
    Hash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #user{username=Username, password=Hash},
    LockedUser = #user{username=LUsername, password=Hash, locked=true, admin_locked=true},
    ALockedUser = #user{username=ALUsername, password=Hash, locked=false, admin_locked=true},

    {setup, fun() -> setup(),
		     {ok, Pid} = store_event_manager:start_link(),
		     store_auth_event_handler:register(),
		     store_test_db:write(User),
		     store_test_db:write(LockedUser),
		     store_test_db:write(ALockedUser)
	    end, 
     fun(X) -> teardown(X) end,
     [?_assertMatch({ok, _}, store_authenticator:authenticate(Username, Password)),
      ?_assertMatch(fail,  store_authenticator:authenticate(Username, "gibbergabber")),
      ?_assertMatch(locked,  store_authenticator:authenticate(LUsername, Password)),
      ?_assertMatch(fail,  store_authenticator:authenticate("Herman", Password)),
      ?_assertMatch(admin_locked,  store_authenticator:authenticate(ALUsername, Password))]}.

