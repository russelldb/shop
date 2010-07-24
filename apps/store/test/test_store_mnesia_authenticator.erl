%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Use mnesia to store users and auth info
%%% @end
%%% Created : 17 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_store_mnesia_authenticator).

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
		     {ok, _} = store_event_manager:start_link(),
		     store_auth_event_handler:register(),
		     store_test_db:write(User),
		     store_test_db:write(LockedUser),
		     store_test_db:write(ALockedUser)
	    end, 
     fun(X) -> teardown(X) end,
     [?_assertMatch({ok, _}, store_mnesia_authenticator:authenticate(Username, Password)),
      ?_assertMatch(fail,  store_mnesia_authenticator:authenticate(Username, "gibbergabber")),
      ?_assertMatch(locked,  store_mnesia_authenticator:authenticate(LUsername, Password)),
      ?_assertMatch(fail,  store_mnesia_authenticator:authenticate("Herman", Password)),
      ?_assertMatch(admin_locked,  store_mnesia_authenticator:authenticate(ALUsername, Password))]}.

%%--------------------------------------------------------------------
%% @doc tests that a successful login updates the last login time, but returns the *last* logged in time before this
%% @spec
%% @end
%%--------------------------------------------------------------------
success_test_() ->
    LastLogin = store_util:now(),
    Username = "user",
    Password = "password",
    Hash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #user{username=Username, password=Hash, last_login=LastLogin},

    {setup, fun() -> setup(),
		     store_test_db:write(User),
		     {Username, Password, LastLogin}
	    end, 
     fun(X) -> teardown(X) end,
     fun(X) -> generate_success(X) end}.

%%--------------------------------------------------------------------
%% @doc generates asserts to check current last login is after User#user.last_login
%% @spec generate_success({UserName, Password, LastLogin})
%% @end
%%--------------------------------------------------------------------
generate_success({Username, Password, LastLogin}) ->
    {ok, #user{last_login=LL}} = store_mnesia_authenticator:authenticate(Username, Password),
    #user{last_login=LL2} = store_test_db:get_user(Username),
    [?_assertMatch(LL, LastLogin), ?_assert(LL2 > LastLogin)].
    

%%--------------------------------------------------------------------
%% @doc tests that a failing login ups the failed count for the user
%% @spec
%% @end
%%--------------------------------------------------------------------
fail_test_() ->
    Username = "user",
    Password = "password",
    Hash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #user{username=Username, password=Hash},
    {setup, fun() -> setup(),
		     store_test_db:write(User),
		     {Username, store_util:now()}
	    end, 
     fun(X) -> teardown(X) end,
     fun(X) -> generate_fail(X) end}.

%%--------------------------------------------------------------------
%% @doc generates asserts to check current last fail is after TS
%% @spec generate_fail({UserName, TS})
%% @end
%%--------------------------------------------------------------------
generate_fail({Username, TS}) ->
    Res = store_mnesia_authenticator:authenticate(Username, "gibbergabber"),
    #user{last_fail=LastFail, fail_count=FC} = store_test_db:get_user(Username),
    [?_assertMatch(fail, Res), ?_assertMatch(FC, 1), ?_assert(LastFail > TS)].
    
%%--------------------------------------------------------------------
%% @doc tests that a failing login over the max count locks the account
%% @spec
%% @end
%%--------------------------------------------------------------------
lock_test_() ->
    Username = "user",
    Password = "password",
    Hash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #user{username=Username, password=Hash, fail_count=2},
    {setup, fun() -> setup(),
		     store_test_db:write(User),
		     User
	    end, 
     fun(X) -> teardown(X) end,
     [?_assertMatch(fail,  store_mnesia_authenticator:authenticate(Username, "gibbergabber")),
      ?_assertMatch(#user{fail_count=3}, store_test_db:get_user(Username)),
      ?_assertMatch(fail,  store_mnesia_authenticator:authenticate(Username, "gibbergabber")),
      ?_assertMatch(locked,  store_mnesia_authenticator:authenticate(Username, Password)),
      ?_assertMatch(#user{fail_count=3, locked=true}, store_test_db:get_user(Username))]}.


%%--------------------------------------------------------------------
%% @doc tests that a successful login resets fail count
%% @spec
%% @end
%%--------------------------------------------------------------------
reset_fail_test_() ->
    Username = "user",
    Password = "password",
    Hash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #user{username=Username, password=Hash, fail_count=2},
    {setup, fun() -> setup(),
		     store_test_db:write(User),
		     User
	    end, 
     fun(X) -> teardown(X) end,
     [?_assertMatch(fail, store_mnesia_authenticator:authenticate(Username, "gibbergabber")),
      ?_assertMatch({ok, _}, store_mnesia_authenticator:authenticate(Username, Password)),
      ?_assertMatch(#user{fail_count=0}, store_test_db:get_user(Username))]}.
