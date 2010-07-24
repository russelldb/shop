%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Uses mnesia to store user creds.
%%% @end
%%% Created : 15 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_authenticator).

-export([authenticate/2]).

-include_lib("stdlib/include/qlc.hrl").
-include("store.hrl").

-define(MAX_FAILS, 3).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc checks the provided credential
%% @spec authenticate(string(), string()) -> {ok, user()} | fail | locked |admin_locked
%% @end
%%--------------------------------------------------------------------
authenticate(Username, Password) ->
    case q(qlc:q([X || X <- mnesia:table(user), X#user.username =:= Username ])) of
	[] -> store_event_manager:notify({fail, unknown_user, Username}),
	      fail;
	[User] ->
	    validate_user(User, Password)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks that the found user is allowed to logon
%% @spec validate_user(user(), Password::string()) -> {ok, user()} | fail | locked |admin_locked
%% @end
%%--------------------------------------------------------------------
validate_user(User, Password) when is_record(User, user) ->
    case  valid_password(User#user.password, Password) of
	true ->
	    check_for_lock(User);
	false ->
	    handle_fail(User, ?MAX_FAILS),
	    store_event_manager:notify({fail, bad_paasword, User}),
	    fail
    end.

%%--------------------------------------------------------------------
%% @doc compares hash of pword to stored hash of pword
%% @spec
%% @end
%%--------------------------------------------------------------------
valid_password(Hash, Password) ->
    Hash =:= bcrypt:hashpw(Password, Hash).

%%--------------------------------------------------------------------
%% @doc checks for account locks
%% @spec check_for_locks(user()) -> {ok, user()} | locked | admin_locked
%% @end
%%--------------------------------------------------------------------
check_for_lock(#user{locked=false, admin_locked=false} = User) ->
    handle_successful_login(User),
    store_event_manager:notify({success, User}),
    {ok, User};
check_for_lock(#user{locked=true}=User) ->
    store_event_manager:notify({fail, locked, User}),
    locked;
check_for_lock(#user{admin_locked=true}=User) ->
    store_event_manager:notify({fail, admin_locked, User}),
    admin_locked.

%%--------------------------------------------------------------------
%% @doc updates the user's record. Update last_login date, reset failed couunt and write to db
%% @spec successful_login(User) -> ok | fail
%% @end
%%--------------------------------------------------------------------
handle_successful_login(User) when is_record(User, user) ->
    LastLogin = store_util:now(),
    User2 = User#user{fail_count=0, last_login=LastLogin},
    write(User2).

%%--------------------------------------------------------------------
%% @doc checks fail count, increments or locks depending
%% @spec handle_fail(user()) -> ok
%% @end
%%--------------------------------------------------------------------
handle_fail(#user{fail_count=Max}=User, Max) ->
    write(User#user{locked=true});
handle_fail(#user{fail_count=FC}=User, _) ->
    FC2 = FC + 1,
    LastFail = store_util:now(),
    write(User#user{fail_count=FC2, last_fail=LastFail}).


%%--------------------------------------------------------------------
%% @doc runs a qlc query in a transaction
%% @spec q(qlcq()) ->
%% @end
%%--------------------------------------------------------------------
q(Q) ->
    F = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%%--------------------------------------------------------------------
%% @doc just write a row
%% @spec write(record()) -> ok
%% @end
%%--------------------------------------------------------------------
write(Row) ->
    F = fun() ->
		mnesia:write(Row) end,
    {atomic, ok} = mnesia:transaction(F),
    ok.
