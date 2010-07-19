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

%%% Public API
%%--------------------------------------------------------------------
%% @doc checks the provided credential
%% @spec authenticate(string(), string()) -> {ok, {user, string(), {role, [role()]}}} | {fail} (also logs result)
%% @end
%%--------------------------------------------------------------------
authenticate(Username, Password) ->
    case q(qlc:q([X || X <- mnesia:table(user), X#user.username =:= Username ])) of
	[] -> store_event_manager:notify({fail, unknown_user, Username}),
	      fail;
	[User] ->
	    case  valid_password(User#user.password, Password) of
		true ->
		    check_for_lock(User);
		false ->
		    store_event_manager:notify({fail, bad_paasword, User}),
		    fail
	    end
    end.

%%--------------------------------------------------------------------
%% @doc compares hash of pword to stored hash of pword
%% @spec
%% @end
%%--------------------------------------------------------------------
valid_password(Hash, Password) ->
    Hash =:= bcrypt:hashpw(Password, Hash).

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
%% @doc checks for account locks
%% @spec check_for_locks(user()) -> {ok, user()} | locked | admin_locked
%% @end
%%--------------------------------------------------------------------
check_for_lock(#user{locked=false, admin_locked=false} = User) ->
    store_event_manager:notify({success, User}),
    {ok, User};
check_for_lock(#user{locked=true}=User) ->
    store_event_manager:notify({fail, locked, User}),
    locked;
check_for_lock(#user{admin_locked=true}=User) ->
    store_event_manager:notify({fail, admin_locked, User}),
    admin_locked.
    
