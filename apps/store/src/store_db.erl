%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% store_db is a gen_server that puts store items into the db.
%%% @end
%%% Created :  6 Jul 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(store_db).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, add_item/1, add_item/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(STORE_API, [{add_item, 1}, {add_item, 2}, {fetch_all_items, 0}]).

-include("store.hrl").


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Env) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Env, []).

%%--------------------------------------------------------------------
%% @doc stops the server
%% @spec stop() -> 
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc calls the callback module's add_item/1 function
%% @spec add_item(item()) -> ok
%% @end
%%--------------------------------------------------------------------
add_item(Item) when is_record(Item, item) ->
    gen_server:call(?SERVER, {add, Item}).

%%--------------------------------------------------------------------
%% @doc adds an item and its options
%% @spec add_item(item(), [opt()]) -> ok
%% @end
%%--------------------------------------------------------------------
add_item(Item, Opts) when is_record(Item, item), is_list(Opts) ->
    gen_server:call(?SERVER, {add, Item, Opts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Env) ->
    Store_db = proplists:get_value(db_module, Env),
    true = is_valid(Store_db:module_info(exports)),
    {ok, {db, Store_db}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, Item}, _From, {db, Db}=State) when is_record(Item, item) ->
    Res = Db:add_item(Item),
    {reply, Res, State};
handle_call({add, Item, Opts}, _From, {db, Db}=State) when is_record(Item, item), is_list(Opts) ->
    Res = Db:add_item(Item, Opts),
    {reply, Res, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Checks that the passed module exports the required functions
%% @spec is_valid([export()]) -> true | false
%% @end
%%--------------------------------------------------------------------
is_valid([]) ->
    exit(bad_mod);
is_valid(Exports) ->
    lists:all( fun(X) -> lists:member(X, Exports) end,  ?STORE_API).


