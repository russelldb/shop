%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@Russell-Browns-MacBook-Pro.local>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2010 by Russell Brown <russell@Russell-Browns-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(basket_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% creates a new, basket containing the given item and returns that basket id and id
%%
%% @spec new() -> {ok, {Id(), contents()}} | {error, Error}
%% @end
%%--------------------------------------------------------------------
new(Item) when is_record(Item ,item) ->
    gen_server:call({new, Item}).

%%--------------------------------------------------------------------
%% @doc
%% get the contents of the basket with BasketId 
%%
%% @spec get() -> {ok, {Id(), contents()}} | {error, Error}
%% @end
%%--------------------------------------------------------------------
get(BasketId) ->
    gen_server:call({get, BasketId}).


%%--------------------------------------------------------------------
%% @doc
%% deletes the basket with BasketId 
%%
%% @spec get() -> {ok, {Id(), []}} | {error, Error}
%% @end
%%--------------------------------------------------------------------
empty(BasketId) ->
    gen_server:call({empty, BasketId}).

%%--------------------------------------------------------------------
%% @doc
%% adds the item to the basket
%%
%% @spec get() -> {ok, {Id(), []}} | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_item(BasketId, Item)->
    
    

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
init([]) ->
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
