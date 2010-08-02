%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% shop item resource
%%% @end
%%% Created :  1 Aug 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(item_resource).

-export([init/1, to_html/2, allowed_methods/2, is_authorized/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

is_authorized(Req, State) ->
    %% Ok
    %% 0. See if over https, no ssl no auth
    %% 1. Check for cookie, no cookie, no auth
    %% 2. If cookie get token and see if there is session, no session no auth
    %% 3. See if user in session, no user, no auth
    %% 4. HTTPS + Cookie + Session + User = auth'd
    %% If no auth'd send back some html to render the login page
    %% Add a hidden field value for where to go after auth (IE this request again)
    case false of
	false ->
	    {ok, Html} = tempile:render(login, [{redirect, wrq:path(Req)}]),
	    ReqDataHtml = wrq:append_to_resp_body(Html, Req),
	    ReqDataCT = wrq:set_resp_header("Content-Type", "text/html;charset=utf-8", ReqDataHtml),
	    {"NeedToLogin", ReqDataCT, State};
	true ->
	    {true, Req, State}
    end.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

to_html(ReqData, State) ->
    {"<html><body>Hello," ++ State ++ "</body></html>", ReqData, State}.
