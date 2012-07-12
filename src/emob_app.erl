%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Main module for the Erlymob server application.
%%% @end
%%%-------------------------------------------------------------------
-module(emob_app).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("defaults.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    app_cache:cache_init(get_table_infos()),
    emob_oauth_fsm_sup:start_link(),
    emob_listener:start(),
    emob_sup:start_link().


stop(_State) ->
    ok.


%% Get the table definitions for app_cache
get_table_infos() ->
    [get_table_info(Name) || Name <- [?POST, ?POST_RSVP, ?POST_IGNORE, ?USER]].

get_table_info(?POST) ->
    app_cache_table_info:table_info(?POST, ordered_set);
get_table_info(?POST_RSVP) ->
    app_cache_table_info:table_info(?POST_RSVP, bag, 10*24*60*60);
get_table_info(?POST_IGNORE) ->
    app_cache_table_info:table_info(?POST_IGNORE, bag, 10*24*60*60);
get_table_info(?USER) ->
    app_cache_table_info:table_info(?USER, set, infinity, [access_token]).
