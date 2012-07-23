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
    [get_table_info(Name) || Name <- [?POST, ?POST_RSVP, ?POST_LIKE, ?POST_RESPONSE_TAG, ?POST_IGNORE, ?USER]].

get_table_info(?POST) ->
    app_cache_table_info:table_info(?POST, ordered_set);
get_table_info(?POST_RSVP) ->
    app_cache_table_info:table_info(?POST_RSVP, bag, ?POST_RSVP_TTL);
get_table_info(?POST_LIKE) ->
    app_cache_table_info:table_info(?POST_LIKE, bag, ?POST_LIKE_TTL);
get_table_info(?POST_RESPONSE_TAG) ->
    app_cache_table_info:table_info(?POST_RESPONSE_TAG, set, ?POST_RESPONSE_TAG_TTL);
get_table_info(?POST_IGNORE) ->
    app_cache_table_info:table_info(?POST_IGNORE, bag, ?POST_IGNORE_TTL);
get_table_info(?USER) ->
    app_cache_table_info:table_info(?USER, set, infinity, [access_token, twitter_id]).
