%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Post related functions
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_post).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_post/1]).
-export([get_rsvps/1]).
-export([get_ignores/1]).
-export([get_all_posts/0]).
-export([empty_posts/0]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec get_post(post_id()) -> #post{} | undefined | error().
get_post(PostId) ->
    case app_cache:get_data(?DIRTY, ?POST, PostId) of
        [Post] ->
            Post;
        _ ->
            undefined
    end.

-spec get_rsvps(post_id()) -> [user_id()] | error().
get_rsvps(PostId) ->
    [X#post_rsvp.rsvp_user || X <- app_cache:get_data(?DIRTY, ?POST_RSVP, PostId)].

-spec get_ignores(post_id()) -> [user_id()] | error().
get_ignores(PostId) ->
    [X#post_ignore.ignore_user || X <- app_cache:get_data(?DIRTY, ?POST_IGNORE, PostId)].


% TODO: Limit to 200
-spec get_all_posts() -> [#post{}] | error().
get_all_posts() ->
    app_cache:get_last_n_entries(?DIRTY, ?POST, ?MAX_POSTS).

-spec empty_posts() -> {atomic, ok} | error().
empty_posts() ->
    mnesia:clear_table(?POST).

