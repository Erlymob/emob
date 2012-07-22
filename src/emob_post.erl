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
-export([get_post_by_response_tag/1]).
-export([get_rsvps/1]).
-export([get_ignores/1]).
-export([get_all_posts/0]).
-export([delete_post/1]).
-export([delete_all_posts/0]).

-export([get_embedded_locations_from_post/1]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Get the post associated with the given post_id
-spec get_post(post_id()) -> #post{} | undefined | error().
get_post(PostId) ->
    case app_cache:get_data(?DIRTY, ?POST, PostId) of
        [Post] ->
            Post;
        _ ->
            undefined
    end.

%% @doc Get the post associated with the given response tag
-spec get_post_by_response_tag(emob_response_tag()) -> #post{} | undefined | error().
get_post_by_response_tag(ResponseTag) ->
    case app_cache:get_data(?DIRTY, ?POST_RESPONSE_TAG, ResponseTag) of
        [ResponseTagData] ->
            get_post(ResponseTagData#post_response_tag.post_id);
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

-spec delete_post(post_id()) -> ok.
delete_post(PostId) ->
    emob_post_receiver:delete_post(PostId).

-spec delete_all_posts() -> ok.
delete_all_posts() ->
    [emob_post_receiver:delete_post(Post#post.id) || Post <- app_cache:get_all_data(?POST)].

-spec get_embedded_locations_from_post(#post{}) -> [#location_data{}] | undefined.
get_embedded_locations_from_post(Post) ->
    emob_post_receiver:get_embedded_locations(Post).
