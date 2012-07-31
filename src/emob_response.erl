%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Response related functions
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_response).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

% Start the filter_stream that'll look for responses
-export([look_for_response/1]).

-export([get_response/1]).
-export([get_all_responses/0]).
-export([delete_response/1]).
-export([delete_all_responses/0]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start the filter_stream
-spec look_for_response(emob_response_tag()) -> {ok, pid()}.
look_for_response(ResponseTag) ->
    StrippedTag = strip_hash_if_necessary(ResponseTag),
    lager:debug("StrippedTag:~p~n", [StrippedTag]),
    emob_response_tag_receiver_sup:start_response_tag_receiver(StrippedTag).

%% @doc Get the post associated with the given post_id
-spec get_response(emob_response_id()) -> #response{} | undefined | error().
get_response(ResponseId) ->
    case app_cache:get_data(?DIRTY, ?RESPONSE, ResponseId) of
        [Response] ->
            Response;
        _ ->
            undefined
    end.

-spec get_all_responses() -> [#response{}] | error().
get_all_responses() ->
    app_cache:get_last_n_entries(?DIRTY, ?RESPONSE, ?MAX_POSTS).

-spec delete_response(emob_response_id()) -> ok.
delete_response(ResponseId) ->
    app_cache:remove_data(?SAFE, ?RESPONSE, ResponseId).

-spec delete_all_responses() -> ok.
delete_all_responses() ->
    [app_cache:remove_data(?SAFE, ?RESPONSE, Response#response.id) || Response <- app_cache:get_all_data(?RESPONSE)].



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec strip_hash_if_necessary(binary()) -> binary().
strip_hash_if_necessary(Item) ->
    case binary:match(Item, [<<"#">>], [{scope, {0, 1}}]) of
        nomatch ->
            Item;
        _ ->
            binary:part(Item, 1, byte_size(Item) - 1)
    end.
