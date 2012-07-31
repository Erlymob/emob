%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Processing when each tweet for a given response_tag is received
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_response_tag_receiver).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([process_response/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(post_receiver_state, {
            token                       :: token(),
            secret                      :: secret(),
            response_tag                :: emob_response_tag()
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Process the incoming tweet
-spec process_response(#tweet{}) -> ok.
process_response(Response) ->
    % Send in a post record, 'cos that is what it expects
    HashTags = emob_post_receiver:get_hashtags(#post{post_data = Response}),
    [emob_manager:safe_cast({?EMOB_RESPONSE_TAG_RECEIVER, ResponseTag}, 
                            {process_response, Response}) || ResponseTag <- HashTags].


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(Token, Secret, ResponseTag) ->
    gen_server:start_link(?MODULE, [Token, Secret, ResponseTag], []).

init([Token, Secret, ResponseTag]) ->
    process_flag(trap_exit, true),
    emob_manager:register_process(?EMOB_RESPONSE_TAG_RECEIVER, ResponseTag),
    DestPid = self(),
    process_tweets(DestPid, Token, Secret, ResponseTag),

    State = #post_receiver_state{token = Token,
                                 secret = Secret,
                                 response_tag = ResponseTag},
    {ok, State}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_response, Tweet}, State) ->
    % TODO error?
    ResponseTag = State#post_receiver_state.response_tag,
    ResponseId = {ResponseTag, Tweet#tweet.id},
    EmbeddedLocations =
                        emob_post_receiver:get_embedded_locations(#post{post_data = Tweet}),
    TweetLocations = emob_post_receiver:get_tweet_locations(#post{post_data = Tweet}),
    ResponseRecord = #response{
            id = ResponseId,
            locations = EmbeddedLocations ++ TweetLocations,
            response_data = Tweet},
    app_cache:set_data(?SAFE, ResponseRecord),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Get the tweet from twitterl
handle_info(Tweet, State) when is_record(Tweet, tweet) ->
    lager:debug("Tweet:~p~n", [Tweet]),
    process_response(Tweet),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_tweets(pid(), token(), secret(), emob_response_tag()) -> ok | pid().
process_tweets(DestPid, Token, Secret, ResponseTag) ->
    SResponseTag = util:get_string(ResponseTag),
    proc_lib:spawn_link(fun() ->
                %% TODO fix this so this happens only after init is completed
                timer:sleep(?STARTUP_TIMER),
                Foo = twitterl:statuses_filter_stream({process, DestPid}, [{"track", SResponseTag}], Token, Secret),
                lager:debug("Foo:~p~n", [Foo])
        end).
