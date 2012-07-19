%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Processing when each post is received
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_post_receiver).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([process_post/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(post_receiver_state, {
            default_user_id             :: user_id(),
            default_user_screen_name    :: screen_name(),
            stream_pid                  :: pid()
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Process the incoming tweet
%%          sent to Target
-spec process_post(#post{}) -> ok.
process_post(Post) ->
    emob_manager:safe_cast({?EMOB_POST_RECEIVER, ?EMOB_POST_RECEIVER}, {process_post, Post}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(Token, Secret) ->
    gen_server:start_link(?MODULE, [Token, Secret], []).

init([Token, Secret]) ->
    process_flag(trap_exit, true),
    emob_manager:register_process(?EMOB_POST_RECEIVER, ?EMOB_POST_RECEIVER),
    DestPid = self(),
    process_tweets(DestPid, Token, Secret),
    DefaultUser = emob_user:get_default_twitter_user(Token, Secret),

    State = #post_receiver_state{default_user_screen_name = DefaultUser#twitter_user.screen_name,
                                 default_user_id = DefaultUser#twitter_user.id_str},
    {ok, State}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_post, Tweet}, State) ->
    % TODO error?
    PostId = Tweet#tweet.id,
    case app_cache:key_exists(?SAFE, ?POST, PostId) of
        false ->
            PostRecord = #post{
                    id = PostId,
                    post_data = Tweet},
            app_cache:set_data(?SAFE, PostRecord),
            respond_to_post(Tweet, State),
            emob_post_distributor:distribute_post(PostId);
        true ->
            ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Get the tweet from twitterl
handle_info(Tweet, State) when is_record(Tweet, tweet) ->
    process_post(Tweet),
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

-spec process_tweets(pid(), token(), secret()) -> ok | pid().
process_tweets(DestPid, Token, Secret) ->
    SinceId =
    case app_cache:get_data_by_last_key(?DIRTY, ?POST) of
        [] ->
            ?FIRST_POST;
        [Post] ->
            Post#post.id
    end,
    SSinceId = util:get_string(SinceId),
    proc_lib:spawn_link(fun() ->
                %% TODO fix this so this happens only after init is completed
                timer:sleep(?STARTUP_TIMER),
                twitterl:statuses_home_timeline({process, DestPid}, [{"since_id", SSinceId}], Token, Secret),
                %% TODO fill hole between these two requests
                twitterl:statuses_user_timeline_stream({process, DestPid}, [], Token, Secret)
        end).

-spec respond_to_post(#tweet{}, #post_receiver_state{}) -> any().
respond_to_post(Tweet, State) ->
    Id = Tweet#tweet.id_str,
    UserId = (Tweet#tweet.user)#twitter_user.id_str,
    ScreenName = (Tweet#tweet.user)#twitter_user.screen_name,
    SScreenName = util:get_string(ScreenName),
    DefaultUserId = State#post_receiver_state.default_user_id,
    Token = twitterl:get_env(oauth_access_token, <<"undefined">>),
    Secret = twitterl:get_env(oauth_access_token_secret, <<"undefined">>),
    if UserId =:= DefaultUserId ->
            void;
        true ->
            Status = get_status(SScreenName),
            Result = twitterl:statuses_update({debug, foo}, [{"status", Status}, {"in_reply_to_status_id", binary_to_list(Id)}], Token, Secret),
            Result
    end.

-spec get_status(string()) -> string().
get_status(SScreenName) ->
    Id = app_cache:cached_sequence_next_value(?EMOB_RECEIVER_SEQ),
    ResponseHash = ?EMOB_RESPONSE_BASE ++ string:right(util:get_base62(Id),
                                                     ?EMOB_RESPONSE_CHAR_COUNT,
                                                     ?EMOB_RESPONSE_PAD_CHAR),
    "@" ++ SScreenName ++ " " ++ ResponseHash.

    





