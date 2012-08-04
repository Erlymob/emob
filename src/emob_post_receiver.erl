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
-export([delete_post/1]).
-export([set_min_users_for_post/2]).

-export([get_tweet_locations/1]).
-export([get_hashtags/1]).
-export([get_embedded_locations/1]).

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
            token                       :: token(),
            secret                      :: secret(),
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

-spec delete_post(post_id()) -> ok.
delete_post(PostId) ->
    lager:debug("PostId:~p~n", [PostId]),
    emob_manager:safe_cast({?EMOB_POST_RECEIVER, ?EMOB_POST_RECEIVER}, {delete_post, PostId}).

-spec set_min_users_for_post(post_id(), integer()) -> ok | error().
set_min_users_for_post(PostId, Count) ->
    emob_manager:safe_cast({?EMOB_POST_RECEIVER, ?EMOB_POST_RECEIVER}, {set_min_users_for_post, PostId, Count}).

-spec get_tweet_locations(#post{}) -> [#location_data{}] | undefined.
get_tweet_locations(Post) ->
    get_tweet_locations_internal(Post#post.post_data).

-spec get_hashtags(#post{}) -> [#location_data{}] | undefined.
get_hashtags(Post) ->
    get_hashtags_internal(Post#post.post_data).

-spec get_embedded_locations(#post{}) -> [#location_data{}] | undefined.
get_embedded_locations(Post) ->
    get_embedded_locations_internal(Post#post.post_data).
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

    State = #post_receiver_state{token = Token,
                                 secret = Secret,
                                 default_user_screen_name = DefaultUser#twitter_user.screen_name,
                                 default_user_id = DefaultUser#twitter_user.id_str},
    {ok, State}.



handle_call({set_min_users_for_post, PostId, Count}, _From, State) ->
    Result = 
    case app_cache:get_data(?SAFE, ?POST, PostId) of
        [Post] ->
            app_cache:set_data(Post#post{min_users = Count});
        _ ->
            {error, {?INVALID_POST, PostId}}
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_post, Tweet}, State) ->
    % TODO error?
    PostId = Tweet#tweet.id,
    case app_cache:key_exists(?SAFE, ?POST, PostId) of
        false ->
            EmbeddedLocations = get_embedded_locations_internal(Tweet),
            TweetLocations = get_tweet_locations_internal(Tweet),
            PostRecord = #post{
                    id = PostId,
                    locations = EmbeddedLocations ++ TweetLocations,
                    post_data = Tweet},
            respond_to_post(PostRecord, State),
            emob_post_distributor:distribute_post(PostId);
        true ->
            ok
    end,
    {noreply, State};

handle_cast({delete_post, PostId}, State) ->
    Token = State#post_receiver_state.token,
    Secret = State#post_receiver_state.secret,
    SPostId = util:get_string(PostId),
    Foo = twitterl:statuses_destroy({self, self}, SPostId, [], Token, Secret),
    lager:debug("Foo:~p~n", [Foo]),
    app_cache:remove_data(?SAFE, ?POST, PostId),
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

%% @doc send a response back to the original poster with a post hash that they
%%      can use as a reference
-spec respond_to_post(#post{}, #post_receiver_state{}) -> any().
respond_to_post(Post, State) ->
    Tweet = Post#post.post_data,
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
            {ResponseTag, Status} = get_response_tag_and_status(SScreenName),
            Result = twitterl:statuses_update({debug, foo}, [{"status", Status}, {"in_reply_to_status_id", binary_to_list(Id)}], Token, Secret),
            update_post_response_tag(Post, ResponseTag),
            Result
    end.

-spec get_response_tag_and_status({emob_response_tag(), string()}) -> string().
get_response_tag_and_status(SScreenName) ->
    Id = app_cache:cached_sequence_next_value(?EMOB_RECEIVER_SEQ),
    ResponseTag = ?EMOB_RESPONSE_BASE ++ string:right(util:get_base62(Id),
                                                     ?EMOB_RESPONSE_CHAR_COUNT,
                                                     ?EMOB_RESPONSE_PAD_CHAR),
    {ResponseTag, "@" ++ SScreenName ++ " #" ++ ResponseTag}.

%% @doc Update the post with the response tag for the post.
%%      Also, update the reverse lookup table
-spec update_post_response_tag(#post{}, emob_response_tag()) -> ok.
update_post_response_tag(Post, ResponseTag) ->
    BResponseTag = util:get_binary(ResponseTag),
    app_cache:set_data(?SAFE, Post#post{response_tag = BResponseTag}),
    app_cache:set_data(?SAFE, #post_response_tag{id = BResponseTag,
                                                 post_id = Post#post.id}).


%% @doc Get the Hashtags embedded in the tweet
-spec get_hashtags_internal(#tweet{}) -> [emob_response_tag()].
get_hashtags_internal(Tweet) ->
    case Tweet#tweet.entities of
        Entities when is_record(Entities, entities) ->
            case Entities#entities.hashtags of
                undefined ->
                    [];
                Other ->
                    Other
            end;
        _ ->
            []
    end.

%% @doc Get the locations embedded in the place field of the tweet
-spec get_tweet_locations_internal(#tweet{}) -> [#location_data{}].
get_tweet_locations_internal(#tweet{place = Place} = Tweet) when is_record(Place, twitter_place) ->
    Timestamp = Tweet#tweet.created_at,
    [#location_data{type = ?LOCATION_TYPE_TWITTER,
                    location = (Tweet#tweet.user)#twitter_user.location,
                    place = Place,
                    timestamp = Timestamp}];
get_tweet_locations_internal(_) ->
    [].


%% @doc Get the locations embedded in any known URLs in the tweet
-spec get_embedded_locations_internal(#tweet{} | undefined) -> [#location_data{}].
get_embedded_locations_internal(undefined) ->
    [];
get_embedded_locations_internal(Tweet) ->
    Timestamp = Tweet#tweet.created_at,
    case Tweet#tweet.entities of
        Entities when is_record(Entities, entities) ->
            TweetURLs = Entities#entities.urls,
            lists:flatten([get_location_data_from_url(URL#entity_url.expanded_url, Timestamp) || URL <- TweetURLs]);
        _ ->
            []
    end.

-spec get_location_data_from_url(url() | undefined, timestamp()) -> #location_data{}.
get_location_data_from_url(undefined, _) -> [];
get_location_data_from_url(URL, Timestamp) -> 
    try
        case binary:match(URL, ?GOOGLE_MAPS_BASE_MATCH) of
            {0, Length} ->
                Params = binary:part(URL, Length, byte_size(URL) - (Length)),
                build_location_data(?LOCATION_TYPE_GOOGLE, Params, URL, Timestamp);
            _ ->
                case binary:match(URL, ?BING_MAPS_BASE_MATCH) of
                    {0, Length} ->
                        Params = binary:part(URL, Length, byte_size(URL) - (Length)),
                        build_location_data(?LOCATION_TYPE_BING, Params, URL, Timestamp);
                    _ ->
                        []
                end
        end
    catch
        _:_ ->
            []
    end.

%% @doc Build out the #location_data{} based on the type of the URL
%%      Add yahoo/bing/whatever here
-spec build_location_data(emob_location_type(), list(), url(), timestamp()) -> #location_data{} | [].
build_location_data(LocationType = ?LOCATION_TYPE_GOOGLE, Params, URL, Timestamp) ->
    SParams = util:get_string(Params),
    Props = oauth:uri_params_decode(SParams),
    #location_data{type = LocationType,
                   location = extract_location_from_props(LocationType, Props),
                   geo = extract_geo_from_props(LocationType, Props),
                   timestamp = Timestamp,
                   raw_url = URL};
build_location_data(LocationType = ?LOCATION_TYPE_BING, Params, URL, Timestamp) ->
    SParams = util:get_string(Params),
    Props = oauth:uri_params_decode(SParams),
    #location_data{type = LocationType,
                   location = extract_location_from_props(LocationType, Props),
                   geo = extract_geo_from_props(LocationType, Props),
                   timestamp = Timestamp,
                   raw_url = URL};
build_location_data(_Type, _Params, _URL, _TImestamp) -> [].

%% @doc Get the coordinates from the query string, and return the geoJson
-spec extract_geo_from_props(emob_location_type(), list()) -> #bounding_box{} | undefined.
extract_geo_from_props(LocationType = ?LOCATION_TYPE_GOOGLE, Props) ->
    LatLong = case lists:keyfind("ll", 1, Props) of
        {"ll", Val} ->
            Val;
        false ->
            case lists:keyfind("sll", 1, Props) of
                {"sll", Val} ->
                    Val;
                false ->
                    undefined
            end
    end,
    build_geo_json(LocationType, LatLong);
extract_geo_from_props(LocationType = ?LOCATION_TYPE_BING, Props) ->
    LatLong = case lists:keyfind("cp", 1, Props) of
        {"cp", Val} ->
            Val;
        false ->
            undefined
    end,
    build_geo_json(LocationType, LatLong).

%% @doc Get the geoJson for the provided Lat Long
%%      Remember, in geoGson, LatLong are reversed
-spec build_geo_json(emob_location_type(), string() | undefined) -> #bounding_box{} | undefined.
build_geo_json(_, undefined) -> undefined;
build_geo_json(?LOCATION_TYPE_GOOGLE, LatLong) ->
    case string:tokens(LatLong, ",") of
        [SLat, SLong] ->
            Lat = bstr:to_number(util:get_binary(SLat)),
            Long = bstr:to_number(util:get_binary(SLong)),
            #bounding_box{type = <<"Point">>, 
                          coordinates = [Long, Lat]};
        _ ->
            undefined
    end;
build_geo_json(?LOCATION_TYPE_BING, LatLong) ->
    case string:tokens(LatLong, "~") of
        [SLat, SLong] ->
            Lat = bstr:to_number(util:get_binary(SLat)),
            Long = bstr:to_number(util:get_binary(SLong)),
            #bounding_box{type = <<"Point">>, 
                          coordinates = [Long, Lat]};
        _ ->
            undefined
    end.

%% @doc Get the query string that was entered in Google
-spec extract_location_from_props(emob_location_type(), list()) -> binary() | undefined.
extract_location_from_props(?LOCATION_TYPE_GOOGLE, SParams) ->
    case lists:keyfind("q", 1, SParams) of
        {"q", SLocation} ->
            util:get_binary(SLocation);
        false ->
            undefined
    end;
extract_location_from_props(?LOCATION_TYPE_BING, SParams) ->
    case lists:keyfind("where1", 1, SParams) of
        {"where1", SLocation} ->
            util:get_binary(SLocation);
        false ->
            undefined
    end.
