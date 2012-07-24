%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Manages the user
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_user).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_user/1, get_user/2]).
-export([get_user_locations/1, get_user_locations/2]).
-export([get_user_location/2, get_user_location/3]).
-export([set_user_location/2]).
-export([get_user_from_post/1]).
-export([update_user_from_post/1]).
-export([set_callback/2]).

-export([get_default_twitter_user/2]).

-export([get_posts/1]).
-export([rsvp_post/2]).
-export([unrsvp_post/2]).
-export([ignore_post/2]).
-export([unignore_post/2]).
-export([like_post/2]).
-export([unlike_post/2]).
-export([process_post/2]).
-export([notify_users_of_post/1]).
-export([notify_user_of_readiness/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

-record(state, {
            token                   :: token(),
            secret                  :: secret(),
            user_id                 :: user_id()
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%%% USER
%% @doc Get the User profile
-spec get_user(user_id()) -> #twitter_user{} | error().
get_user(UserId) ->
    get_user(UserId, ?SAFE).

-spec get_user(user_id(), emob_request_type()) -> #twitter_user{} | error().
get_user(UserId, RequestType) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_user, RequestType}).

-spec get_user_locations(user_id()) -> #twitter_user{} | error().
get_user_locations(UserId) ->
    get_user_locations(UserId, ?SAFE).

-spec get_user_locations(user_id(), emob_request_type()) -> #twitter_user{} | error().
get_user_locations(UserId, RequestType) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_user_locations, RequestType}).

-spec get_user_location(user_id(), emob_location_type()) -> #twitter_user{} | error().
get_user_location(UserId, LocationType) ->
    get_user_location(UserId, LocationType, ?SAFE).

-spec get_user_location(user_id(), emob_location_type(), emob_request_type()) -> #twitter_user{} | error().
get_user_location(UserId, LocationType, RequestType) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_user_location, LocationType, RequestType}).

-spec set_user_location(user_id(), #location_data{}) -> #twitter_user{} | error().
set_user_location(UserId, Location) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {set_user_location, Location}).

-spec get_user_from_post(#post{}) -> user_id() | error().
get_user_from_post(Post) ->
    try
        UserId = ((Post#post.post_data)#tweet.user)#twitter_user.id_str,
        case app_cache:get_data(?SAFE, ?USER, UserId) of
            [User] ->
                User;
            _ ->
                {error, {?INVALID_USER, UserId}}
        end
    catch
        _:_ ->
            {error, {?INVALID_POST, Post#post.id}}
    end.

-spec update_user_from_post(#post{}) -> #user{} | error().
update_user_from_post(Post) ->
    UserId = ((Post#post.post_data)#tweet.user)#twitter_user.id_str,
    emob_manager:safe_call({?EMOB_USER, UserId}, {update_user_from_post, Post}).

-spec set_callback(user_id(), target()) -> ok | error().
set_callback(UserId, Callback) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {set_callback, Callback}).

%%% TWITTER
%% @doc Get the Default User's Profile
-spec get_default_twitter_user(token(), secret()) -> #twitter_user{} | error().
get_default_twitter_user(Token, Secret) ->
    get_default_twitter_user_internal(Token, Secret).


%%% POSTS
%% @doc Retrieve the latest posts from the user
-spec get_posts(user_id()) -> [#post{}] | error().
get_posts(UserId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {get_posts}).

%% @doc RSVP to a post
-spec rsvp_post(user_id(), post_id()) -> ok | error().
rsvp_post(UserId, PostId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {rsvp_post, PostId}).

%% @doc un-RSVP to a post
-spec unrsvp_post(user_id(), post_id()) -> ok | error().
unrsvp_post(UserId, PostId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {unrsvp_post, PostId}).

%% @doc Like a post
-spec like_post(user_id(), post_id()) -> ok | error().
like_post(UserId, PostId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {like_post, PostId}).

%% @doc un-Like to a post
-spec unlike_post(user_id(), post_id()) -> ok | error().
unlike_post(UserId, PostId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {unlike_post, PostId}).

%% @doc Ignore to a post
-spec ignore_post(user_id(), post_id()) -> ok | error().
ignore_post(UserId, PostId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {ignore_post, PostId}).

%% @doc UnIgnore to a post
-spec unignore_post(user_id(), post_id()) -> ok | error().
unignore_post(UserId, PostId) ->
    emob_manager:safe_call({?EMOB_USER, UserId}, {unignore_post, PostId}).

%% @doc Process the incoming tweet
%%          sent to Target
-spec process_post(user_id(), post_id()) -> ok.
process_post(UserId, PostId) ->
    emob_manager:safe_cast({?EMOB_USER, UserId}, {process_post, PostId}).

%% @doc Notify all the users that a new post exists
% TODO have callback users in a seperate table, efficiently notify them, etc.
notify_users_of_post(Post) ->
    UserFun = fun() -> mnesia:foldl(fun(X, Acc) ->
                    Target = X#user.callback,
                    twitterl:respond_to_target(Target, {post, Post}),
                    Acc
                end, [], ?USER) end,
    mnesia:transaction(UserFun).

%% @doc Notify a given user that the post is ready for action
%%      (min_users have rsvp'd)
notify_user_of_readiness(PostId) ->
    User = get_user_from_post(emob_post:get_post(PostId)),
    notify_user_of_readiness(User, PostId).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link(Token, Secret, UserId) ->
    gen_server:start_link(?MODULE, [Token, Secret, UserId], []).

init([Token, Secret, UserId]) ->
    emob_manager:register_process(?EMOB_USER, UserId),
    % get data from the cache
    State = #state{token = Token,
                   secret = Secret,
                   user_id = UserId},
    {ok, State}.

handle_call({get_user, RequestType}, _From, State) ->
    UserId = State#state.user_id,
    Token = State#state.token,
    Secret = State#state.secret,
    User = get_user_internal(UserId, RequestType, Token, Secret),
    Response = validate_user(User),
    {reply, Response, State};

handle_call({get_user_location, LocationType, RequestType}, _From, State) ->
    UserId = State#state.user_id,
    Token = State#state.token,
    Secret = State#state.secret,
    Location = get_user_location_internal(UserId, LocationType, RequestType, Token, Secret),
    {reply, Location, State};

handle_call({get_user_locations, RequestType}, _From, State) ->
    UserId = State#state.user_id,
    Token = State#state.token,
    Secret = State#state.secret,
    Locations = get_user_locations_internal(UserId, RequestType, Token, Secret),
    {reply, Locations, State};

handle_call({set_user_location, Location}, _From, State) ->
    UserId = State#state.user_id,
    Result = set_user_location_internal(UserId, Location),
    {reply, Result, State};

handle_call({update_user_from_post, Post}, _From, State) ->
    Response = update_user_from_post_internal(Post),
    {reply, Response, State};

handle_call({rsvp_post, PostId}, _From, State) ->
    UserId = State#state.user_id,
    Response = rsvp_post_internal(UserId, PostId),
    {reply, Response, State};

handle_call({unrsvp_post, PostId}, _From, State) ->
    UserId = State#state.user_id,
    Response = unrsvp_post_internal(UserId, PostId),
    {reply, Response, State};

handle_call({like_post, PostId}, _From, State) ->
    UserId = State#state.user_id,
    Response = like_post_internal(UserId, PostId),
    {reply, Response, State};

handle_call({unlike_post, PostId}, _From, State) ->
    UserId = State#state.user_id,
    Response = unlike_post_internal(UserId, PostId),
    {reply, Response, State};

handle_call({ignore_post, PostId}, _From, State) ->
    UserId = State#state.user_id,
    Response = ignore_post_internal(UserId, PostId),
    {reply, Response, State};

handle_call({unignore_post, PostId}, _From, State) ->
    UserId = State#state.user_id,
    Response = unignore_post_internal(UserId, PostId),
    {reply, Response, State};

handle_call({set_callback, Callback}, _From, State) ->
    UserId = State#state.user_id,
    Response = set_callback_internal(UserId, Callback),
    {reply, Response, State};

handle_call({get_posts}, _From, State) ->
    UserId = State#state.user_id,
    Token = State#state.token,
    Secret = State#state.secret,
    User = get_user_internal(UserId, ?DIRTY, Token, Secret),
    Posts = update_posts_from_cache(User),
    {reply, {ok, Posts}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_post, PostId}, State) ->
    UserId = State#state.user_id,
    Token = State#state.token,
    Secret = State#state.secret,
    User = get_user_internal(UserId, ?DIRTY, Token, Secret),
    notify_user_of_post(User, PostId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Identify the twitter user that this application is running as
-spec get_default_twitter_user_internal(token(), secret()) -> #twitter_user{} | error().
get_default_twitter_user_internal(Token, Secret) ->
    SToken = util:get_string(Token),
    SSecret = util:get_string(Secret),
    case twitterl:account_verify_credentials({self, self}, [], SToken, SSecret) of
        TwitterUser when is_record(TwitterUser, twitter_user) ->
            TwitterUser;
        Error ->
            lager:error("Error getting default user:~p~n", [Error]),
            Error
    end.

%% @doc Get all the user's locations
-spec get_user_locations_internal(user_id(), emob_request_type(), token(), secret()) -> #location_data{}.
get_user_locations_internal(UserId, RequestType, Token, Secret) ->
    case get_user_internal(UserId, RequestType, Token, Secret) of
        User when is_record(User, user) ->
            User#user.locations;
        Error ->
            Error
    end.

%% @doc Get the value of a given location_type from the user's locations
-spec get_user_location_internal(user_id(), emob_location_type(), emob_request_type(), token(), secret()) -> #location_data{}.
get_user_location_internal(UserId, LocationType, RequestType, Token, Secret) ->
    case get_user_internal(UserId, RequestType, Token, Secret) of
        User when is_record(User, user) ->
            get_location(LocationType, User#user.locations);
        Error ->
            Error
    end.

%% @doc Set the user's location based on whateer the API sends in.
%%      Can possibly overwrite data received from elsewhere
-spec set_user_location_internal(user_id(), #location_data{}) -> #location_data{} | error().
set_user_location_internal(UserId, Location) ->
    case get_user_internal(UserId, ?DIRTY, undefined, undefined) of
        User when is_record(User, user) ->
            NewLocations = update_locations(Location, User#user.locations, true),
            NewUser = User#user{locations = NewLocations},
            app_cache:set_data(NewUser),
            Location;
        Error ->
            Error
    end.

%% @doc Get a user's data from twitter, update the local version, and return it
-spec get_user_internal(user_id(), emob_request_type(), token(), secret()) -> #user{}.
get_user_internal(UserId, ?SAFE, Token, Secret) ->
    TwitterUser = get_user_data_from_twitter(UserId, Token, Secret),
    if TwitterUser#twitter_user.id_str =:= UserId ->
            update_user_from_twitter_user(TwitterUser);
        true ->
            {error, {?TWITTER_ERROR, UserId}}
    end;
%% @doc Return the cached version of the user's data
get_user_internal(UserId, ?DIRTY, _Token, _Secret) ->
    case app_cache:get_data(?DIRTY, ?USER, UserId) of
        [User] ->
            User;
        _ ->
            #user{}
    end.

-spec get_user_from_twitter_id(user_id()) -> #user{}.
get_user_from_twitter_id(UserId) ->
    case app_cache:get_data_from_index(?SAFE, ?USER, UserId, ?USER_TWITTER_ID) of
        [User] ->
            User;
        _ ->
            #user{}
    end.

%% @doc Retrieve a user's data from twitter
-spec get_user_data_from_twitter(user_id(), token(), secret()) -> #twitter_user{} | false.
get_user_data_from_twitter(UserId, Token, Secret) ->
    SUserId = util:get_string(UserId),
    twitterl:users_show({self, self}, [{"user_id", SUserId}, {"include_entities", "true"}], Token, Secret).

%% @doc Update a user based on the data in the tweet
%%      If the user doesnt exist, it gets created
-spec update_user_from_twitter_user(#twitter_user{}) -> #user{}.
update_user_from_twitter_user(TwitterUser) ->
    TwitterUserId = TwitterUser#twitter_user.id_str,
    User = get_user_from_twitter_id(TwitterUserId),
    update_user_data_from_twitter(User, TwitterUser).

-spec update_user_data_from_twitter(#user{}, #twitter_user{}) -> #user{}.
update_user_data_from_twitter(User, TwitterUser) ->
    TweetLocation = #location_data{type = ?LOCATION_TYPE_TWITTER, 
                             location = TwitterUser#twitter_user.location,
                             geo = geo_from_tweet(TwitterUser#twitter_user.status),
                             place = place_from_tweet(TwitterUser#twitter_user.status),
                             timestamp = timestamp_from_tweet(TwitterUser#twitter_user.status)},
    EmbeddedLocations = emob_post:get_embedded_locations_from_post(#post{post_data = TwitterUser#twitter_user.status}),
    NewLocations = lists:foldl(fun(Location, _Acc) ->
                    update_locations(Location, User#user.locations, false)
            end, [], [TweetLocation | EmbeddedLocations]),
    NewUser = User#user{locations = NewLocations,
              tweet = TwitterUser#twitter_user.status},
    app_cache:set_data(NewUser),
    NewUser.

%% @doc Update a user's info based on the info in the received tweet
-spec update_user_from_post_internal(#post{}) -> #user{} | error().
update_user_from_post_internal(Post) ->
    % Blank out user in the tweet
    Tweet = (Post#post.post_data)#tweet{user = undefined},
    % Set the user's status to this post
    TwitterUser = ((Post#post.post_data)#tweet.user)#twitter_user{status = Tweet},
    update_user_from_twitter_user(TwitterUser).

%% @doc Rsvp a post for a given user
-spec rsvp_post_internal(user_id(), post_id()) -> ok | error().
rsvp_post_internal(UserId, PostId) ->
    Entry = #post_rsvp{id = PostId, rsvp_user = UserId},
    Result = app_cache:set_data(?SAFE, Entry),
    notify_user_of_readiness(PostId),
    Result.

%% @doc Unrsvp a post for a given user
-spec unrsvp_post_internal(user_id(), post_id()) -> ok | error().
unrsvp_post_internal(UserId, PostId) ->
    case get_rsvp_for_user(UserId, PostId) of
        false ->
            ok;
        Entry ->
            app_cache:remove_record(?SAFE, Entry)
    end.

-spec get_rsvp_for_user(user_id(), post_id()) -> #post_rsvp{} | false.
get_rsvp_for_user(UserId, PostId) ->
    Rsvps = app_cache:get_data(?POST_RSVP, PostId),
    lists:keyfind(UserId, #post_rsvp.rsvp_user, Rsvps).

%% @doc Rsvp a post for a given user
-spec like_post_internal(user_id(), post_id()) -> ok | error().
like_post_internal(UserId, PostId) ->
    Entry = #post_like{id = PostId, like_user = UserId},
    app_cache:set_data(?SAFE, Entry).

%% @doc Unlike a post for a given user
-spec unlike_post_internal(user_id(), post_id()) -> ok | error().
unlike_post_internal(UserId, PostId) ->
    case get_like_for_user(UserId, PostId) of
        false ->
            ok;
        Entry ->
            app_cache:remove_record(?SAFE, Entry)
    end.

-spec get_like_for_user(user_id(), post_id()) -> #post_like{} | false.
get_like_for_user(UserId, PostId) ->
    Likes = app_cache:get_data(?POST_LIKE, PostId),
    lists:keyfind(UserId, #post_like.like_user, Likes).

%% @doc Ignore a given post for a user
-spec ignore_post_internal(user_id(), post_id()) -> ok | error().
ignore_post_internal(UserId, PostId) ->
    Entry = #post_ignore{id = PostId, ignore_user = UserId},
    app_cache:set_data(?SAFE, Entry).

%% @doc Unignore a given post for a user
-spec unignore_post_internal(user_id(), post_id()) -> ok | error().
unignore_post_internal(UserId, PostId) ->
    case get_ignore_for_user(UserId, PostId) of
        false ->
            ok;
        Entry ->
            app_cache:remove_record(?SAFE, Entry)
    end.

%% @doc What posts are ignored by a given user?
-spec get_ignore_for_user(user_id(), post_id()) -> #post_ignore{} | false.
get_ignore_for_user(UserId, PostId) ->
    Ignores = app_cache:get_data(?POST_IGNORE, PostId),
    lists:keyfind(UserId, #post_ignore.ignore_user, Ignores).

%% @doc Set the callback for a given user to something :-)
-spec set_callback_internal(user_id(), target()) -> #user{}.
set_callback_internal(UserId, Target) ->
    case app_cache:get_data(?SAFE, ?USER, UserId) of
        [User] ->
            app_cache:set_data(?SAFE, User#user{callback = Target});
        _ ->
            {error, ?INVALID_USER}
    end.

%% @doc Return all the posts since the last one that the user processed.
-spec update_posts_from_cache(#user{}) -> list().
update_posts_from_cache(User) ->
    case app_cache:get_after(?DIRTY, ?POST, User#user.last_post_processed) of
        [_H|_Tail] = AllPosts ->
            SortedPosts = lists:sort(fun(A,B) -> A#post.id >= B#post.id end, AllPosts),
            LimitedPosts = lists:sublist(SortedPosts, ?MAX_POSTS),
            case LimitedPosts of
                [LastPost|_] ->
                    app_cache:set_data(?SAFE, User#user{last_post_processed = LastPost#post.id});
                [] ->
                    void
            end,
            LimitedPosts;
        _ ->
            []
    end.

%% @doc Send the post to a given user
notify_user_of_post(User, PostId) ->
    Target= User#user.callback,
    case Target=/= undefined of
        true ->
            Post = app_cache:get(?DIRTY, ?POST, PostId),
            twitterl:respond_to_target(Target, {post, Post});
        false ->
            void
    end.

%% @doc Tell the user that a given post has achieved min_users
notify_user_of_readiness(User, PostId) ->
    Target= User#user.callback,
    case Target=/= undefined of
        true ->
            Post = app_cache:get(?DIRTY, ?POST, PostId),
            twitterl:respond_to_target(Target, {post_ready, Post});
        false ->
            void
    end.

%% @doc Get a specific location for a user. If it doesnt exist, create an empty
%%      one
-spec get_location(emob_location_type(), [#location_data{}]) -> #location_data{} | false.
get_location(LocationType, Locations) ->
    case lists:keyfind(LocationType, #location_data.type, Locations) of
        Location when is_record(Location, location_data) ->
            Location;
        false ->
            #location_data{type = LocationType, 
                           timestamp = util:datetime_to_epoch(calendar:universal_time())}
    end.

%% @doc Update the list of #location_data{} for a user
-spec update_locations(#location_data{} | undefined, [#location_data{}], boolean()) -> #location_data{}.
update_locations(undefined, Locations, _UseTimestamp) -> Locations;
update_locations(Location, Locations, UseTimestamp) ->
    LocationType = Location#location_data.type,
    OldLocation = get_location(LocationType, Locations),
    case is_same_location(OldLocation, Location, UseTimestamp) of
        true ->
            Locations;
        false -> 
            lists:keystore(LocationType, #location_data.type, Locations, Location)
    end.

%% @doc Are the two locations the same? UserTimestamp is used to check w/ and
%%      w/out the timestamp field
-spec is_same_location(#location_data{}, #location_data{}, boolean()) -> boolean().
is_same_location(OldLocation, Location, _UseTimestamp = true) ->
    (OldLocation#location_data.geo =:= Location#location_data.geo) andalso
    (OldLocation#location_data.place =:= Location#location_data.place) andalso
    (OldLocation#location_data.location =:= Location#location_data.location);
is_same_location(OldLocation, Location, _UseTimestamp = false) ->
    OldLocation =:= Location.

%% @doc extract the geo information from a tweet
%%      NOTE:  This is actually the "coordinates" field in the tweet
-spec geo_from_tweet(#tweet{} | null) -> #bounding_box{} | null.
geo_from_tweet(Tweet) when is_record(Tweet, tweet) ->
    Tweet#tweet.coordinates;
geo_from_tweet(_) -> null.

%% @doc extract the place information from a tweet
-spec place_from_tweet(#tweet{} | null) -> #bounding_box{} | null.
place_from_tweet(Tweet) when is_record(Tweet, tweet) ->
    Tweet#tweet.place;
place_from_tweet(_) -> null.

%% @doc extract the timestamp from a tweet
-spec timestamp_from_tweet(#tweet{} | null) -> epoch().
timestamp_from_tweet(Tweet) when is_record(Tweet, tweet) ->
    Tweet#tweet.created_at;
timestamp_from_tweet(_) ->
    null.

%% @doc Make sure that this is actually a valid user
-spec validate_user(#user{}) -> #user{} | error().
validate_user(User) ->
    if User#user.id  =/= undefined ->
            User;
        true ->
            {error, ?INVALID_USER}
    end.

