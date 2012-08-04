%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Main module for the emob application.
%%% @end
%%%-------------------------------------------------------------------
-module(emob_SUITE).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("../src/defaults.hrl").
 
%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------
-define(is_equal(A,B),
        case A =:= B of
            true -> true;
            false -> 
                ct:pal("Not equal, A:~p, B:~p~n", [A, B]),
                erlang:exit({not_equal, A,B})
        end).

-define(is_member(A,B),
        case lists:member(A,B) of
            true -> true;
            false ->
                ct:pal("not_member, A:~p, B:~p~n", [A, B]),
                erlang:exit({not_member, A, B})
        end).

%% ------------------------------------------------------------------
%% Exports
%% ------------------------------------------------------------------
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).
-export([t_get_user/1]).

-export([t_test_post_roundtrip/1]).
-export([t_test2_post_roundtrip/1]).

-export([t_test_check_response_tag/1]).
-export([t_test2_check_response_tag/1]).

-export([t_test_rsvp_post/1]).
-export([t_test2_rsvp_post/1]).

-export([t_test_ignore_post/1]).
-export([t_test2_ignore_post/1]).

-export([t_test_like_post/1]).
-export([t_test2_like_post/1]).

 
groups() -> [{check_roundtrips, [parallel], 
              [t_test_post_roundtrip, t_test2_post_roundtrip]},
             {check_response_tags, [parallel], 
              [t_test_check_response_tag, t_test2_check_response_tag]},
             {rsvp_posts, [parallel], 
              [t_test_rsvp_post, t_test2_rsvp_post]},
             {ignore_posts, [parallel], 
              [t_test_ignore_post, t_test2_ignore_post]},
             {like_posts, [parallel], 
              [t_test_like_post, t_test2_like_post]}
            ].

all() -> [{group, rsvp_posts}, 
          {group, ignore_posts},
          {group, like_posts}
         ].
 
%% Init functions
init_per_suite(Config) ->
    emob:setup(),
    emob:start(),
    timer:sleep(5000),
    ct:pal("Started...~n~n~n", []),
    initialize_system(Config).
end_per_suite(_Config) ->
    emob:stop().

init_per_group(check_roundtrips, Config) ->
    clear_all_users(Config),
    Config;
init_per_group(_, Config) ->
    Config.
end_per_group(_, _Config) ->
    ok.

init_per_testcase(t_test_post_roundtrip, Config) ->
    clear_user(test, Config);
init_per_testcase(t_test2_post_roundtrip, Config) ->
    clear_user(test2, Config);
init_per_testcase(t_test_check_response_tag, Config) ->
    clear_user(test, Config);
init_per_testcase(t_test2_check_response_tag, Config) ->
    clear_user(test2, Config);
init_per_testcase(t_test_rsvp_post, Config) ->
    create_user(test, Config),
    clear_user(test, Config);
init_per_testcase(t_test2_rsvp_post, Config) ->
    create_user(test2, Config),
    clear_user(test2, Config);
init_per_testcase(t_test_ignore_post, Config) ->
    create_user(test, Config),
    clear_user(test, Config);
init_per_testcase(t_test2_ignore_post, Config) ->
    create_user(test2, Config),
    clear_user(test2, Config);
init_per_testcase(t_test_like_post, Config) ->
    create_user(test, Config),
    clear_user(test, Config);
init_per_testcase(t_test2_like_post, Config) ->
    create_user(test2, Config),
    clear_user(test2, Config);
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.
%% ------------------------------------------------------------------
%% Test Function Definitions
%% ------------------------------------------------------------------
% Create a user w/out a screen_name. Have to do a round-trip to twitter to get
% the actual screen_name
t_get_user(Config) ->
    ok = create_user(test, Config),
    TestUser = ?config(test_user, Config),
    User = emob_user:get_user(TestUser#twitter_user.id_str, ?SAFE),
    ?is_equal(User#user.screen_name, TestUser#twitter_user.screen_name).

t_test_post_roundtrip(Config) ->
    post_roundtrip(test, Config).

t_test2_post_roundtrip(Config) ->
    post_roundtrip(test2, Config).

t_test_check_response_tag(Config) ->
    check_response_tag(test, Config).

t_test2_check_response_tag(Config) ->
    check_response_tag(test2, Config).

t_test_rsvp_post(Config) ->
    rsvp_post(test, Config).

t_test2_rsvp_post(Config) ->
    rsvp_post(test2, Config).

t_test_ignore_post(Config) ->
    ignore_post(test, Config).

t_test2_ignore_post(Config) ->
    ignore_post(test2, Config).

t_test_like_post(Config) ->
    like_post(test, Config).

t_test2_like_post(Config) ->
    like_post(test2, Config).


%%
%% Helper Functions
%%

%%
%%  Initialization
%%
initialize_system(Config) ->
    NewConfig = get_users_and_vars(Config),
    clear_all_users(NewConfig),
    NewConfig.

get_users_and_vars(Config) ->
    Test2Token = twitterl:get_env(test2_oauth_access_token),
    Test2Secret = twitterl:get_env(test2_oauth_access_token_secret),
    TestToken = twitterl:get_env(test_oauth_access_token),
    TestSecret = twitterl:get_env(test_oauth_access_token_secret),
    Token = twitterl:get_env(oauth_access_token),
    Secret = twitterl:get_env(oauth_access_token_secret),
    User = emob_user:get_default_twitter_user(Token, Secret),
    TestUser = emob_user:get_default_twitter_user(TestToken, TestSecret),
    Test2User = emob_user:get_default_twitter_user(Test2Token, Test2Secret),
    [{token, Token},
     {secret, Secret},
     {user, User},
     {test_token, TestToken},
     {test_secret, TestSecret},
     {test2_token, Test2Token},
     {test2_secret, Test2Secret},
     {test_user, TestUser},
     {test2_user, Test2User} | Config].

clear_all_users(Config) ->
    clear_user(default, Config),
    clear_user(test, Config),
    clear_user(test2, Config),
    Config.

% Clear all the tweet's that we have for the user
clear_user(default, Config) ->
    Token = ?config(token, Config),
    Secret = ?config(secret, Config),
    User = ?config(user, Config),
    ScreenName = User#twitter_user.screen_name,
    clear_user_tweets(ScreenName, Token, Secret),
    Config;
clear_user(test, Config) ->
    Token = ?config(test_token, Config),
    Secret = ?config(test_secret, Config),
    User = ?config(test_user, Config),
    ScreenName = User#twitter_user.screen_name,
    clear_user_tweets(ScreenName, Token, Secret),
    Config;
clear_user(test2, Config) ->
    Token = ?config(test2_token, Config),
    Secret = ?config(test2_secret, Config),
    User = ?config(test2_user, Config),
    ScreenName = User#twitter_user.screen_name,
    clear_user_tweets(ScreenName, Token, Secret),
    Config.

clear_user_tweets(ScreenName, Token, Secret) ->
    clear_user_tweets(ScreenName, undefined, Token, Secret).

clear_user_tweets(ScreenName, Id, Token, Secret) ->
    try
        SScreenName = util:get_string(ScreenName),
        case twitterl:statuses_user_timeline({self, self}, [{"count", "1"}, {"screen_name", SScreenName}], Token, Secret) of
            #tweet{id_str = Id} = Tweet ->
	            ct:pal("Failed destroying tweet:~p~n", [Tweet]),
	            erlang:exit(fail);
	        Tweet when is_record(Tweet, tweet) ->
	            destroy_tweet(Tweet, Token, Secret),
	            clear_user_tweets(ScreenName, Token, Secret);
	        Error1 ->
	            ct:pal("Error:~p~n", [Error1]),
	            {error, Error1}
	    end
    catch
        _:{timeout, _} = Error2 ->
            {error, Error2}
    end.

destroy_tweet(Tweet, Token, Secret) ->
    Id = util:get_string(Tweet#tweet.id_str),
    twitterl:statuses_destroy({self, self}, Id, [], Token, Secret).

%%
%% Tweets
%%
%% @doc Get the first mention in the user's feed
get_mention(default, Config) ->
    Token = ?config(token, Config),
    Secret = ?config(secret, Config),
    get_the_mention(Token, Secret);
get_mention(test, Config) ->
    Token = ?config(test_token, Config),
    Secret = ?config(test_secret, Config),
    get_the_mention(Token, Secret);
get_mention(test2, Config) ->
    Token = ?config(test2_token, Config),
    Secret = ?config(test2_secret, Config),
    get_the_mention(Token, Secret).

get_the_mention(Token, Secret) ->
    timer:sleep(2000),
    try
        twitterl:statuses_mentions({self, self}, 
                               [{"count", "1"}, 
                                {"include_entities", "true"}], Token, Secret)
    catch
        _:{timeout, _} = Error ->
            {error, Error}
    end.


%%
%%  Users
%%
create_user(default, Config) ->
    Token = ?config(token, Config),
    Secret = ?config(secret, Config),
    User = ?config(user, Config),
    Id = User#twitter_user.id_str,
    create_user(Id, Token, Secret);
create_user(test, Config) ->
    Token = ?config(test_token, Config),
    Secret = ?config(test_secret, Config),
    User = ?config(test_user, Config),
    Id = User#twitter_user.id_str,
    create_user(Id, Token, Secret);
create_user(test2, Config) ->
    Token = ?config(test2_token, Config),
    Secret = ?config(test2_secret, Config),
    User = ?config(test2_user, Config),
    Id = User#twitter_user.id_str,
    create_user(Id, Token, Secret).

% user w/out screenname
create_user(UserId, Token, Secret) ->
    create_user(UserId, undefined, Token, Secret).
% user w/ screenname
create_user(UserId, ScreenName, Token, Secret) ->
    User = #user{id = UserId,
                 screen_name = ScreenName,
                 access_token = Token,
                 access_token_secret = Secret},
    app_cache:set_data(User).

%%
%%  Posts
%%
%% Send a post to the default user from the test/test2 user
post_roundtrip(Type, Config) ->
    Tweet = send_post(Type, Config),
    true = is_record(Tweet, tweet),
    Post = emob_post:get_post(Tweet#tweet.id),
    ?is_equal(Post#post.post_data#tweet.id,Tweet#tweet.id).

%% Compare the response tag to the one that actually is associated with the post
check_response_tag(Type, Config) ->
    Tweet = send_post(Type, Config),
    % Wait to make sure the mention got back
    Mention = get_mention(Type, Config),
    Post = emob_post:get_post(Tweet#tweet.id),
    ct:pal("Mention:~p~n", [Mention]),
    ?is_member(Post#post.response_tag, (Mention#tweet.entities)#entities.hashtags).

rsvp_post(Type, Config) ->
    Tweet = send_post(Type, Config),
    % Wait to make sure the mention got back
    ok = rsvp_post_internal(Type, Tweet, Config),
    ok = rsvp_post_internal(Type, Tweet, Config),
    ?is_equal(length(emob_post:get_rsvps(Tweet#tweet.id)), 1),
    ok = unrsvp_post_internal(Type, Tweet, Config),
    ok = unrsvp_post_internal(Type, Tweet, Config),
    ?is_equal(length(emob_post:get_rsvps(Tweet#tweet.id)), 0).


rsvp_post_internal(test, Tweet, Config) ->
    User = ?config(test_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:rsvp_post(UserId, PostId);
rsvp_post_internal(test2, Tweet, Config) ->
    User = ?config(test2_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:rsvp_post(UserId, PostId).

unrsvp_post_internal(test, Tweet, Config) ->
    User = ?config(test_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:unrsvp_post(UserId, PostId);
unrsvp_post_internal(test2, Tweet, Config) ->
    User = ?config(test2_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:unrsvp_post(UserId, PostId).

ignore_post(Type, Config) ->
    Tweet = send_post(Type, Config),
    % Wait to make sure the mention got back
    ok = ignore_post_internal(Type, Tweet, Config),
    ok = ignore_post_internal(Type, Tweet, Config),
    ?is_equal(length(emob_post:get_ignores(Tweet#tweet.id)), 1),
    ok = unignore_post_internal(Type, Tweet, Config),
    ok = unignore_post_internal(Type, Tweet, Config),
    ?is_equal(length(emob_post:get_ignores(Tweet#tweet.id)), 0).

ignore_post_internal(test, Tweet, Config) ->
    User = ?config(test_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:ignore_post(UserId, PostId);
ignore_post_internal(test2, Tweet, Config) ->
    User = ?config(test2_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:ignore_post(UserId, PostId).

unignore_post_internal(test, Tweet, Config) ->
    User = ?config(test_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:unignore_post(UserId, PostId);
unignore_post_internal(test2, Tweet, Config) ->
    User = ?config(test2_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:unignore_post(UserId, PostId).

like_post(Type, Config) ->
    Tweet = send_post(Type, Config),
    % Wait to make sure the mention got back
    ok = like_post_internal(Type, Tweet, Config),
    ok = like_post_internal(Type, Tweet, Config),
    ?is_equal(length(emob_post:get_likes(Tweet#tweet.id)), 1),
    ok = unlike_post_internal(Type, Tweet, Config),
    ok = unlike_post_internal(Type, Tweet, Config),
    ?is_equal(length(emob_post:get_likes(Tweet#tweet.id)), 0).

like_post_internal(test, Tweet, Config) ->
    User = ?config(test_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:like_post(UserId, PostId);
like_post_internal(test2, Tweet, Config) ->
    User = ?config(test2_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:like_post(UserId, PostId).

unlike_post_internal(test, Tweet, Config) ->
    User = ?config(test_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:unlike_post(UserId, PostId);
unlike_post_internal(test2, Tweet, Config) ->
    User = ?config(test2_user, Config),
    UserId = User#twitter_user.id_str,
    PostId = Tweet#tweet.id,
    emob_user:unlike_post(UserId, PostId).

send_post(test, Config) ->
    Token = ?config(test_token, Config),
    Secret = ?config(test_secret, Config),
    DestUser = ?config(user, Config),
    send_post(DestUser, Token, Secret);
send_post(test2, Config) ->
    Token = ?config(test2_token, Config),
    Secret = ?config(test2_secret, Config),
    DestUser = ?config(user, Config),
    send_post(DestUser, Token, Secret).

send_post(DestUser, Token, Secret) ->
    Payload = build_payload(DestUser),
    Result = emob_post:send_post(Payload, Token, Secret),
    % Wait for it to go, and (hopefully) come back to us
    timer:sleep(2000),
    Result.

build_payload(User) ->
    "@" ++ 
    util:get_string(User#twitter_user.screen_name) ++
    " " ++
    util:get_string(calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

%%
%% Utility
%%
