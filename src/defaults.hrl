%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Common header files and definitions.
%%% @end
%%%-------------------------------------------------------------------

-include("types.hrl").

%% EMOB DEFINES
-define(EMOB_POST_RECEIVER, emob_post_receiver).
-define(EMOB_POST_DISTRIBUTOR, emob_post_distributor).
-define(EMOB_USER, emob_user).
-define(LATEST, <<"latest">>).
-define(INVALID_BOOLEAN, <<"invalid_boolean">>).
-define(INVALID_BINARY, <<"invalid_binary">>).
-define(INVALID_STRING, <<"invalid_string">>).
-define(EMPTY_ERROR, <<"empty_error">>).
-define(STARTUP_TIMER, 5000).
-define(MAX_POSTS, 200).
% User
-define(USER_ACCESS_TOKEN, access_token).
-define(ID, <<"id">>).


%% TWITTERL DEFINES
-define(CALLBACK_URL, <<"callback_url">>).
-define(TOKEN, <<"token">>).
-define(OAUTH_TOKEN, <<"oauth_token">>).
-define(SECRET, <<"secret">>).
-define(OAUTH_VERIFIER, <<"oauth_verifier">>).
-define(USER_ID, <<"user_id">>).
-define(SCREEN_NAME, <<"screen_name">>).
-define(API_VERSION, <<"1.0">>).

-define(INVALID_SESSION, <<"invalid_session">>).
-define(INVALID_USER, <<"invalid_user">>).
-define(GPROC_UNKNOWN_PROCESS, <<"gproc_unknown_process">>).
-define(INVALID_TOKEN_DATA, <<"invalid_token_data">>).

-define(DEFAULT_TIMER_TIMEOUT, 5000).
-define(OAUTH_FSM, oauth_fsm).


-define(FIRST_POST, 1).


%% TWITTER SPECIFIC RECORDS

-record(twitter_token_data, {
            access_token            :: binary(),
            access_token_secret     :: binary()
            }).

-record(twitter_access_data, {
            access_token            :: binary(),
            access_token_secret     :: binary(),
            user_id                 :: binary(),
            screen_name             :: binary()
            }).

-record(oauth_step_1, {
            access_token            :: binary(),
            access_token_secret     :: binary()
            }).

-record(oauth_step_2, {
            access_token            :: binary(),
            access_verifier         :: binary()
            }).

%% Twitter

-record(bounding_box, {
            type          :: binary(),
            coordinates   :: list()
            }).

-record(twitter_place, {
            id            :: binary(),
            url           :: binary(),
            place_type    :: binary(),
            name          :: binary(),
            full_name     :: binary(),
            country_code  :: binary(),
            country       :: binary(),
            bounding_box  :: #bounding_box{}
            }).

-record(twitter_user, {
            id_str        :: binary(),
            id            :: integer(),
            name          :: binary(),
            screen_name   :: binary(),
            location      :: any(),
            description   :: any(),
            profile_image_url   :: any()
            }).

-record(entity_url, {
            url           :: binary(),
            expanded_url  :: binary(),
            display_url   :: binary()
            }).

-record(entities, {
            hashtags      :: list(),
            urls          :: list()
            }).

-record(tweet, {
            id_str        :: binary(),
            id            :: integer(),
            text          :: binary(),
            coordinates   :: any(),
            place         :: any(),
            created_at    :: any(),
            user          :: #twitter_user{},
            entities      :: #entities{}
            }).




%% APP_CACHE defines and records

-define(SAFE,     safe).
-define(DIRTY,     dirty).
-define(POST_TTL,        ?INFINITY).
-define(USER_TTL,        ?INFINITY).
-define(POST_RSVP_TTL,   60*60*24*100).
-define(POST_IGNORE_TTL, 60*60*24*100).


-define(TABLES, [
            #app_metatable{
                table = ?POST,
                version = 1,
                time_to_live = ?POST_TTL,
                type = ordered_set},

            #app_metatable{
                table = ?POST_RSVP,
                version = 1,
                time_to_live = ?POST_RSVP_TTL,
                type = bag},

            #app_metatable{
                table = ?POST_IGNORE,
                version = 1,
                time_to_live = ?POST_IGNORE_TTL,
                type = bag},

            #app_metatable{
                table = ?USER,
                version = 1,
                time_to_live = ?POST_IGNORE_TTL,
                type = bag,
                secondary_index_fields = [access_token]
                }
            ]).
-define(FOO, fields(post)).

%% All tables must have a field called timestamp. Somewhere. For sure.
-define(POST, post).
-record(post, {
          id                                        :: twitter_id(),
          timestamp                                 :: timestamp(),
          location                                  :: any(),
          post_data                                 :: #tweet{},
          processed = false                         :: post_processed_status()
         }).


-define(POST_RSVP, post_rsvp).
-record(post_rsvp, {
          id                                        :: post_id(),
          timestamp                                 :: timestamp(),
          rsvp_user                                 :: user_id()
         }).

-define(POST_IGNORE, post_ignore).
-record(post_ignore, {
          id                                        :: post_id(),
          timestamp                                 :: timestamp(),
          ignore_user                               :: user_id()
         }).


-define(USER, user).
-record(user, {
          id                                        :: twitter_id(),
          timestamp                                 :: timestamp(),
          location                                  :: any(),
          access_token                              :: token(),
          access_token_secret                       :: secret(),
          screen_name                               :: screen_name(),
          user_id                                   :: user_id(),
          profile_picture                           :: profile_picture(),
          last_post_processed = ?FIRST_POST         :: post_id(),
          callback                                  :: target()
         }).


