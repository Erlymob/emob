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
-define(INVALID_INTEGER, <<"invalid_integer">>).
-define(EMPTY_ERROR, <<"empty_error">>).
-define(TWITTER_ERROR, <<"twitter_error">>).
-define(STARTUP_TIMER, 5000).
-define(MAX_POSTS, 200).

-define(EMOB_RECEIVER_SEQ, emob_receiver_seq).
-define(EMOB_RESPONSE_BASE, "mob").
-define(EMOB_RESPONSE_CHAR_COUNT, 6).
-define(EMOB_RESPONSE_PAD_CHAR, $0).

-define(EMOB_REQUEST_CACHED, cached).
-define(EMOB_REQUEST_LIVE, live).

% User
-define(USER_ACCESS_TOKEN, access_token).
-define(USER_TWITTER_ID, twitter_id).
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
-include("../deps/twitterl/include/twitterl.hrl").

%% EMOB LOCATION DATA

-define(GOOGLE_MAPS_BASE_MATCH, [<<"http://maps.google.com/maps?">>, <<"https://maps.google.com/maps?">>]).
-define(BING_MAPS_BASE_MATCH, [<<"http://www.bing.com/maps?">>, <<"https://www.bing.com/maps?">>]).

-define(LOCATION_TYPE_TWITTER, twitter).
-define(LOCATION_TYPE_WEB, web).
-define(LOCATION_TYPE_GOOGLE, google).
-define(LOCATION_TYPE_BING, bing).
-define(LOCATION_DATA, location_data).
-record(?LOCATION_DATA, {
          type                                      :: emob_location_type(),
          location = <<>>                           :: binary(),
          geo = []                                  :: list(),
          place                                     :: #twitter_place{},
          timestamp                                 :: epoch(),
          raw_url                                   :: url()
         }).


%% APP_CACHE defines and records

-define(SAFE,     safe).
-define(DIRTY,     dirty).
-define(POST_TTL,        ?INFINITY).
-define(USER_TTL,        ?INFINITY).
-define(POST_RSVP_TTL,   60*60*24*100).
-define(POST_RESPONSE_TAG_TTL, 60*60*24*100).
-define(POST_IGNORE_TTL, 60*60*24*100).



%% All tables must have a field called timestamp. Somewhere. For sure.
-define(POST, post).
-record(?POST, {
          id                                        :: twitter_id(),
          timestamp                                 :: timestamp(),
          locations = []                            :: [#location_data{}],
          post_data                                 :: #tweet{},
          response_tag                              :: emob_response_tag(),
          processed = false                         :: post_processed_status()
         }).


-define(POST_RSVP, post_rsvp).
-record(?POST_RSVP, {
          id                                        :: post_id(),
          timestamp                                 :: timestamp(),
          rsvp_user                                 :: user_id()
         }).

-define(POST_RESPONSE_TAG, post_response_tag).
-record(?POST_RESPONSE_TAG, {
          id                                        :: emob_response_tag(),
          timestamp                                 :: timestamp(),
          post_id                                   :: post_id()
         }).

-define(POST_IGNORE, post_ignore).
-record(?POST_IGNORE, {
          id                                        :: post_id(),
          timestamp                                 :: timestamp(),
          ignore_user                               :: user_id()
         }).


-define(USER, user).
-record(?USER, {
          id                                        :: user_id(),
          timestamp                                 :: timestamp(),
          locations = []                            :: [#location_data{}],
          access_token                              :: token(),
          access_token_secret                       :: secret(),
          screen_name                               :: screen_name(),
          twitter_id                                :: twitter_id(),
          profile_picture                           :: profile_picture(),
          last_post_processed = ?FIRST_POST         :: post_id(),
          callback                                  :: target(),
          tweet                                    :: #tweet{}
         }).

