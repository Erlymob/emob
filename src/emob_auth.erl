%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Authentication related functions
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_auth).
-define(SERVER, ?MODULE).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_request_token/1,
         get_access_token/2,
         get_credentials/1,
         remove_credentials/1]).

%% ------------------------------------------------------------------
%% Includes and Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%% @doc Get a request token.
-spec get_request_token(url()) -> #twitter_token_data{} | error().
get_request_token(CallbackURL) ->
    emob_oauth_fsm:get_request_token(CallbackURL).

           
%% @doc Get an access token.
-spec get_access_token(token(), verifier()) -> #twitter_access_data{} | error().
get_access_token(Token, Verifier) ->
    emob_oauth_fsm:get_access_token(Token, Verifier).

%% @doc Get stored credentials for a token. Returns #access_data{}
-spec get_credentials(token()) -> #twitter_access_data{} | error().
get_credentials(Token) ->
    emog_oauth_fsm:get_credentials(Token).

%% @doc Remove stored credentials
-spec remove_credentials(token()) -> ok.
remove_credentials(Token) ->
    emob_oauth_fsm:remove_credentials(Token).

