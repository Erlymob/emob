%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc emob response_tag receiver supervisor
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_response_tag_receiver_sup).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').


-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_response_tag_receiver/1]).

%% Supervisor callbacks
-export([init/1]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Type, Module, Args), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_response_tag_receiver(emob_response_tag()) -> any().
start_response_tag_receiver(ResponseTag) ->
    supervisor:start_child(?MODULE, [ResponseTag]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Token = twitterl:get_env(oauth_access_token, <<"undefined">>),
    Secret = twitterl:get_env(oauth_access_token_secret, <<"undefined">>),
    Receiver = ?CHILD(make_ref(), worker, emob_response_tag_receiver, [Token, Secret]), 
    {ok, { {simple_one_for_one, 5, 300}, [Receiver]} }.
