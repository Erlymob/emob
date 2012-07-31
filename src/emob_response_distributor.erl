%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2011-2012 Juan Jose Comellas, Mahesh Paolini-Subramanya
%%% @doc Processing when each response is distributed
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(emob_response_distributor).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([distribute_response/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Defines
%% ------------------------------------------------------------------
-include("defaults.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Process the incoming tweet
%%          sent to Target
-spec distribute_response(emob_response_id()) -> ok.
distribute_response(ResponseId) ->
    emob_manager:safe_cast({?EMOB_RESPONSE_DISTRIBUTOR, ?EMOB_RESPONSE_DISTRIBUTOR}, {distribute_response, ResponseId}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    emob_manager:register_process(?EMOB_RESPONSE_DISTRIBUTOR, ?EMOB_RESPONSE_DISTRIBUTOR),
    State = {},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({distribute_response, ResponseId}, State) ->
    {IsProcessed, Response} = get_processed_status_and_response(ResponseId),
    case IsProcessed of
        true ->
            ok;
        false ->
            send_response_to_users(Response),
            set_response_to_processed(Response)
    end,
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
-spec get_processed_status_and_response(emob_response_id()) -> {response_processed_status(), #response{}}.
get_processed_status_and_response(ResponseId) ->
    case app_cache:get_data(?SAFE, ?RESPONSE, ResponseId) of
        [Response] ->
            {Response#response.processed, Response};
        _ ->
            {false, #response{}}
    end.

-spec send_response_to_users(#response{}) -> ok | error().
send_response_to_users(Response) ->
    emob_user:notify_users_of_response(Response).

-spec set_response_to_processed(#response{}) -> ok | error().
set_response_to_processed(Response) ->
    app_cache:set_data(?SAFE, Response#response{processed = true}).
