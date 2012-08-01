%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Tom Heinan <me@tomheinan.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Tom Heinan
%%% @doc Erlymob HTTP interface for the API that manages mobs and locations.
%%% @end
%%%-------------------------------------------------------------------
-module(emob_http).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').
-author('Tom Heinan <me@tomheinan.com>').

-compile([{parse_transform, lager_transform}]).

-behaviour(cowboy_http_handler).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/3, handle/2, terminate/2]).


%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("defaults.hrl").


%% ------------------------------------------------------------------
%% Defines
%% ------------------------------------------------------------------

-define(HEADER_CONTENT_TYPE, 'Content-Type').
-define(MIME_TYPE_JSON, "application/json").

-type http_req()                                :: tuple().

-record(state, {
         peer
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init({tcp, http}, Req, _Opts) ->
    Peer = printable_peer(Req),
    %% {Method, _} = cowboy_http_req:method(Req),
    %% lager:debug("[~s] Initializing ~s ~s~n", [Peer, Method, printable_path(Req)]),
    {ok, Req, #state{peer = Peer}}.


handle(Req0, State) ->
    {Method, Req1} = cowboy_http_req:method(Req0),
    {Path, Req} = cowboy_http_req:path(Req1),
    lager:debug("[~s] ~s ~s~n", [State#state.peer, Method, printable_path(Req)]),
    {ok, Reply} = case Method of
                      'GET' ->
                          handle_get(Path, Req, State);
                      'POST' ->
                          handle_post(Path, Req, State);
                      'PUT' ->
                          handle_put(Path, Req, State);
                      'DELETE' ->
                          handle_delete(Path, Req, State);
                      _ ->
                          cowboy_http_req:reply(405, Req)    %% method not allowed
                  end,
    {ok, Reply, State}.


terminate(_Req, _State) ->
    %% {Method, _} = cowboy_http_req:method(_Req),
    %% lager:debug("[~s] Terminating ~s ~s~n", [State#state.peer, Method, printable_path(_Req)]),
    ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Update an entity.
-spec handle_put(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_put(Path, Req, State) ->
    lager:warning("[~s] Malformed PUT request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(400, Req).   %% bad request


%% @doc Retrieve an entity.
%% @doc Returns the object representation of the specified mob, including
%%      the original tweet info, plus parsed location and time data and
%%      participants who have RSVP’d.
%%      Params:
%%      id: the ID of the mob we want to retrieve
%%      token (optional): the access token for the twitter API for the user
%%                        whose data we wish to retrieve. If there’s no token
%%                        given, we can return the mob, but the "going" flag
%%                        will be missing.
-spec handle_get(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_get([<<"mob">>], Req0, _State) ->
    %% /mob?id=:id[&token=:token]
    case cowboy_http_req:qs_val(?ID, Req0) of
        {undefined, Req1} ->
            {RawQs, Req} = cowboy_http_req:raw_qs(Req1),
            lager:info("Missing 'id' in /mob request; qs=~p~n", [RawQs]),
            cowboy_http_req:reply(400, Req);   %% bad request
        {PostId, Req1} ->
            %% The token (user ID) is optional
            {AttendingUserId, Req} = user_id_from_req(Req1),
            case emob_post:get_post(bstr:to_integer(PostId)) of
                #post{} = Post ->
                    Response = json_post(Post, AttendingUserId),
                    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);
                _ ->
                    lager:info("Could not find post with id=~p~n", [PostId]),
                    cowboy_http_req:reply(404, Req)    %% not found
            end
    end;

handle_get([<<"mobs">>], Req0, _State) ->
    %% /mobs
    case cowboy_http_req:qs_val(?TOKEN, Req0) of
        {undefined, Req1} ->
            {RawQs, Req} = cowboy_http_req:raw_qs(Req1),
            lager:info("Missing 'token' in /mob request; qs=~p~n", [RawQs]),
            cowboy_http_req:reply(400, Req);   %% bad request
        %% /mobs?token=:token
        {Token, Req} ->
            case emob_auth:get_user_from_token(Token) of
                [#user{id = AttendingUserId}] ->
                    %% Once we support location-based requests we'll no longer return all the posts.
                    Response = json_posts(emob_post:get_all_posts(), AttendingUserId),
                    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);
                [] ->
                    lager:info("Could not find user for token '~s'~n", [Token]),
                    %% WARNING: an attacker may gather information about
                    %%          valid tokens with this response code.
                    cowboy_http_req:reply(404, Req);  %% not found
                {error, _Reason} = Error ->
                    lager:info("Error looking up user for token '~s': ~p~n", [Token, Error]),
                    cowboy_http_req:reply(500, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], json_error(Error), Req)
            end
    end;

handle_get([<<"get_loc">>], Req0, _State) ->
    case cowboy_http_req:qs_val(?TOKEN, Req0) of
        {undefined, Req} ->
            cowboy_http_req:reply(400, Req);   %% bad request
        %% /get_loc?token=:token
        {Token, Req} ->
            case emob_user:get_user_location(Token, ?LOCATION_TYPE_TWITTER) of
                #location_data{} = Location ->
                    Response = ejson:encode({location_centroid(Location)}),
                    cowboy_http_req:reply(200, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req);
                _ ->
                    cowboy_http_req:reply(404, Req)   %% not found
            end
    end;

handle_get([<<"get_request_token">>], Req0, _State) ->
    %% The callback_url is in the query string of the GET request
    case cowboy_http_req:qs_val(?CALLBACK_URL, Req0) of
        {undefined, Req} ->
            cowboy_http_req:reply(400, Req);   %% bad request
        %% /get_request_token?callback_url=:url
        {CallbackUrl, Req} ->
            {Code, Response} = case emob_auth:get_request_token(CallbackUrl) of
                                   TokenData when is_record(TokenData, twitter_token_data) ->
                                       {200, json_token(TokenData)};
                                   Error ->
                                       {500, json_error(Error)}
                               end,
            cowboy_http_req:reply(Code, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req)
    end;

handle_get([<<"get_access_token">>], Req0, _State) ->
    %% The Token and Verifier are in the query string itself
    %% /get_access_token?oauth_token=:oauth_token&oauth_verifier=:oauth_verifier
    {Token, Req1} = cowboy_http_req:qs_val(?OAUTH_TOKEN, Req0),
    {Verifier, Req} = cowboy_http_req:qs_val(?OAUTH_VERIFIER, Req1),
    if
        Token =:= undefined orelse Verifier =:= undefined ->
            cowboy_http_req:reply(400, Req);   %% bad request
        true ->
            {Code, Response} = case emob_auth:get_access_token(Token, Verifier) of
                                   AccessData when is_record(AccessData, twitter_access_data) ->
                                       {200, json_access(AccessData)};
                                   Error ->
                                       {500, json_error(Error)}
                               end,
            cowboy_http_req:reply(Code, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req)
    end;

handle_get([<<"get_credentials">>], Req0, _State) ->
    %% The Token is in the query string
    %% /get_credentials?oauth_token=:oauth_token
    case cowboy_http_req:qs_val(?OAUTH_TOKEN, Req0) of
        {undefined, Req} ->
            cowboy_http_req:reply(400, Req);   %% bad request
        {Token, Req} ->
            {Code, Response} = case emob_auth:get_credentials(Token) of
                                   AccessData when is_record(AccessData, twitter_access_data) ->
                                       {200, json_access(AccessData)};
                                   Error ->
                                       {500, json_error(Error)}
                               end,
            cowboy_http_req:reply(Code, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req)
    end;

handle_get([<<"remove_credentials">>], Req0, _State) ->
    %% The Token is in the body of the request
    case cowboy_http_req:qs_val(?OAUTH_TOKEN, Req0) of
        {undefined, Req} ->
            cowboy_http_req:reply(400, Req);   %% bad request
        {Token, Req} ->
            ok = emob_auth:remove_credentials(Token),
            cowboy_http_req:reply(200, Req)  %% OK
    end;

handle_get(Path, Req, State) ->
    lager:warning("[~s] Malformed GET request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(400, Req).   %% bad request


%% @doc Create or update en entity.
-spec handle_post(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_post([<<"rsvp">>], Req0, _State) ->
    %% /rsvp with id=:id&token=:token&going=true|false in the body
    {PostId, Req1} = cowboy_http_req:qs_val(?ID, Req0),
    {Token, Req2} = cowboy_http_req:qs_val(?TOKEN, Req1),
    {Going, Req} = cowboy_http_req:qs_val(?GOING, Req2),
    if
        PostId =:= undefined orelse Token =:= undefined orelse Going =:= undefined ->
            cowboy_http_req:reply(400, Req1);   %% bad request
        true ->
            {Code, Response} = case emob_auth:get_user_from_token(Token) of
                                   [#twitter_user{id_str = UserId}] ->
                                       Flag = to_boolean(Going),
                                       ok = case Flag of
                                                true  -> emob_user:rsvp_post(UserId, PostId);
                                                false -> emob_user:unrsvp_post(UserId, PostId)
                                            end,
                                       {200, ejson:encode({[{<<"going">>, Flag}]})};
                                   {error, _Reason} = Error ->
                                       lager:info("Could not find user for token '~s': ~p~n", [Token, Error]),
                                       %% WARNING: an attacker may gather information about
                                       %%          valid tokens with this response code.
                                       {400, json_error(Error)}
                               end,
            cowboy_http_req:reply(Code, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], Response, Req)
    end;

handle_post([<<"like">>], Req0, _State) ->
    %% /like with id=:id&token=:token&like=true|false in the body
    {PostId, Req1} = cowboy_http_req:qs_val(?ID, Req0),
    {Token, Req2} = cowboy_http_req:qs_val(?TOKEN, Req1),
    {Like, Req} = cowboy_http_req:qs_val(?LIKE, Req2),
    if
        PostId =:= undefined orelse Token =:= undefined orelse Like =:= undefined ->
            cowboy_http_req:reply(400, Req1);   %% bad request
        true ->
            case emob_auth:get_user_from_token(Token) of
                [#twitter_user{id_str = UserId}] ->
                    ok = case to_boolean(Like) of
                             true  -> emob_user:like_post(UserId, PostId);
                             false -> emob_user:unlike_post(UserId, PostId)
                         end,
                    cowboy_http_req:reply(200, Req);
                {error, _Reason} = Error ->
                    lager:info("Could not find user for token '~s': ~p~n", [Token, Error]),
                    %% WARNING: an attacker may gather information about
                    %%          valid tokens with this response code.
                    cowboy_http_req:reply(400, [{?HEADER_CONTENT_TYPE, <<?MIME_TYPE_JSON>>}], json_error(Error), Req)
            end
    end;

handle_post(Path, Req, State) ->
    lager:warning("[~s] Malformed POST request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(400, Req).   %% bad request


%% @doc Delete an entity.
-spec handle_delete(Path :: cowboy_dispatcher:tokens(), http_req(), #state{}) -> {ok, http_req()}.
handle_delete(Path, Req, State) ->
    lager:warning("[~s] Malformed DELETE request to ~p~n", [State#state.peer, Path]),
    cowboy_http_req:reply(400, Req).   %% bad request


% -spec ejson_to_proplist({[{Name :: binary(), Value :: binary()}]}) -> proplists:proplist().
% ejson_to_proplist({Doc}) ->
%     ejson_to_proplist(Doc, []).

% ejson_to_proplist([{Name, Value} | Tail], Acc) ->
%     %% FIXME We should be using binary_to_existing_atom/2 to convert the field
%     %%       names into atoms. Before we do that we must ensure that all the
%     %%       possible valid atoms are already in the atom table.
%     %% ejson_to_proplist(Tail, [{binary_to_existing_atom(Name, utf8), Value} | Acc]);
%     ejson_to_proplist(Tail, [{binary_to_atom(Name, utf8), Value} | Acc]);
% ejson_to_proplist([], Acc) ->
%     lists:reverse(Acc).


-spec printable_peer(Req :: term()) -> string().
printable_peer(Req) ->
    {{{I1, I2, I3, I4}, Port}, _} = cowboy_http_req:peer(Req),
    lists:flatten(io_lib:format("~w.~w.~w.~w:~w", [I1, I2, I3, I4, Port])).


-spec printable_path(Req :: term()) -> string().
printable_path(Req) ->
    {Path, _} = cowboy_http_req:raw_path(Req),
    Path.


%% ok_to_ejson() ->
%%     {[{<<"code">>, <<"ok">>}]}.

token_to_ejson(TokenData) ->
    {[{?TOKEN, TokenData#twitter_token_data.access_token},
      {?SECRET, TokenData#twitter_token_data.access_token_secret}]}.

access_to_ejson(AccessData) ->
    {[{?TOKEN, AccessData#twitter_access_data.access_token},
      {?SECRET, AccessData#twitter_access_data.access_token_secret},
      {?USER_ID, AccessData#twitter_access_data.user_id},
      {?SCREEN_NAME, AccessData#twitter_access_data.screen_name}]}.

post_to_ejson(Post = #post{post_data = Tweet}, AttendingUserId) ->
    Rsvps = emob_post:get_rsvps(Post#post.id),
    Likes = emob_post:get_likes(Post#post.id),
    %% lager:debug("Rsvps: ~p~n", [Rsvps]),
    %% lager:debug("Likes: ~p~n", [Likes]),
    {UserName, Tail} = case Tweet#tweet.user of
                           #twitter_user{screen_name = ScreenName, id_str = AttendingUserId} when is_binary(ScreenName) ->
                               {ScreenName, [{?GOING, lists:member(AttendingUserId, Rsvps)},
                                             {?LIKE, lists:member(AttendingUserId, Likes)}]};
                           _ ->
                               {null, []}
                        end,
    %% lager:debug("User: ~p~n", [UserName]),
    %% lager:debug("Post ID: ~p~n", [Post#post.id]),
    {[{<<"id">>,      Post#post.id},
      {<<"tweet">>,   Tweet#tweet.text},
      {<<"user">>,    UserName},
      {<<"created">>, Tweet#tweet.created_at},
      {<<"where">>, {post_coordinates(Post)}},
      {<<"when">>, Tweet#tweet.created_at},
      {<<"hashtag">>, Post#post.response_tag},
      {<<"rsvps">>, length(Rsvps)},
      {<<"likes">>, length(Likes)} | Tail]}.


post_coordinates(#post{locations = Locations}) ->
    location_centroid(preferred_location(Locations)).


preferred_location(Locations) ->
    preferred_location(Locations, {0, undefined}).

preferred_location([Loc | Tail], {PrevPrecedence, _PrevLoc} = Preferred) ->
    Precedence = location_precedence(Loc#location_data.type),
    NewPreferred = case Precedence > PrevPrecedence of
                       true  -> {Precedence, Loc};
                       false -> Preferred
                   end,
    preferred_location(Tail, NewPreferred);
preferred_location([], {_Precedence, Loc}) ->
    Loc.


location_precedence(web)     -> 3;
location_precedence(google)  -> 2;
location_precedence(bing)    -> 2;
location_precedence(twitter) -> 1;
location_precedence(_Type)   -> 0.


location_centroid(#location_data{geo = BBox}) when is_record(BBox, bounding_box) ->
    case bounding_box_centroid(BBox#bounding_box.coordinates) of
        [Lon, Lat] ->
            [{<<"longitude">>, Lon},
             {<<"latitude">>, Lat}];
        [] ->
            []
    end;
location_centroid(_Location) ->
    [].

bounding_box_centroid([_Lon, _Lat] = Coords) ->
    Coords;
bounding_box_centroid([[Lon, Lat] | Tail] = _Coords) ->
    case bounding_box_centroid(Tail, Lon, Lat, 1) of
        [_ | _] = Center ->
            Center;
        [] ->
            lager:debug("Invalid location ~p~n", [_Coords]),
            []
    end;
bounding_box_centroid(_Coords) ->
    lager:debug("Invalid location ~p~n", [_Coords]),
    [].

%% @doc Calculate the centroid of a polygon (triangle, tetrahedron) assuming
%%      an Euclidean space (cartesian coordinates).
%%      See http://en.wikipedia.org/wiki/Centroid#Of_triangle_and_tetrahedron
bounding_box_centroid([[Lon, Lat] | Tail], LonAcc, LatAcc, Count) ->
    bounding_box_centroid(Tail, LonAcc + Lon, LatAcc + Lat, Count + 1);
bounding_box_centroid([], LonAcc, LatAcc, Count) ->
    [LonAcc / Count, LatAcc / Count];
bounding_box_centroid(_Coords, _LonAcc, _LatAcc, _Count) ->
    [].


build_valid_response(Result) ->
    {[{result, Result},
      {version, ?API_VERSION}]}.

build_error_response({error, Error}) ->
    {[{error, bstr:bstr(Error)},
      {version, ?API_VERSION}]}.

json_error(Error) ->
    ejson:encode(build_error_response(Error)).

%% json_ok() ->
%%     Result = ok_to_ejson(),
%%     ejson:encode(build_valid_response(Result)).

json_access(AccessData) ->
    Result = access_to_ejson(AccessData),
    ejson:encode(build_valid_response(Result)).

json_token(TokenData) ->
    Result = token_to_ejson(TokenData),
    ejson:encode(build_valid_response(Result)).

%% json_post(Post) ->
%%     json_post(Post, undefined).

json_post(Post, AttendingUserId) ->
    ejson:encode(post_to_ejson(Post, AttendingUserId)).

%% json_posts(Posts) ->
%%     json_posts(Posts, undefined).

json_posts(Posts, AttendingUserId) ->
    %% FIXME Hack added to filter invalid posts (with 'undefined' IDs).
    ValidPosts = lists:filter(fun (Post) -> Post#post.id =/= undefined end, Posts),
    SortedPosts = lists:sort(fun compare_post/2, ValidPosts),
    ejson:encode([post_to_ejson(SortedPost, AttendingUserId) || SortedPost <- SortedPosts]).

compare_post(#post{post_data = T1}, #post{post_data = T2}) ->
    T1#tweet.created_at > T2#tweet.created_at.


user_id_from_req(Req0) ->
    case cowboy_http_req:qs_val(?TOKEN, Req0) of
        {undefined, _Req} = Result ->
            Result;
        {Token, Req} ->
            case emob_auth:get_user_from_token(Token) of
                [#twitter_user{id_str = UserId}] ->
                    {UserId, Req};
                Error ->
                    lager:info("Could not find user for token '~s': ~p~n", [Token, Error]),
                    {undefined, Req}
            end
    end.


-spec to_boolean(binary()) -> boolean().
to_boolean(<<"true">>) ->
    true;
to_boolean(<<"false">>) ->
    false;
to_boolean(<<"1">>) ->
    true;
to_boolean(<<"0">>) ->
    false;
to_boolean(<<"yes">>) ->
    true;
to_boolean(<<"no">>) ->
    false;
to_boolean(<<"TRUE">>) ->
    true;
to_boolean(<<"FALSE">>) ->
    false.
