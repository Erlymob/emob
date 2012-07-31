Testing from the command-line
=============================

Initial configuration for the Erlymob server Twitter user
---------------------------------------------------------

Erlymob needs a Twitter user to be able to receive tweets from
other users. The `app.config` file used by Erlymob has a section for
the *twitterl* Erlang application that looks like this:

``` erlang
{twitterl, [
  %% Tweet parsers
  {tweet_parsers, 2},
  {requestors, 1},
  {retry_count, 5},
  %% Oauth Data
  {oauth_consumer_key, "le6bGXNLfBNaDUbeOyJNog"},
  {oauth_consumer_secret, "0n6qzMt1s9wJ0AxR4VQP3Hq84YzA6P6aFUV4e7Hvk"},
  {oauth_access_token, "727027692-AKKgrMARJBw2moazuQelM3Xf0DSMCeeKSeGqbBzZ"},
  {oauth_access_token_secret, "0jdjh2jH2HOF35XV1ATChaDkb9M7yPV7ynzt06q818"}
]},
```
The OAuth tokens present in this section can be obtained by following
these steps:

1) Create a Twitter account that you're going to be using for the
Erlymob server (e.g. *erlymob_server*) and make sure that Twitter is
sending location data for the user.

2) Sign in to [dev.twitter.com](http://dev.twitter.com) with the
credentials of your new Twitter user.

3) Create a new application by clicking on the
[Create an app](https://dev.twitter.com/apps/new) link. You can use
the name of the user for the application if you want
(e.g. *erlymob_server*).

4) In *Settings* be sure to fill in the following parameters before
accepting the license terms and submitting:
    * use anything for the website URL
    * set a Callback URL (anything for now. it's necessary for the callback token stuff to work)

5) Create your access token (button at the bottom of the screen).

6) Now, go to *Settings*, and set *Application Type* to *Read and
Write*.

7) Grab the consumer key, secret, token, token_secret, and update your
`app.config` file.


Retrieving OAuth Tokens and making requests from the command line
-----------------------------------------------------------------

1) Enter the following lines in the command line to get a request token:

    # CALLBACK_URL='http://localhost:8080/ping
    CALLBACK_URL='http://www.posttestserver.com'
    REQUEST_TOKEN_URL="http://localhost:8080/get_request_token?callback_url=$CALLBACK_URL"
    curl -v -H "Accept: application/json" -X GET $REQUEST_TOKEN_URL

Result:
``` javascript
{
  "result": {
    "token": "w6kKpBh0eSmw1w9Jpy6AQMHg5aPZUkavco5TM9MgduQ",
	"secret": "Ktt9RkvmF4jCBZazWg0uTebJgDHJewbLQS1pORgZBA"
  },
  "version": "1.0"
}
```

2) Open a browser where your client Twitter user is logged in and
enter the URL resulting from the following commands, using the request
token from *step 1*:

    REQUEST_TOKEN='w6kKpBh0eSmw1w9Jpy6AQMHg5aPZUkavco5TM9MgduQ'
    TWITTER_URL="https://api.twitter.com/oauth/authenticate?oauth_token=$REQUEST_TOKEN"
    echo $TWITTER_URL

e.g.

   https://api.twitter.com/oauth/authenticate?
   oauth_token=w6kKpBh0eSmw1w9Jpy6AQMHg5aPZUkavco5TM9MgduQ

This will take you through the steps necessary to authorize the app
and automagically redirect you to the `callback_url` you entered in
*step 1*. The URL you're redirected to will contain an `oauth_token`
argument and an `oauth_verifier` argument which you will need in the
next step.

e.g.

  http://www.posttestserver.com/?
  oauth_token=5cfvsHirEdu7zRx93WcAZCrMlBj9nmhY87POuVvuQY&
  oauth_verifier=O5UFQ7wfqX18P4vS6wn6igemItChLeiYYFWFWhfCc

3) Enter the following lines in the command line using the
`oauth_token` and the `oauth_verifier` from *step 2* to get an access
token:

    OAUTH_TOKEN='5cfvsHirEdu7zRx93WcAZCrMlBj9nmhY87POuVvuQY&'
    OAUTH_VERIFIER='O5UFQ7wfqX18P4vS6wn6igemItChLeiYYFWFWhfCc'
    ACCESS_TOKEN_URL="http://localhost:8080/get_access_token?oauth_token=$OAUTH_TOKEN&oauth_verifier=$OAUTH_VERIFIER"
    curl -v -H "Accept: application/json" -X GET $ACCESS_TOKEN_URL

Result:
``` javascript
{
  "result": {
    "token": "630384097-B0BwxDFRh8zVsbAhjaN42rfRleo7j4tZwvlH7iwq",
    "secret": "xDcxmZja6qWbJt7eVF4Rk4PekoDgKzPNg2gTGq74iU",
    "user_id": "630384097",
    "screen_name": "erlymob3"
  },
  "version":"1.0"
}
```
This token is the one you'll be using to make requests to the Erlymob
HTTP API.


Accessing the Erlymob HTTP API
------------------------------

1) Get the list of mobs for your user by doing:

    TOKEN='630384097-B0BwxDFRh8zVsbAhjaN42rfRleo7j4tZwvlH7iwq'
    MOBS_URL="http://localhost:8080/mobs?token=$TOKEN"
    curl -v -H "Accept: application/json" -X GET $MOBS_URL

Result:
``` javascript
[
  {
    "id": 230304686060761088,
    "tweet": "@erlymob_server This is my second message today",
    "user": "erlymob3",
    "created": 1343743890,
    "where": {
      "latitude":null,
      "longitude":null
    },
    "when": 1343743890,
    "rsvps": 0,
    "going": false
  },
  {
    "id": 230303751351709696,
    "tweet": "@erlymob_server Do you read me?",
    "user": "erlymob3",
    "created": 1343743667,
    "where": {
      "latitude": null,
      "longitude": null
    },
    "when": 1343743667,
    "rsvps": 0,
    "going": false
  }
]
```
