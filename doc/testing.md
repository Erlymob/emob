Testing from the command-line
=============================

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
