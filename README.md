Erlymob
=======

[Erlymob](http://erlymob.com) is a Twitter-based event aggregator. It
collects tweets in real time and distributes them to those who have the best
opportunity to attend. Flash mobs, local deals, activism - everyone can find
something social.

Erlymob was designed to run with the frontend and backend running in different
Erlang VMs and so far has only been tested with Ubuntu Linux and Erlang R15B.
To test it you'll need to start an Erlang VM with the
[emob](https://github.com/Erlymob/emob) project running (backend) and the
[emob_web](https://github.com/Erlymob/emob_web) project (frontend).
The instructions to build, deploy and run the system are the following:

*Build*:

``` text
cd emob
make deps
make
make console
```

*Set Up (only needs to be done once per fresh install)*:

``` erlang
emob:setup().
```

*Deploy*:

``` erlang
emob:start().
```

Posting a new mob
-----------------

To post a new mob, send a tweet to @erlymob_test (or the Twitter
screen name you have chosen) containing the text you want to post.
It will then be visible on your home screen in Erlymob.
