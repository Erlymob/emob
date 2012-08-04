APPLICATION := emob

ERL := erl
EPATH := -pa ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit -pz deps/*/ebin

.PHONY: all compile deps doc clean depclean distclean dialyze test console test-console

all: compile

compile:
	@rebar compile

deps:
	@rebar get-deps

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

distclean:
	@rebar delete-deps

dialyze: compile
	@dialyzer -r ./apps/

test:
	ERL_FLAGS="-config app" rebar skip_deps=true ct

console:
	$(ERL) +K true +A 4 +P 65535 -sname $(APPLICATION) $(EPATH) -config app

start:
	$(ERL) +K true +A 4 +P 65535 -sname $(APPLICATION) $(EPATH) -config app -s emob

test-console: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH) -config app

