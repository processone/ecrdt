REBAR=./rebar

all: src

src:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) skip_deps=true -v eunit

.PHONY: clean src all test
