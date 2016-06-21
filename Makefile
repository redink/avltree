.PHONY: compile rel ct doc

all: compile

REBAR ?= $(shell which ./rebar 2>/dev/null || which rebar)

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

xref: compile
	$(REBAR) xref

eunit: compile
	$(REBAR) skip_deps=true eunit -v