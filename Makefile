PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
.PHONY: all test clean

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

get-deps:
	@$(REBAR) get-deps

clean:
	@rm -rf deps/ ebin/*.beam logs/

dialyzer:
	@$(REBAR) dialyze
