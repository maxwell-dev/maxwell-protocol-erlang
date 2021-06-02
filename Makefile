.PHONY : compile test clean

REBAR=rebar3

compile: get-deps
	${REBAR} compile

get-deps:
	${REBAR} get-deps

gen: get-deps
	${REBAR} compile
	bin/maxwell_protocol_gen.sh

test:
	${REBAR} eunit

clean:
	${REBAR} clean
