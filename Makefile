.PHONY : compile test clean

REBAR=rebar3

compile:
	${REBAR} get-deps
	${REBAR} compile

test:
	${REBAR} eunit

clean:
	${REBAR} clean
