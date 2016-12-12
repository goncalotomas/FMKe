REBAR = $(shell pwd)/rebar3
BENCH=_build/default/lib/basho_bench
EBIN=_build/default/lib/fmk/ebin
CLIENT=basho_bench_driver_fmkclient

all: compile rel

compile:
	${REBAR} compile
	cd ${BENCH}; make all; cd -
	cp ${BENCH}/include/basho_bench.hrl ./include/
	erlc test/${CLIENT}.erl
	mv ${CLIENT}.beam ${EBIN}/

rel:
	rm -rf _build/default/rel/
	${REBAR} release -n fmk

relclean:
	rm -rf _build/default/rel

populate: compile
	./test/fmk_setup_script.erl 1 fmk@127.0.0.1

bench: compile
	${BENCH}/_build/default/bin/basho_bench test/fmkclient.config
	-Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current

console: rel
	./_build/default/rel/fmk/bin/env console

dialyzer:
	-rm _build/default/lib/fmk/ebin/basho_bench_driver_fmkclient.beam
	${REBAR} dialyzer
