REBAR = $(shell pwd)/rebar3
BENCH=_build/default/lib/basho_bench

all: compile rel

compile:
	${REBAR} compile
	./scripts/compile_basho_bench.sh

rel:
	rm -rf _build/default/rel/
	${REBAR} release -n fmke

relclean:
	rm -rf _build/default/rel

populate: compile
	./scripts/populate_fmke.erl "antidote" "../config/fmke_travis.config" "fmk@127.0.0.1"

bench: compile
	${BENCH}/_build/default/bin/basho_bench test/fmkclient.config
	-Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current

console: rel
	./_build/default/rel/fmke/bin/env console

travis:
	#./travis.sh test fmk
	./travis.sh test antidote
	./travis.sh bench antidote

dialyzer:
	-rm _build/default/lib/fmke/ebin/basho_bench_driver_fmkclient.beam
	${REBAR} dialyzer

kv_driver_test:
	 ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/ct/kv_driver_SUITE
