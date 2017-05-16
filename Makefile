REBAR = $(shell pwd)/rebar3
BENCH=_build/default/lib/basho_bench

all: compile rel

compile:
	${REBAR} compile
	./scripts/compile_basho_bench.sh

rel:
	rm -rf _build/default/rel/
	${REBAR} release -n fmk

relclean:
	rm -rf _build/default/rel

populate: compile
	./scripts/fmk_setup_script.erl 1 fmk@127.0.0.1

bench: compile
	${BENCH}/_build/default/bin/basho_bench test/fmkclient.config
	-Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current

console: rel
	./_build/default/rel/fmk/bin/env console

travis:
	#./travis.sh test fmk
	./travis.sh test antidote
	./travis.sh bench antidote

dialyzer:
	-rm _build/default/lib/fmk/ebin/basho_bench_driver_fmkclient.beam
	${REBAR} dialyzer

kv_driver_test:
	 ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/ct/kv_driver_SUITE
