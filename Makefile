REBAR = $(shell pwd)/rebar3
BENCH=_build/test/lib/lasp_bench

all: compile compilebench rel reltest

compile:
	${REBAR} as test compile
	cd ./_build/test/lib/lasp_bench; ./rebar3 escriptize; cd -

rel:
	rm -rf _build/default/rel/
	${REBAR} release -n fmke

reltest:
	rm -rf _build/test/rel/
	${REBAR} as test release -n fmke

relclean:
	rm -rf _build/default/rel

populate: compile
	./scripts/populate_fmke.erl "antidote" "../config/fmke_travis.config" "fmk@127.0.0.1"

bench: compile
	${BENCH}/_build/default/bin/basho_bench test/fmkclient.config
	-Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current

console: rel
	./_build/default/rel/fmke/bin/env console

compilebench: compile
	cd ./_build/test/lib/lasp_bench; ./rebar3 escriptize

ct: all
	./scripts/config/set_target_data_store.sh antidote
	./scripts/start_data_store.sh antidote
	./scripts/start_fmke.sh
	./rebar3 ct
	./scripts/stop_fmke.sh
	./scripts/stop_data_store.sh antidote

travis:
	#./travis.sh test fmk
	./travis.sh test antidote
	./travis.sh bench antidote

dialyzer:
	${REBAR} dialyzer

kv_driver_test:
	 ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/ct/kv_driver_SUITE
