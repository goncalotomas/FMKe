REBAR = $(shell pwd)/rebar3
BENCH=_build/default/lib/basho_bench

all: compile rel

attach:
	./_build/default/rel/fmk/bin/env attach

bench: compile
	./travis.sh bench antidote
	./travis.sh bench riak

bench-antidote: rel-antidote
	./travis.sh bench antidote

bench-riak: rel-riak
	./travis.sh bench riak

compile:
	${REBAR} as test,riak,antidote compile
	./scripts/compile_basho_bench.sh

console: rel
	./_build/default/rel/fmk/bin/env console

console-riak: rel-riak
	./scripts/start_data_store.sh riak
	./_build/default/rel/fmk/bin/env console

console-antidote: rel-antidote
	./scripts/start_data_store.sh antidote
	./_build/default/rel/fmk/bin/env console

dialyzer:
	-rm _build/default/lib/fmk/ebin/basho_bench_driver_fmkclient.beam
	${REBAR} dialyzer

kv_driver_test:
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/ct/kv_driver_SUITE

populate:
	./scripts/populate_fmke_travis.erl 1 fmk@127.0.0.1

rel: relclean
	${REBAR} as test,riak,antidote release -n fmk

rel-antidote:
	${REBAR} as antidote release

rel-riak:
	${REBAR} as riak release

relclean:
	rm -rf _build/default/rel

select-antidote:
	./scripts/config/set_target_data_store.sh antidote

select-riak:
	./scripts/config/set_target_data_store.sh riak

start:
	./scripts/start_fmke.sh

start-antidote: select-antidote
	./scripts/start_data_store.sh antidote

start-riak: select-riak
	./scripts/start_data_store.sh riak

stop:
	./scripts/stop_fmke.sh

stop-antidote:
	./scripts/stop_data_store.sh antidote

stop-riak:
	./scripts/stop_data_store.sh riak

test: compile
	./travis.sh test antidote
	./travis.sh test riak

test-antidote:
	./travis.sh test antidote

test-riak:
	./travis.sh test riak
