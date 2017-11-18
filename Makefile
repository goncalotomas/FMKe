REBAR = rebar3
BENCH=_build/test/lib/lasp_bench

all: compile rel

attach:
	./_build/default/rel/fmke/bin/env attach

bench: compile
	./travis.sh bench short antidote
	./travis.sh bench short antidote_norm
	./travis.sh bench short redis
	./travis.sh bench short riak

bench-antidote: rel
	./travis.sh bench normal antidote

bench-antidote-norm: rel
	./travis.sh bench normal antidote_norm

bench-results:
	Rscript --vanilla _build/test/lib/lasp_bench/priv/summary.r -i tests/current

bench-antidote-norm: rel
	./travis.sh bench antidote_norm

bench-redis: rel
	./travis.sh bench normal redis

bench-riak: rel
	./travis.sh bench normal riak

compile:
	${REBAR} as test compile
	cd ./_build/test/lib/lasp_bench && rebar3 escriptize

console: rel
	./_build/default/rel/fmke/bin/env console

ct: all
	./travis.sh ct antidote
	./travis.sh ct antidote_norm
	./travis.sh ct redis
	./travis.sh ct riak

ct-antidote: rel
	./travis.sh ct antidote

ct-antidote-norm: rel
	./travis.sh ct antidote_norm

ct-redis: rel
	./travis.sh ct redis

ct-riak: rel
	./travis.sh ct riak

dialyzer:
	${REBAR} dialyzer

eunit: compile
	./travis.sh test antidote
	./travis.sh test antidote_norm
	./travis.sh test redis
	./travis.sh test riak

eunit-antidote: compile
	./travis.sh test antidote

eunit-antidote-norm: compile
	./travis.sh test antidote_norm

eunit-redis: compile
	./travis.sh test redis

eunit-riak: compile
	./travis.sh test riak

kv_driver_test:
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/ct/kv_driver_SUITE

populate: compile
	./scripts/populate_fmke.erl "antidote" "../config/fmke_travis.config" "fmke@127.0.0.1"

rel: relclean
	rm -rf _build/default/rel/
	${REBAR} release -n fmke

relclean:
	rm -rf _build/default/rel

select-antidote:
	./scripts/config/change_db.sh antidote

select-redis:
	./scripts/config/change_db.sh redis

select-riak:
	./scripts/config/change_db.sh riak

shell:
	${REBAR} shell --apps fmke --name fmke@127.0.0.1 --setcookie fmke

shell-antidote: rel
	./scripts/start_data_store.sh antidote
	./_build/default/rel/fmke/bin/env console

shell-redis: rel
	./scripts/start_data_store.sh redis
	./_build/default/rel/fmke/bin/env console

shell-riak: rel
	./scripts/start_data_store.sh riak
	./_build/default/rel/fmke/bin/env console

start:
	./scripts/start_fmke.sh

start-antidote: select-antidote
	./scripts/start_data_store.sh antidote

start-redis: select-redis
	./scripts/start_data_store.sh redis

start-riak: select-riak
	./scripts/start_data_store.sh riak

stop:
	./scripts/stop_fmke.sh

stop-antidote:
	./scripts/stop_data_store.sh antidote

stop-redis:
	./scripts/stop_data_store.sh redis

stop-riak:
	./scripts/stop_data_store.sh riak

test-multiple-releases:
	rm -rf _build/default/rel
	./scripts/start_data_store.sh antidote
	./scripts/config/change_http_port.sh 9090
	./scripts/config/change_db.sh antidote
	./scripts/config/change_db_ports.sh antidote
	${REBAR} release -n fmke
	./_build/default/rel/fmke/bin/env start
	sleep 10
	./scripts/config/change_http_port.sh 9190
	${REBAR} release -n fmke_test
	./_build/default/rel/fmke_test/bin/env_test start
	sleep 10
	./scripts/populate_fmke.escript "antidote" "../config/benchmark_short.config" "fmke_test@127.0.0.1"
	_build/test/lib/lasp_bench/_build/default/bin/lasp_bench config/benchmark_short.config
	./scripts/config/change_http_port.sh 9090
	./_build/default/rel/fmke/bin/env stop
	./_build/default/rel/fmke_test/bin/env_test stop
	./scripts/stop_data_store.sh antidote
