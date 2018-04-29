REBAR=rebar3
BENCH=_build/test/lib/lasp_bench

all: compile rel

attach:
	./_build/default/rel/fmke/bin/env attach

bench: compile
	./travis.sh bench short antidote
	./travis.sh bench short antidote_norm
	./travis.sh bench short redis
	./travis.sh bench short riak
	./travis.sh bench short riak_norm

bench-antidote: rel
	./travis.sh bench normal antidote

bench-antidote-norm: rel
	./travis.sh bench normal antidote_norm

bench-redis: rel
	./travis.sh bench normal redis

bench-results:
	Rscript --vanilla _build/test/lib/lasp_bench/priv/summary.r -i tests/current

bench-riak: rel
	./travis.sh bench normal riak

bench-riak-norm: rel
	./travis.sh bench normal riak_norm

compile:
	${REBAR} as test compile

console: rel
	./_build/default/rel/fmke/bin/env console

dialyzer:
	${REBAR} dialyzer

kv_driver_test:
	ct_run -pa ./_build/default/lib/*/ebin -logdir logs -suite test/ct/kv_driver_SUITE

lint:
	rebar3 as lint lint

populate: compile
	./scripts/populate_fmke.erl "antidote" "../config/fmke_travis.config" "fmke@127.0.0.1"

rel: relclean
	rm -rf _build/default/rel/
	${REBAR} release -n fmke

relclean:
	rm -rf _build/default/rel

select-antidote:
	./scripts/config/change_db.sh antidote

select-antidote-norm:
	./scripts/config/change_db.sh antidote_norm

select-redis:
	./scripts/config/change_db.sh redis

select-riak:
	./scripts/config/change_db.sh riak

select-riak-norm:
	./scripts/config/change_db.sh riak_norm

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

start: rel
	./scripts/start_fmke.sh

start-antidote: select-antidote
	./scripts/start_data_store.sh antidote

start-antidote-norm: select-antidote-norm
	./scripts/start_data_store.sh antidote

start-redis: select-redis
	./scripts/start_data_store.sh redis

start-riak: select-riak
	./scripts/start_data_store.sh riak

start-riak-norm: select-riak-norm
	./scripts/start_data_store.sh riak

stop:
	./scripts/stop_fmke.sh

stop-antidote:
	./scripts/stop_data_store.sh antidote

stop-redis:
	./scripts/stop_data_store.sh redis

stop-riak:
	./scripts/stop_data_store.sh riak

test: all
	rebar3 eunit
	rebar3 ct

xref:
	rebar3 xref
