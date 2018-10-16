REBAR=rebar3
BENCH=_build/test/lib/lasp_bench

all: compile rel

attach:
	./_build/default/rel/fmke/bin/env attach

bench: compile
	./travis.sh bench normal antidote
	./travis.sh bench normal antidote_norm
	./travis.sh bench normal redis
	./travis.sh bench normal riak
	./travis.sh bench normal riak_norm
	./travis.sh bench normal riak_norm_mg

bench-short: compile
	./travis.sh bench short antidote
	./travis.sh bench short antidote_norm
	./travis.sh bench short redis
	./travis.sh bench short riak
	./travis.sh bench short riak_norm
	./travis.sh bench short riak_norm_mg

bench-short-antidote: rel
	./travis.sh bench short antidote

bench-short-antidote-norm: rel
	./travis.sh bench short antidote_norm

bench-short-redis: rel
	./travis.sh bench short redis

bench-short-riak: rel
	./travis.sh bench short riak

bench-short-riak-norm: rel
	./travis.sh bench short riak_norm

bench-short-riak-norm-mg: rel
	./travis.sh bench short riak_norm_mg

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

bench-riak-norm-mg: rel
	./travis.sh bench normal riak_norm_mg

compile:
	${REBAR} as test compile
	escript -s scripts/populate_fmke.escript

console: rel
	./_build/default/rel/fmke/bin/env console

coverage: eunit ct
	${REBAR} cover --verbose

ct:
	${REBAR} ct --cover
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/ets_nested_data_model.config --cover --cover_export_name=core_ets_nested
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/ets_non_nested_data_model.config --cover --cover_export_name=core_ets_non_nested
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/antidote_non_nested_data_model.config --cover --cover_export_name=core_antidote_non_nested_opt
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/riak_non_nested_data_model.config --cover --cover_export_name=core_riak_non_nested_opt
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/redis_non_nested_data_model.config --cover --cover_export_name=core_redis_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/ets_nested_data_model.config --cover --cover_export_name=http_ets_nested
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/ets_non_nested_data_model.config --cover --cover_export_name=http_ets_non_nested
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/antidote_non_nested_data_model.config --cover --cover_export_name=http_antidote_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/riak_non_nested_data_model.config --cover --cover_export_name=http_riak_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/redis_non_nested_data_model.config --cover --cover_export_name=http_redis_non_nested_opt

ct-riak-norm-mg: rel
	./travis.sh ct riak_norm_mg

dialyzer:
	${REBAR} dialyzer

eunit:
	${REBAR} eunit

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

select-riak-norm-mg:
	./scripts/config/change_db.sh riak_norm_mg

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

start-riak-norm-mg: select-riak-norm-mg
	./scripts/start_data_store.sh riak

stop:
	./scripts/stop_fmke.sh

stop-antidote:
	./scripts/stop_data_store.sh antidote

stop-redis:
	./scripts/stop_data_store.sh redis

stop-riak:
	./scripts/stop_data_store.sh riak

test: all eunit ct

xref:
	rebar3 xref
