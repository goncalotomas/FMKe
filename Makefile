REBAR=rebar3
BENCH=_build/test/lib/lasp_bench

all: compile rel

attach:
	./_build/default/rel/fmke/bin/env attach

bench-results:
	Rscript --vanilla _build/test/lib/lasp_bench/priv/summary.r -i tests/current

compile:
	${REBAR} as test compile

console: rel
	./_build/default/rel/fmke/bin/env console

coverage: eunit ct
	${REBAR} cover --verbose

ct:
	${REBAR} ct --suite fmke_antidote_transactions_SUITE.erl --cover --cover_export_name=antidote_txn_check
	${REBAR} ct --suite fmke_db_conn_manager_SUITE.erl --cover --cover_export_name=db_conn_manager
	${REBAR} ct --suite fmke_unstable_db_conn_SUITE.erl --cover --cover_export_name=unstable_db_conn
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/antidote_non_nested_data_model.config --cover --cover_export_name=core_antidote_non_nested_opt
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/cassandra_non_nested_data_model.config --cover --cover_export_name=core_cassandra_non_nested_opt
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/ets_nested_data_model.config --cover --cover_export_name=core_ets_nested
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/ets_non_nested_data_model.config --cover --cover_export_name=core_ets_non_nested
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/redis_cluster_non_nested_data_model.config --cover --cover_export_name=core_redis_cluster_non_nested_opt
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/redis_crdb_non_nested_data_model.config --cover --cover_export_name=core_redis_crdb_non_nested_opt
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/riak_non_nested_data_model.config --cover --cover_export_name=core_riak_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/antidote_non_nested_data_model.config --cover --cover_export_name=http_antidote_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/cassandra_non_nested_data_model.config --cover --cover_export_name=http_cassandra_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/ets_nested_data_model.config --cover --cover_export_name=http_ets_nested
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/ets_non_nested_data_model.config --cover --cover_export_name=http_ets_non_nested
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/redis_cluster_non_nested_data_model.config --cover --cover_export_name=http_redis_cluster_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/redis_crdb_non_nested_data_model.config --cover --cover_export_name=http_redis_crdb_non_nested_opt
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/riak_non_nested_data_model.config --cover --cover_export_name=http_riak_non_nested_opt

ct-antidote:
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/antidote_non_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/antidote_non_nested_data_model.config

ct-cassandra:
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/cassandra_non_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/cassandra_non_nested_data_model.config

ct-ets:
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/ets_nested_data_model.config
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/ets_non_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/ets_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/ets_non_nested_data_model.config

ct-redis-cluster:
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/redis_cluster_non_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/redis_cluster_non_nested_data_model.config

ct-redis:
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/redis_crdb_non_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/redis_crdb_non_nested_data_model.config

ct-riak:
	${REBAR} ct --suite fmke_core_unit_test_SUITE.erl --config test/fmke_configs/riak_non_nested_data_model.config
	${REBAR} ct --suite fmke_http_api_SUITE.erl --config test/fmke_configs/riak_non_nested_data_model.config

dialyzer:
	${REBAR} dialyzer

eunit:
	${REBAR} eunit

lint:
	rebar3 as lint lint

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

test: all eunit ct

xref:
	rebar3 xref
