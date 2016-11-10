REBAR2=rebar
REBAR3=rebar3
RIAK_PB=_build/default/lib/riak_pb
KATJA=_build/default/lib/katja
BENCH=_build/default/lib/basho_bench
BENCH_HRL=basho_bench
EBIN=_build/default/lib/fmk/ebin
CLIENT=basho_bench_driver_fmkclient

all: compile rel

compile:
	./${REBAR3} compile; \
	cp ${RIAK_PB}/rebar ${KATJA}/ ; \
	cd ${RIAK_PB}; ${REBAR2} get-deps; ${REBAR2} compile; \
	cd - ; \
	cd ${KATJA}; ${REBAR2} get-deps; ${REBAR2} compile; \
	cd - ; \
	cd ${BENCH}; make all; \
	cd - ; \
	./${REBAR3} compile; \
	cp ${BENCH}/include/${BENCH_HRL}.hrl ./include/ ; \
	erlc test/${CLIENT}.erl; \
	mv ${CLIENT}.beam ${EBIN}/

rel:
	./${REBAR3} release

relclean:
	rm -rf _build/

bench:
	${BENCH}/basho_bench test/fmkclient.config; \
	Rscript --vanilla _build/default/lib/basho_bench/priv/summary.r -i tests/current
