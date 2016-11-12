REBAR=rebar3
BENCH=_build/default/lib/basho_bench
EBIN=_build/default/lib/fmk/ebin
CLIENT=basho_bench_driver_fmkclient

all: compile rel

compile:
	${REBAR} compile; \
	cd ${BENCH}; make all; \
	cd -; \
	cp ${BENCH}/include/basho_bench.hrl ./include/ ; \
	erlc test/${CLIENT}.erl; \
	mv ${CLIENT}.beam ${EBIN}/

rel:
	${REBAR} release

relclean:
	rm -rf _build

bench: compile
	${BENCH}/_build/default/bin/basho_bench test/fmkclient.config; \
	Rscript --vanilla ${BENCH}/priv/summary.r -i tests/current

console: rel
	./_build/default/rel/fmk/bin/fmk console
