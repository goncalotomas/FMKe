BENCH=_build/test/lib/lasp_bench

all: compile rel

attach:
	./_build/default/rel/fmke/bin/env attach

bench-antidote: compile rel
	./scripts/configure -lt antidote
	./scripts/setup -lt antidote
	./scripts/bench -sp -b short
	./scripts/setup -lT -t antidote

bench-redis: compile rel
	./scripts/configure -lt redis
	./scripts/setup -lt redis
	./scripts/bench -sp -b short
	./scripts/setup -lT -t redis

bench-results:
	Rscript --vanilla _build/test/lib/lasp_bench/priv/summary.r -i tests/current

bench-riak: compile rel
	./scripts/configure -lt riak
	./scripts/setup -lt riak
	./scripts/bench -sp -b short
	./scripts/setup -lT -t riak

bench-test: bench-antidote bench-redis bench-riak

compile:
	rebar3 as test compile
	escript -s scripts/populate_fmke.escript
	cd ${BENCH} && make && cd -

console: rel
	./_build/default/rel/fmke/bin/env console

ct:
	rebar3 ct

dialyzer:
	rebar3 dialyzer

eunit:
	rebar3 eunit

lint:
	rebar3 as lint lint

rel: relclean
	rm -rf _build/default/rel/
	rebar3 release -n fmke

relclean:
	rm -rf _build/default/rel

select-antidote:
	./scripts/configure -lt antidote

select-redis:
	./scripts/configure -lt redis

select-riak:
	./scripts/configure -lt riak

shell:
	rebar3 shell --apps fmke --name fmke@127.0.0.1 --setcookie fmke

shell-antidote: rel
	./scripts/setup -lt antidote
	./_build/default/rel/fmke/bin/env console

shell-redis: rel
	./scripts/setup -lt redis
	./_build/default/rel/fmke/bin/env console

shell-riak: rel
	./scripts/setup -lt riak
	./_build/default/rel/fmke/bin/env console

start: rel
	./scripts/setup -lt fmke

start-antidote: select-antidote
	./scripts/setup -lt antidote

start-redis: select-redis
	./scripts/setup -lt redis

start-riak: select-riak
	./scripts/setup -lt riak

stop:
	./scripts/setup -lT -t fmke

stop-antidote:
	./scripts/setup -lT -t antidote

stop-redis:
	./scripts/setup -lT -t redis

stop-riak:
	./scripts/setup -lT -t riak

test: all lint xref dialyzer eunit ct bench-test

xref:
	rebar3 xref
