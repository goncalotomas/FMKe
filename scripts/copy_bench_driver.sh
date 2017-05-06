#!/bin/bash
# copy Basho Bench driver into correct folder before building
cp test/basho_bench_driver_fmkclient.erl _build/default/lib/basho_bench/src
cd _build/default/lib/basho_bench; make all; cd -
