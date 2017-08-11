#!/bin/bash
set -e
echo "Running small FMKe benchmark tests with $1 back end..."
./scripts/config/set_target_data_store.sh $1
./scripts/start_data_store.sh $1
./scripts/start_fmke.sh

# Fill database with testdata:
echo "populating $1 via FMKe..."
./scripts/populate_fmke.escript $1 'config/benchmark_short.config' 'fmke@127.0.0.1'

# Start benchmark
echo "starting benchmark..."
_build/test/lib/lasp_bench/_build/default/bin/lasp_bench config/benchmark_short.config

if [ -s tests/current/error.log ]; then
    # start existing docker container:
    echo "fatal: benchmark exited prematurely with errors"
    exit 3
fi

echo "benchmark complete."
./scripts/stop_fmke.sh
./scripts/stop_data_store.sh $1
echo "done"
echo "done"
