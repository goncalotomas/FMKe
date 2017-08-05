#!/bin/bash
set -e
echo "Running small FMKe benchmark tests with $1 back end..."
./scripts/config/set_target_data_store.sh $1
./scripts/start_data_store.sh $1
./scripts/start_fmke.sh

# Fill database with testdata:
echo "populating $1 via FMKe..."
./scripts/populate_fmke.escript $1 'config/fmke_travis.config' 'fmke@127.0.0.1'

# Start benchmark
echo "starting benchmark..."
_build/default/lib/basho_bench/_build/default/bin/basho_bench config/fmke_travis.config

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
