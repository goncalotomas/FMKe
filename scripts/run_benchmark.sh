#!/bin/bash
set -e
echo "Running FMKe benchmark against $2..."
./scripts/config/change_db.sh $2
./scripts/start_data_store.sh $2
make rel
./scripts/start_fmke.sh

# TODO make fmke node reference extensible
# Fill database with testdata:
echo "populating $2 via FMKe..."
if [[ $1 = 'short' ]]; then
  echo "Populating FMKe using short benchmark configuration..."
  ./scripts/populate_fmke.escript $2 'benchmark_short.config' 'fmke@127.0.0.1'
elif [[ $1 = 'normal' ]]; then
  echo "Populating FMKe using standard benchmark configuration..."
  ./scripts/populate_fmke.escript $2 'benchmark_standard.config' 'fmke@127.0.0.1'
fi

# Start benchmark
echo "Starting benchmark..."
if [[ $1 = 'short' ]]; then
  _build/test/lib/lasp_bench/_build/default/bin/lasp_bench config/benchmark_short.config
elif [[ $1 = 'normal' ]]; then
  _build/test/lib/lasp_bench/_build/default/bin/lasp_bench config/benchmark_standard.config
fi

if [ -s tests/current/error.log ]; then
    # start existing docker container:
    echo "fatal: benchmark exited prematurely with errors"
    exit 3
fi

echo "benchmark complete."
./scripts/stop_fmke.sh
./scripts/stop_data_store.sh $2
echo "done"
echo "done"
