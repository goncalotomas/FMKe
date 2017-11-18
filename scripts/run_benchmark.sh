#!/bin/bash
set -e

reset="\033[0m"
red="\033[31m"
green="\033[32m"
yellow="\033[33m"
cyan="\033[36m"
white="\033[37m"

printf "$green> Setting up FMKe to benchmark $2...$reset\n"
./scripts/config/change_db.sh $2
./scripts/config/change_db_ports.sh $2
make rel
./scripts/start_data_store.sh $2
./scripts/start_fmke.sh

# TODO make fmke node reference extensible
# TODO make 'normal' mode opcional
# Fill database with testdata:
if [[ $1 = 'short' ]]; then
  echo "$green> Populating $2 via FMKe using short benchmark configuration...$reset\n"
  ./scripts/populate_fmke.escript $2 benchmark_short.config 'fmke@127.0.0.1'
elif [[ $1 = 'normal' ]]; then
  echo "$green> Populating $2 via FMKe using standard benchmark configuration...$reset\n"
  ./scripts/populate_fmke.escript $2 benchmark_standard.config 'fmke@127.0.0.1'
fi

# Start benchmark
printf "$yellow> Starting benchmark...$reset\n"
if [[ $1 = 'short' ]]; then
  _build/test/lib/lasp_bench/_build/default/bin/lasp_bench config/benchmark_short.config
elif [[ $1 = 'normal' ]]; then
  _build/test/lib/lasp_bench/_build/default/bin/lasp_bench config/benchmark_standard.config
fi

if [ -s tests/current/error.log ]; then
    ./scripts/stop_fmke.sh
    ./scripts/stop_data_store.sh $2
    printf "$red> Fatal error: benchmark exited prematurely with errors.$reset\n"
    exit 3
fi

./scripts/stop_fmke.sh
./scripts/stop_data_store.sh $2
printf "$green> Benchmark successful! You should now run 'make bench-results' to generate the graphs for the benchmark.$reset\n"
