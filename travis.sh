#!/bin/bash
set -e

if [ $1 = "test" ]; then
    if [ $2 = "fmke" ]; then
        echo "running FMKe tests..."
        # TODO maybe perform unit tests with ets or other built in erlang utility...
        # TODO maybe run dialyzer...
        echo "done"
    else
        ./scripts/run_fmke_operations.sh $2
    fi
elif [ $1 = "bench" ]; then
        ./scripts/run_benchmark_short.sh $2
else
    echo "fatal: unknown operation."
    exit 1
fi
