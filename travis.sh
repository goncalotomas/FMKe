#!/bin/bash
set -e

if [ $1 = "bench" ]; then
    ./scripts/run_benchmark.sh $2 $3
else
    echo "fatal: unknown operation."
    exit 1
fi
