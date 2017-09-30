#!/bin/bash
set -e

echo "compiling FMKe..."
rebar3 compile
# Start FMK:
echo "starting FMKe..."
_build/default/rel/fmke/bin/env start

# wait some time for FMKe to start
echo "waiting for FMKe to start..."
sleep 3
echo "FMKe started."
