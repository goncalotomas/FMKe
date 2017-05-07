#!/bin/bash
set -e

# Start FMK:
echo "starting FMKe..."
_build/default/rel/fmk/bin/env start

# wait some time for FMKe to start
echo "waiting for FMKe to start..."
sleep 10
echo "FMKe started."
