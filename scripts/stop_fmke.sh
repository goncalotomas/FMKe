#!/bin/bash
set -e

# Start FMK:
echo "stopping FMKe..."
_build/default/rel/fmk/bin/env stop

echo "successful FMKe shutdown."
