#!/bin/bash
set -e

# Start FMK:
echo "stopping FMKe..."
_build/default/rel/fmke/bin/env stop

echo "successful FMKe shutdown."
