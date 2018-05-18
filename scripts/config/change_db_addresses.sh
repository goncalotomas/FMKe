#!/bin/bash
set -e

if [[ $1 = "localhost" ]]; then
  ./scripts/config/set_param.sh "database_addresses" '["127.0.0.1"]'
else
  ./scripts/config/set_param.sh "database_addresses" "$1"
fi
