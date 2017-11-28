#!/bin/bash
set -e

if [[ $1 == "antidote" || $1 == "antidote_norm" ]]; then
  ./scripts/config/set_param.sh "database_ports" [8087]
elif [[ $1 == "redis" ]]; then
  ./scripts/config/set_param.sh "database_ports" [6379]
elif [[ $1 == "riak" || $1 == "riak_norm" ]]; then
  ./scripts/config/set_param.sh "database_ports" [8087]
else
  ./scripts/config/set_param.sh "database_ports" $1
fi
