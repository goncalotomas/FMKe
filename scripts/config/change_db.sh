#!/bin/bash
set -e

show_supported_dbs()
{
  echo "FMKe supports the following data stores:"
  for value in "antidote" "antidote_norm" "riak" "riak_norm" "redis" ; do
      echo "-$value"
  done
}

if [ "$#" -lt 1 ]; then
    echo "Error: no data store name supplied"
    show_supported_dbs
    echo "error"
    exit 1
fi

TARGETDB=$1

./scripts/config/set_param.sh "target_database" $TARGETDB
if [[ $? -eq 0 ]]; then
  echo "success"
fi
