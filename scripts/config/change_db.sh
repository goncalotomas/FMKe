#!/bin/bash
set -e

show_supported_dbs()
{
  echo "FMKe supports the following data stores:"
  for value in "antidote" "riak" "redis" ; do
      echo "-$value"
  done
}

if [ "$#" -ne 1 ]; then
    echo "Error: no data store name supplied"
    show_supported_dbs
    echo "error"
    exit 1
fi

TARGETDB=$1

./scripts/config/set_param.sh "target_database" $TARGETDB
if [[ $TARGETDB = "antidote" ]]; then
    ./scripts/config/change_db_ports.sh "[8087]"
elif [[ $TARGETDB = "redis" ]]; then
    ./scripts/config/change_db_ports.sh "[6379]"
elif [[ $TARGETDB = "riak" ]]; then
    ./scripts/config/change_db_ports.sh "[8087]"
fi

echo "success"
