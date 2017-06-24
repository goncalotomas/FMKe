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

DBTYPE_DRIVER=""
DBDRIVER_MODULE=""

echo "Trying to select ${TARGETDB} as target..."

if [ "$1" == "antidote" ]; then
    DBTYPE_DRIVER="fmke_kv_driver"
    DBDRIVER_MODULE="fmke_db_driver_antidote"
elif [ "$1" == "riak" ]; then
    DBTYPE_DRIVER="fmke_kv_driver"
    DBDRIVER_MODULE="fmke_db_driver_riak_kv"
elif [ "$1" == "redis" ]; then
    DBTYPE_DRIVER="fmke_kv_driver"
    DBDRIVER_MODULE="fmke_db_driver_redis"
else
    echo "Error: invalid or unsupported data store name"
    show_supported_dbs
    echo "error"
    exit 3
fi

echo "Changing driver type to ${DBTYPE_DRIVER}..."
echo "Changing implementation driver to ${DBDRIVER_MODULE}..."

if [ "$?" -ne 0 ]; then
    echo "Error: error changing driver type"
    echo "error"
    exit 3
fi

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Making changes in the header file using Linux commands..."
    sed -i -e "2s/.*/-define(DB_DRIVER, $DBTYPE_DRIVER)\./" ./include/fmk.hrl
    sed -i -e "3s/.*/-define(KV_IMPLEMENTATION, $DBDRIVER_MODULE)\./" ./include/fmk.hrl
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Making changes in the header file using OS X commands..."
    sed -i '' -e "2s/.*/-define(DB_DRIVER, $DBTYPE_DRIVER)\./" ./include/fmk.hrl
    sed -i '' -e "3s/.*/-define(KV_IMPLEMENTATION, $DBDRIVER_MODULE)\./" ./include/fmk.hrl
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    echo "Making changes in the header file using FreeBSD commands..."
    sed -i -e "2s/.*/-define(DB_DRIVER, $DBTYPE_DRIVER)\./" ./include/fmk.hrl
    sed -i -e "3s/.*/-define(KV_IMPLEMENTATION, $DBDRIVER_MODULE)\./" ./include/fmk.hrl
fi

if [ "$?" -ne 0 ]; then
    echo "Error: error changing driver implementation"
    echo "error"
    exit 3
fi
echo "success"
