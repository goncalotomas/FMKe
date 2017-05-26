#!/bin/sh
set -e

declare -a kvs=("antidote" "riak" "redis")
declare -a sqldbs=()

show_supported_dbs()
{
  echo "FMKe supports the following data stores:"
  for i in ${kvs[@]}; do echo "-$i"; done
  for i in ${sqldbs[@]}; do echo "-$i"; done
}

select_driver()
{
    if [ "$1" == "antidote" ]; then
        DBTYPE_DRIVER="fmke_kv_driver"
        DBDRIVER_MODULE="antidote_kv_driver"
    elif [ "$1" == "riak" ]; then
        DBTYPE_DRIVER="fmke_kv_driver"
        DBDRIVER_MODULE="riak_kv_driver"
    elif [ "$1" == "redis" ]; then
        DBTYPE_DRIVER="fmke_kv_driver"
        DBDRIVER_MODULE="redis_kv_driver"
    fi
}

if [ "$#" -ne 1 ]; then
    echo "Error: no data store name supplied"
    show_supported_dbs
    echo "error"
    exit 1
fi

TARGETDB=$1

if [[ ! " ${kvs[@]} " =~ " ${TARGETDB} " && ! " ${sqldbs[@]} " =~ " ${TARGETDB}" ]]; then
    echo "Error: invalid or unsupported data store name"
    show_supported_dbs
    echo "error"
    exit 2
fi

DBTYPE_DRIVER=""
DBDRIVER_MODULE=""

# TARGETDB contains a valid data store name
if [[ " ${kvs[@]} " =~ " ${TARGETDB} " ]]; then
    echo "Trying to select ${TARGETDB} as target..."
    select_driver ${TARGETDB}
    echo "Changing driver type to ${DBTYPE_DRIVER}..."
    sed -i '' -e "s/-define(DB_DRIVER,.*/\-define(DB_DRIVER, $DBTYPE_DRIVER)\./g" include/fmk.hrl
    if [ "$?" -ne 0 ]; then
        echo "Error: error changing driver type"
        echo "error"
        exit 3
    fi
    echo "Changing implementation driver to ${DBDRIVER_MODULE}..."
    sed -i '' -e "s/-define(KV_IMPLEMENTATION,.*/\-define(KV_IMPLEMENTATION, $DBDRIVER_MODULE)\./g" include/fmk.hrl
    if [ "$?" -ne 0 ]; then
        echo "Error: error changing driver implementation"
        echo "error"
        exit 3
    fi
    echo "success"
fi

#sed -i '' -e "s/-define(KV_IMPLEMENTATION,.*/\-define(KV_IMPLEMENTATION,something)\./g" include/fmk.hrl
