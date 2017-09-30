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

CURRENT_DB=$(
  grep '{target_database, .*}' config/fmke.config | awk '{print $2;}' | sed 's/}.$//'
)

if [[ "$TARGETDB" == "$CURRENT_DB" ]]; then
    echo "No changes needed, target database already set to $TARGETDB."
    echo "success"
    exit 0
fi

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Making changes in the header file using Linux commands..."
    sed -i -e "s/{target_database, .*}\./{target_database, ${TARGETDB}}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Making changes in the header file using OS X commands..."
    sed -i '' -e "s/{target_database, .*}\./{target_database, ${TARGETDB}}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    echo "Making changes in the header file using FreeBSD commands..."
    sed -i -e "s/{target_database, .*}\./{target_database, ${TARGETDB}}\./g" ./config/fmke.config
fi

if [ "$?" -ne 0 ]; then
    echo "Error: error changing target database"
    echo "error"
    exit 3
fi
echo "success"
