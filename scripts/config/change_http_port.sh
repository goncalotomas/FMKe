#!/bin/bash
set -e

CURRENT_PORT=$(
  grep '{http_port, .*}' config/fmke.config | awk '{print $2;}' | sed 's/}.$//'
)

if [[ "$1" == "$CURRENT_PORT" ]]; then
    echo "No changes needed, HTTP port already set to $1."
    echo "success"
    exit 0
fi

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Making changes in the header file using Linux commands..."
    sed -i -e "s/{http_port, .*}\./{http_port, $1}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Making changes in the header file using OS X commands..."
    sed -i '' -e "s/{http_port, .*}\./{http_port, $1}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    echo "Making changes in the header file using FreeBSD commands..."
    sed -i -e "s/{http_port, .*}\./{http_port, $1}\./g" ./config/fmke.config
fi

if [ "$?" -ne 0 ]; then
    echo "Error: error changing HTTP port"
    echo "error"
    exit 3
fi
echo "success"
