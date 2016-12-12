#!/bin/bash
set -e

pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

ANTIDOTE_FOLDER=""

if [ "$#" -eq 0 ]; then
    # Assume that Antidote is already running
    if nc -z localhost 8087; then
        echo "Antidote is running"
    else
        echo "Antidote is not running on PB port 8087"
        echo "Start Antidote manually, or start the script with a different option."
        exit 1
    fi
elif [ $1 = "docker" ]; then
    # load antidote from docker:
    if docker inspect antidote; then
        # start existing docker container:
        docker start antidote
    else
        # setup new antidote docker container:
        docker run -d --name antidote -p "4368:4368" -p "8085:8085" -p "8087:8087" -p "8099:8099" -p "9100:9100" -e NODE_NAME=antidote@127.0.0.1 peterzel/antidote_map_rr
    fi
elif [ $1 = "github" ]; then
    # clone antidote from github
    ANTIDOTE_FOLDER=_build/antidote
    if cd $ANTIDOTE_FOLDER; then
        echo "Using antidote clone in $ANTIDOTE_FOLDER"
        # already cloned
    else
        git clone https://github.com/SyncFree/antidote $ANTIDOTE_FOLDER
        cd $ANTIDOTE_FOLDER
    fi
    # use fixed branch
    git checkout crdt-lib-map_rr
    cd $SCRIPTPATH
else
    # use provided path to antidote
    ANTIDOTE_FOLDER=$1
fi

if [ -n "$ANTIDOTE_FOLDER" ]; then
    cd $ANTIDOTE_FOLDER
    # clean last release
    rm -rf _build/default/rel/
    make rel
    _build/default/rel/antidote/bin/env start
fi

cd $SCRIPTPATH

# compile FMK:
echo "Compiling FMK"
make all

# Start FMK:
echo "Starting FMK"
_build/default/rel/fmk/bin/env start

# wait for FMK to start (TODO better way?)
echo "Waiting for FMK to start"
sleep 2

# Fill database with testdata:
echo "Filling Antidote with testdata"
./test/fmk_setup_script.erl 1 'fmk@127.0.0.1' || true

# Start benchmark
echo "Starting Benchmark"
make bench
echo "Benchmark done"

# Stop FMK
echo "Stopping FMK"
_build/default/rel/fmk/bin/env stop

if [ -n "$ANTIDOTE_FOLDER" ]; then
    # Stop Antidote
    echo "Stopping Antidote"
    cd $ANTIDOTE_FOLDER
    _build/default/rel/antidote/bin/env stop
    cd $SCRIPTPATH
fi

# Display the summary
cat tests/current/summary.csv
ERRORS=`awk -F "\"*, \"*" '{print $5}' tests/current/summary.csv | sed '2q;d'`
SUCCESS=`awk -F "\"*, \"*" '{print $4}' tests/current/summary.csv | sed '2q;d'`
echo "in the first 10 seconds $SUCCESS successful requests were done and $ERRORS failed."
if [ "$ERRORS" -gt 100 ]; then
    echo "too many errors"
    exit 1
fi

if [ "$SUCCESS" -lt 500 ]; then
    echo "not enough successful requests"
    exit 1
fi