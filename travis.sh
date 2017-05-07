#!/bin/bash
set -e

pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

ANTIDOTE_FOLDER=""

if [ $1 = "test" ]; then
    # Assume that Antidote is already running
    if [ $2 = "fmke" ]; then
        echo "running FMKe tests..."
        # TODO add some unit tests...
        # TODO maybe run dialyzer...
        echo "done"
    elif [ $2 = "antidote"]; then
        echo "running FMKe test with antidote..."
        ./scripts/start_data_store.sh antidote
        ./scripts/start_fmke.sh
        ./rebar3 eunit
        echo "done"
    fi
elif [ $1 = "bench" ]; then
    if [ $2 = "redis" ]; then
        echo "running small benchmark with Redis..."
        echo "fatal: not implemented"
        exit 1
    elif [ $2 = "antidote" ]; then
        echo "running small benchmark with antidote..."
        ./scripts/start_data_store.sh antidote

        # compile FMK:
        echo "compiling FMKe..."
        make all



        # Fill database with testdata:
        echo "populating antidote via FMKe..."
        ./scripts/populate_fmke_travis.erl 1 'fmk@127.0.0.1'

        # Start benchmark
        echo "starting benchmark..."
        _build/default/lib/basho_bench/_build/default/bin/basho_bench test/fmke_travis.config

        if [ -s tests/current/error.log ]; then
            # start existing docker container:
            echo "fatal: benchmark exited prematurely with errors"
            exit 3
        fi

        echo "benchmark complete."

        # Stop FMK
        echo "stopping FMKe..."
        _build/default/rel/fmk/bin/env stop > /dev/null

        echo "shutting down antidote..."
        docker stop antidote > /dev/null

        echo "done"
    elif [ $2 = "riak" ]; then
        echo "running small benchmark with riak..."
        echo "fatal: not implemented"
        exit 1
    else
        echo "fatal: second argument not recognised. Cannot proceed."
        exit 1
    fi

else
    echo "fatal: first argument not recognised. Cannot proceed."
    exit 1
fi
