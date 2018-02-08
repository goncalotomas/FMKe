#!/bin/bash
set -e
echo "trying to load $1 docker image..."
if [[ $1 = "antidote" || $1 = "antidote_norm" ]]; then
    docker pull mweber/antidotedb
    if docker inspect antidote &> /dev/null; then
        set -e
        docker start antidote
    else
        set -e
        # setup new antidote docker container:
        docker run -d --name antidote -p "8087:8087" mweber/antidotedb
    fi
    sleep 15
    echo "antidote started."
elif [ $1 = "redis" ]; then
    docker pull redis
    set +e
    docker run -d --name redis -p "6379:6379" redis
    sleep 15
    echo "redis started."
elif [[ $1 = "riak" || $1 = "riak_norm" ]]; then
    docker pull goncalotomas/riak
    set +e
    if docker inspect riak &> /dev/null; then
        # start existing docker container:
        set -e
        docker start riak
    else
        set -e
        docker run -d --name riak -p "8087:8087" -p "8098:8098" -e NODE_NAME=riak@127.0.0.1 goncalotomas/riak
    fi
    sleep 15
    echo "riak started."
else
    echo "fatal: data store not recognised. Cannot proceed."
    exit 1
fi
