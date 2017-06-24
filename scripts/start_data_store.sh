#!/bin/bash
set -e
echo "trying to load $1 docker image..."
if [ $1 = "antidote" ]; then
    # load antidote from docker:
    # docker inspect returns != 0 when containers don't exist in the system
    set +e
    docker inspect antidote &> /dev/null
    if [ $? -eq 0 ]; then
        # start existing docker container:
        set -e
        docker start antidote
    else
        set -e
        # setup new antidote docker container:
        docker run -d --name antidote -p "4368:4368" -p "8085:8085" -p "8087:8087" -p "8099:8099" -p "9100:9100" -e NODE_NAME=antidote@127.0.0.1 mweber/antidotedb
    fi
    sleep 5
    echo "antidote started."
elif [ $1 = "redis" ]; then
    #TODO use redis docker image
    echo "fatal: not implemented"
    exit 2
elif [ $1 = "riak" ]; then
    #load riak from docker:
    #docker inspect returns != 0 when containers don't exist in the system
    set +e
    docker inspect antidote &> /dev/null
    if [ $? -eq 0 ]; then
        # start existing docker container:
        set -e
        docker start riak
    else
        set -e
        docker run -d --name riak -p "8087:8087" -p "8098:8098" -e NODE_NAME=riak@127.0.0.1 goncalotomas/riak
    fi
    sleep 5
    echo "riak started."
else
    echo "fatal: data store not recognised. Cannot proceed."
    exit 1
fi
