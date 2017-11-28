#!/bin/bash
set -e
echo "stopping $1..."
if [[ $1 = "antidote" || $1 = "antidote_norm" ]]; then
    docker stop antidote > /dev/null
    docker rm antidote
elif [[ $1 = "riak" || $1 = "riak_norm" ]]; then
    docker stop riak > /dev/null
    docker rm riak
else
    docker stop $1 > /dev/null
    echo "removing docker image to eliminate persistent storage..."
    docker rm $1
fi
echo "$1 stopped, docker image removed"
