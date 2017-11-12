#!/bin/bash
set -e

if [[ $1 = "antidote" || $1 = "antidote_norm" ]]; then
    echo "stopping $1..."
    docker stop antidote > /dev/null
    echo "removing docker image to eliminate persistent storage..."
    docker rm antidote
else
    echo "stopping $1..."
    docker stop $1 > /dev/null
    echo "removing docker image to eliminate persistent storage..."
    docker rm $1
fi
echo "$1 stoped, docker image removed"
