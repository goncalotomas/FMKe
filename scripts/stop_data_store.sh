#!/bin/bash
set -e

echo "stopping $1..."
docker stop $1 > /dev/null
echo "removing docker image to eliminate persistent storage..."
docker rm $1
echo "$1 stoped, docker image removed"
