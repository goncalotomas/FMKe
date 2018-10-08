#!/bin/bash
set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

# args checking
if [[ $# -lt 2 ]]; then
    echo "Error: usage $0 <private_key_file> <IP_ADDR_LIST> ..."
    exit 2
fi;

if [[ ! -e $1 ]]; then
    echo "Error: $1: no such file"
    exit 2
fi;

# env
KEY_FILE_PATH=$1
KEY_FILE_NAME=$(basename $KEY_FILE_PATH)
BUILD_SCRIPT_PATH="./src/bin/worker_instance_build.sh"
BUILD_SCRIPT_REMOTE_PATH="~/worker_instance_build.sh"

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
IP_ADDR_FILE="./ip_addr_list.txt"
echo $IP_ADDR_LIST | tr " " "\n" > $IP_ADDR_FILE

SSH_USERNAME=ubuntu
SSH_OPTIONS="-i $KEY_FILE_PATH -o StrictHostKeyChecking=no"

# prepare the images for the build
for IP_ADDR in $IP_ADDR_LIST; do
    scp $SSH_OPTIONS $KEY_FILE_PATH     $SSH_USERNAME@$IP_ADDR:~/.ssh
    scp $SSH_OPTIONS $BUILD_SCRIPT_PATH $SSH_USERNAME@$IP_ADDR:$BUILD_SCRIPT_REMOTE_PATH
    scp $SSH_OPTIONS $IP_ADDR_FILE      $SSH_USERNAME@$IP_ADDR:~/
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod +x $BUILD_SCRIPT_REMOTE_PATH
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod 600 "~/.ssh/$KEY_FILE_NAME"
done

# build images
for IP_ADDR in $IP_ADDR_LIST; do
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR nohup $BUILD_SCRIPT_REMOTE_PATH &
done;

