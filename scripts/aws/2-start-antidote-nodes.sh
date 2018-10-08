#!/bin/bash
# author goncalotomas
# This script starts N replicas of antidote on previously configured remote vms.
# You should pass in a list of (public) IP addresses as arguments to the script,
# as well as the following environment variables:
# PRIVATEKEY: used to ssh into the amazon virtual machines. Every machine is
# assumed to be accessible using one key.
# GITBRANCH: antidote branch to start up
# CLEANMAKE = TRUE/FALSE: make relclean && make rel or not

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
if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="build-local-cluster-aws"
fi

# env
PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)

SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"
SSH_USERNAME=ubuntu

echo "[SCRIPT] RUNNING SCRIPT TO START MULTIPLE ANTIDOTE REPLICAS..."

ANTIDOTE_SCRIPT="./src/bin/worker-start-antidote.sh"
REMOTE_ANTIDOTE_SCRIPT="/home/ubuntu/worker-start-antidote.sh"

# copy scripts to remote machines and add execute permission
echo "[SCRIPT] COPYING REQUIRED SCRIPTS TO REMOTE MACHINES..."
for IP_ADDR in $IP_ADDR_LIST; do
    scp $SSH_OPTIONS $ANTIDOTE_SCRIPT $SSH_USERNAME@$IP_ADDR:$REMOTE_ANTIDOTE_SCRIPT
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod u+x $REMOTE_ANTIDOTE_SCRIPT
done

echo "[SCRIPT] COPIED ALL WORKER SCRIPTS."

echo "[SCRIPT] STARTING REMOTE ANTIDOTE NODES..."
for IP_ADDR in $IP_ADDR_LIST; do
    Command="ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR GITBRANCH=${GITBRANCH} CLEANMAKE=${CLEANMAKE} IP=${IP_ADDR} $REMOTE_ANTIDOTE_SCRIPT"
    echo "[SCRIPT] Starting antidote on node ${IP_ADDR}, using the following command:"
    echo "[SCRIPT] ${Command}"
    eval $Command &
done

# making sure the message is visible, antidote takes a few seconds to turn on
sleep 10
echo "[SCRIPT] Done. Antidote has been launched on the specified replicas."
