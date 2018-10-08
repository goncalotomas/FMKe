#!/bin/bash
# author goncalotomas
# This script joins N already running replicas of antidote into a cluster (DC).
# You should pass in a list of (public) IP addresses as arguments to the script,
# as well as the following environment variables:
# PRIVATEKEY: used to ssh into the amazon virtual machines. Every machine is
# assumed to be accessible using one key.

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
    GITBRANCH="build-local-cluster"
fi

# env
PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)

SSH_USERNAME=ubuntu
SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)

for IP_ADDR in $IP_ADDR_LIST; do
    ACCUM="$ACCUM antidote@$IP_ADDR"
    REQUESTER=$IP_ADDR
done

echo "[SCRIPT] RUNNING SCRIPT TO JOIN MULTIPLE ANTIDOTE CLUSTERS..."

echo "[SCRIPT] RUNNING THE JOIN CLUSTER SCRIPT FROM $REQUESTER..."

JOIN_CLUSTER_SCRIPT="./src/bin/join_dcs_script.erl"
REMOTE_JOIN_CLUSTER_SCRIPT="/home/ubuntu/join_dcs_script.erl"

scp $SSH_OPTIONS $JOIN_CLUSTER_SCRIPT $SSH_USERNAME@$REQUESTER:$REMOTE_JOIN_CLUSTER_SCRIPT
ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod u+x $REMOTE_JOIN_CLUSTER_SCRIPT
Command="ssh $SSH_OPTIONS $SSH_USERNAME@$REQUESTER $REMOTE_JOIN_CLUSTER_SCRIPT $ACCUM"
echo "Requesting antidote cluster join on node $REQUESTER, using the following command:"
echo "${Command}"
eval $Command

# cluster creation may take a while
echo "[SCRIPT] Done. The specified antidote replicas are now joined in a cluster."
