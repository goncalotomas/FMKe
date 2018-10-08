#!/bin/bash
# author goncalotomas
# This script starts N replicas of FMKe on previously configured remote vms.
# You should pass in a list of (public) IP addresses as arguments to the script,
# as well as the following environment variables:
# PRIVATEKEY: used to ssh into the amazon virtual machines. Every machine is
# assumed to be accessible using one key.
# GITBRANCH: FMKe branch to start up
# ANTIDOTE_ADDRESS: list of CSV antidote IP addresses
# ANTIDOTE_PB_PORT: list of CSV antidote PB ports (must be 1-to-1 with previous list)

set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

USAGE="Usage: ANTIDOTE_ADDRESS=<CSV IP LIST> ANTIDOTE_PB_PORT=<CSV PORT LIST> $0 <private_key_file> <FMK_IP_ADDR_LIST>"

# args checking
if [[ $# -lt 2 ]]; then
    echo $USAGE
    exit 2
fi;

if [[ ! -e $1 ]]; then
    echo "Error: $1: no such file"
    exit 2
fi;
if [ -z "$ANTIDOTE_ADDRESS" ]; then
    echo $USAGE
    exit 2
fi
if [ -z "$ANTIDOTE_PB_PORT" ]; then
    echo $USAGE
    exit 2
fi

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="perf-and-errors"
fi

# env
PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)

IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)


echo "[SCRIPT] RUNNING SCRIPT TO START MULTIPLE FMKe REPLICAS..."

SSH_USERNAME=ubuntu
SSH_OPTIONS="-i $PRIVATEKEY -o StrictHostKeyChecking=no"

FMK_SCRIPT="./src/bin/worker-start-fmk.sh"
REMOTE_FMK_SCRIPT="/home/ubuntu/worker-start-fmk.sh"

# copy scripts to remote machines and add execute permission
echo "[SCRIPT] COPYING REQUIRED SCRIPTS TO REMOTE MACHINES..."
for IP_ADDR in $IP_ADDR_LIST; do
    scp $SSH_OPTIONS $FMK_SCRIPT $SSH_USERNAME@$IP_ADDR:$REMOTE_FMK_SCRIPT
    ssh $SSH_OPTIONS $SSH_USERNAME@$IP_ADDR chmod u+x $REMOTE_FMK_SCRIPT
done
echo "[SCRIPT] COPIED ALL WORKER SCRIPTS."

ANTIDOTE_ADDRESS_ARR=(`echo ${ANTIDOTE_ADDRESS}`);
ANTIDOTE_ADDRESS_ARR_SIZE=${#ANTIDOTE_ADDRESS_ARR[@]}
ANTIDOTE_PORT_ARR=(`echo ${ANTIDOTE_PB_PORT}`);
IP_ARR=(`echo ${IP_ADDR_LIST}`);

for index in "${!IP_ARR[@]}"; do
    ## ASSIGN EACH FMK TO EACH ANTIDOTE IN A ROUND ROBIN FASHION
    ## SO THAT WE ALLOW THE CASE WHERE #FMKNODES > #ANTIDOTENODES
    Command="ssh $SSH_OPTIONS $SSH_USERNAME@${IP_ARR[$index]} GITBRANCH=${GITBRANCH} CLEANMAKE=${CLEANMAKE} IP=${IP_ARR[$index]} ANTIDOTE_ADDRESS=${ANTIDOTE_ADDRESS_ARR[$(($index % $ANTIDOTE_ADDRESS_ARR_SIZE))]} ANTIDOTE_PB_PORT=${ANTIDOTE_PORT_ARR[$(($index % $ANTIDOTE_ADDRESS_ARR_SIZE))]} ${REMOTE_FMK_SCRIPT}"
    echo "[SCRIPT] Starting antidote on node ${IP_ARR[$index]}, using the following command:"
    echo "[SCRIPT] ${Command}"
    eval $Command &
done

sleep 10 # fmk takes a while to boot up.
echo "[SCRIPT] Done. FMKe has been launched on the specified replicas."
