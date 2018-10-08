#!/usr/bin/env bash
# author goncalotomas
set -e

if [ -z "$PRIVATEKEY" ]; then
    PRIVATEKEY=~/.ssh/fmke_experiments.pem
fi
if [[ ! -e $PRIVATEKEY ]]; then
    echo "Error: $PRIVATEKEY: no such file"
    exit 2
fi;

BENCHMARKS_DIR=benchmarks
KEY_FILE_NAME=$(basename $PRIVATEKEY)
IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
SSH_OPTIONS="-o StrictHostKeyChecking=no -i $PRIVATEKEY"
USER=ubuntu

if [ ! -d "$BENCHMARKS_DIR" ]; then
    mkdir $BENCHMARKS_DIR
fi

IP_ARR=(`echo ${IP_ADDR_LIST}`);

WORKER_SCRIPT="${PWD}/src/bin/compile-and-compress-results.sh"
REMOTE_WORKER_SCRIPT="/home/ubuntu/compile-and-compress-results.sh"

cd $BENCHMARKS_DIR

for index in "${!IP_ARR[@]}"; do
    ## COMPILE RESULTS, TAR, FETCH TO LOCAL dir
    echo "[SCRIPT]: Copying worker script to remote machine..."
    scp ${SSH_OPTIONS} ${WORKER_SCRIPT} ${USER}@${IP_ARR[$index]}:${REMOTE_WORKER_SCRIPT}
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} chmod u+x ${REMOTE_WORKER_SCRIPT} &
    echo "[SCRIPT]: Worker script copied successfully."
    echo "[SCRIPT]: Running worker script..."
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} WORKERID=$(($index + 1)) ${REMOTE_WORKER_SCRIPT} &
    pids="$pids $!"
 done

 echo "[SCRIPT] Waiting for SSH processes to finish their work..."
 for pid in $pids; do
     wait $pid || let "RESULT=1"
 done

 if [ "$RESULT" == "1" ]; then
     echo "[SCRIPT] Something went wrong in installing all the software!"
     exit 1
 else
     echo "[SCRIPT] Done. All remote machines have the required software stack and repositories."
 fi
