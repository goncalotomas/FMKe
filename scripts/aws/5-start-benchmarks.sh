#!/usr/bin/env bash
# author goncalotomas

PRIVATEKEY=$1
KEY_FILE_NAME=$(basename $PRIVATEKEY)
IP_ADDR_LIST=$(echo $* | cut -d' ' -f2-)
SSH_OPTIONS="-o StrictHostKeyChecking=no -i $PRIVATEKEY"
USER=ubuntu

if [ -z "$BENCHDURATION" ]; then
    BENCHDURATION=1
fi
if [ -z "$POPULATE_ANTIDOTE" ]; then
    POPULATE_ANTIDOTE=FALSE
fi

USAGE="FMK_HTTP_ADDRESSES=<LIST OF CSV IP ADDRESSES> FMK_HTTP_PORTS=<LIST OF CSV PORTS> NUM_CLIENTS=<NUM THREADS PER MACHINE> [POPULATE_ANTIDOTE=<TRUE|FALSE>] [BENCHDURATION=<MINUTES>] $0 <PATH TO PVT KEY> <LIST OF BASHO BENCH IPS>"

# check that the script was called with the right parameters
if [[ ! -e $1 ]]; then
    echo "Private key error: $1: no such file"
    exit 2
fi;
if [ -z "$FMK_HTTP_ADDRESSES" ]; then
  echo "Missing list of FMKe server addresses"
  echo ${USAGE}
  exit 1
fi
if [ -z "$FMK_HTTP_PORTS" ]; then
  echo "Missing list of FMKe server ports"
  echo ${USAGE}
  exit 1
fi
if [ -z "$NUM_CLIENTS" ]; then
  echo "Missing number of basho bench clients"
  echo ${USAGE}
  exit 1
fi

##########################################################
    # Verify that all nodes are reachable
##########################################################
echo "[SCRIPT]: STEP 1/4: TESTING REQUIREMENTS FOR EVERY BASHO BENCH NODE..."

for IP_ADDR in $IP_ADDR_LIST; do
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} exit
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] SSH connection OK for ${IP_ADDR}"
    else
        echo "[SCRIPT] SSH connection NOT OK for ${IP_ADDR}, aborting..."
        exit 1
    fi
done

echo "[SCRIPT] All nodes are reachable."

REMOTE_FILE_PATH="/home/ubuntu/basho_bench/_build/default/bin/basho_bench"

#########################################################
# CONNECTION TEST STAGE                                 #
#########################################################
for IP_ADDR in $IP_ADDR_LIST; do
    ssh ${SSH_OPTIONS} ${USER}@${IP_ADDR} cat $REMOTE_FILE_PATH > /dev/null 2>&1
    if [ "$?" = 0  ]; then
        echo "[SCRIPT] Basho bench escript is present for ${IP_ADDR}"
    else
        echo "[SCRIPT] Basho bench is not compiled in node ${IP_ADDR}, aborting..."
        exit 1
    fi
done

echo "[SCRIPT]: STEP 1/4: Done. All nodes contain a compiled version of basho_bench."

#########################################################
# BENCHMARK CONFIGURATION STAGE                         #
#########################################################
FMK_ADDRESS_ARR=(`echo ${FMK_HTTP_ADDRESSES}`);
FMK_ADDRESS_ARR_SIZE=${#FMK_ADDRESS_ARR[@]}
FMK_PORT_ARR=(`echo ${FMK_HTTP_PORTS}`);
IP_ARR=(`echo ${IP_ADDR_LIST}`);

echo "[SCRIPT]: STEP 2/4: Editing configuration files for each bench node..."
REMOTE_CONFIG_FILE="/home/ubuntu/basho_bench/examples/fmkclient.config"
WORKER_SCRIPT="./src/bin/worker-configure-benchmark.sh"
REMOTE_WORKER_SCRIPT="/home/ubuntu/worker-configure-benchmark.sh"
WORKER_BENCH_SCRIPT="./src/bin/worker-start-basho-bench.sh"
REMOTE_WORKER_BENCH_SCRIPT="/home/ubuntu/worker-start-basho-bench.sh"

for index in "${!IP_ARR[@]}"; do
    ## ASSIGN EACH BASHO BENCH TO EACH FMK IN A ROUND ROBIN FASHION
    ## SO THAT WE ALLOW THE CASE WHERE #BENCHENODES > #FMKNODES
    echo "[SCRIPT]: Copying configuration script to remote machine..."
    scp ${SSH_OPTIONS} ${WORKER_SCRIPT} ${USER}@${IP_ARR[$index]}:${REMOTE_WORKER_SCRIPT}
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} chmod u+x ${REMOTE_WORKER_SCRIPT}
    echo "[SCRIPT]: Configuration script copied successfully."
    echo "[SCRIPT]: Copying runnable worker script to remote machine..."
    scp ${SSH_OPTIONS} ${WORKER_BENCH_SCRIPT} ${USER}@${IP_ARR[$index]}:${REMOTE_WORKER_BENCH_SCRIPT}
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} chmod u+x ${REMOTE_WORKER_BENCH_SCRIPT}
    echo "[SCRIPT]: Runnable worker script copied successfully."
    echo "[SCRIPT]: Running configuration script..."
    ssh ${SSH_OPTIONS} ${USER}@${IP_ARR[$index]} NUM_CLIENTS=${NUM_CLIENTS} BENCHDURATION=${BENCHDURATION} IP_ADDR=${IP_ARR[$index]} FMK_HTTP_ADDRESSES=${FMK_ADDRESS_ARR[$(($index % $FMK_ADDRESS_ARR_SIZE))]} FMK_HTTP_PORTS=${FMK_PORT_ARR[$(($index % $FMK_ADDRESS_ARR_SIZE))]} REMOTE_CONFIG_FILE=${REMOTE_CONFIG_FILE} ${REMOTE_WORKER_SCRIPT}
done

#########################################################
# ANTIDOTE POPULATION STAGE                             #
#########################################################
echo "[SCRIPT]: STEP 3/4: Checking if antidote population has been requested..."
if [ "$POPULATE_ANTIDOTE" = TRUE ]
    then
        echo "[SCRIPT] Antidote population has been requested."
        for IP_ADDR in $IP_ADDR_LIST; do
            REQUESTER=${IP_ADDR} ## This is dumb but it works, so I'll leave it
        done;
        for IP_ADDR in $FMK_HTTP_ADDRESSES; do
            POPULATION_ADDRESS=${IP_ADDR} ## This is dumb but it works, so I'll leave it
        done;

        echo "[SCRIPT] Running antidote population worker script..."
        LOCAL_POPULATION_SCRIPT="./src/bin/fmk_setup_script.erl"
        REMOTE_POPULATION_SCRIPT="/home/ubuntu/fmk_setup_script.erl"
        POPULATOR_NODE_REF="populate@${REQUESTER}"
        FMK_NODE_REF="fmk@${POPULATION_ADDRESS}"
        scp $SSH_OPTIONS $LOCAL_POPULATION_SCRIPT $USER@$IP_ADDR:$REMOTE_POPULATION_SCRIPT
        ssh $SSH_OPTIONS $USER@$IP_ADDR chmod u+x $REMOTE_POPULATION_SCRIPT
        ssh $SSH_OPTIONS $USER@$IP_ADDR $REMOTE_POPULATION_SCRIPT $POPULATOR_NODE_REF $FMK_NODE_REF
    else
        echo "[SCRIPT] No request for antidote population found. Continuing..."
fi
echo "STEP 3/4: Done."

#########################################################
# BENCHMARKING STAGE                                    #
#########################################################
REMOTE_BB_SCRIPT=${REMOTE_WORKER_BENCH_SCRIPT}
echo "[SCRIPT]: STEP 4/4: Starting benchmarks..."
for IP_ADDR in $IP_ADDR_LIST; do
    echo "[SCRIPT]: Starting benchmark in node ${IP_ADDR}..."
    ssh $SSH_OPTIONS $USER@${IP_ADDR} GITBRANCH=${GITBRANCH} CLEANMAKE=${CLEANMAKE} ${REMOTE_BB_SCRIPT} &
done
echo "[SCRIPT]: BENCHMARKS STARTED IN ALL NODES."

echo "[SCRIPT]: I'm just gonna sleep for ${BENCHDURATION} minutes, ok? BRB."

sleep $((${BENCHDURATION}*60))

echo "ZzzzzZZZzzzzzZZZzzzzz...."

sleep 45

echo "[SCRIPT]: Done!"
