#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-fmk.sh

FMK_DIR="/home/ubuntu/FMKe"

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="perf-and-errors"
fi
if [ -z "$ANTIDOTE_ADDRESS" ]; then
    echo "Error: missing list of comma separated antidote IP addresses"
    exit 2
fi
if [ -z "$ANTIDOTE_PB_PORT" ]; then
    echo "Error: missing list of comma separated antidote PB ports"
    exit 2
fi

if [ CLEANMAKE=TRUE ]; then
    cd $FMK_DIR
    echo "[SCRIPT] KILLING ALL ERLANG PROCESSES ON REMOTE MACHINES..."
    pkill beam
    if [ -f rebar.lock ]; then
        rm rebar.lock ## not doing this was causing issues
    fi
    echo "----Worker $IP ----: git checkout $GITBRANCH"
    git checkout $GITBRANCH
    echo "----Worker $IP ----: git pull"
    git pull
    echo "[SCRIPT] DELETING DATA FROM PREVIOUS BENCHMARKS, IF ANY..."
    echo "----Worker $IP ----: make relclean"
    make relclean
    echo "[SCRIPT] REGENERATING RELX RELEASE..."
    echo "----Worker $IP ----: make rel"
    make rel

fi
echo "----Worker $IP ----: IP=$IP ANTIDOTE_ADDRESS=$ANTIDOTE_ADDRESS IP=$IP INSTANCE_NAME=fmk ./_build/default/rel/fmk/bin/env foreground"
ANTIDOTE_ADDRESS=$ANTIDOTE_ADDRESS ANTIDOTE_PB_PORT=$ANTIDOTE_PB_PORT IP=$IP INSTANCE_NAME=fmk nohup ./_build/default/rel/fmk/bin/env foreground &
