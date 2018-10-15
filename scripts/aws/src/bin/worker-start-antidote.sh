#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-antidote.sh

ANTIDOTE_DIR="/home/ubuntu/antidote"

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="build-local-cluster"
fi

if [ CLEANMAKE=TRUE ]; then
    cd $ANTIDOTE_DIR
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
    cd -
fi

echo "----Worker $IP ----: IP=$IP INSTANCE_NAME=antidote ~/antidote/_build/default/rel/antidote/bin/env foreground"
IP=$IP INSTANCE_NAME=antidote nohup ~/antidote/_build/default/rel/antidote/bin/env foreground &
