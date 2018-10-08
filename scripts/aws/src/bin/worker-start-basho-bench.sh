#!/usr/bin/env bash
# Call this like IP=1.2.3.4 worker-start-fmk.sh

BB_DIR="/home/ubuntu/basho-bench"

if [ -z "$CLEANMAKE" ]; then
    CLEANMAKE=TRUE
fi
if [ -z "$GITBRANCH" ]; then
    GITBRANCH="antidote_pb_fmk_aws"
fi

if [ CLEANMAKE=TRUE ]; then
    cd $BB_DIR
    git checkout $GITBRANCH
    git pull
    make all
fi

./_build/default/bin/basho_bench examples/fmkclient.config &
