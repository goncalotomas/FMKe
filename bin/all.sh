#!/bin/bash

export RELX_REPLACE_OS_VARS=true
i=$1
   echo "stopping FMKe instances..."
   ./bin/stop-nodes.sh ${i}
      echo "cleanning old FMKe releases and building new ones..."
   ./bin/build-releases.sh ${i}
   echo "launching FMKe releases..."
   ./bin/launch-nodes.sh ${i}

