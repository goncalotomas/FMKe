#!/bin/bash

export RELX_REPLACE_OS_VARS=true
for i in `seq 1 $1`;
do
  _build/default/rel/fmk${i}/bin/fmk${i} stop
done
