#!/bin/bash

export RELX_REPLACE_OS_VARS=true
for i in `seq 1 $1`;
do
  HTTP_PORT=9${i}90 INSTANCE_NAME=fmk${i} ANTIDOTE_PB_PORT=8${i}87 COOKIE=antidote _build/default/rel/fmk${i}/bin/env foreground &
done
