#!/bin/bash

pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

for i in `seq 1 $1`; do
	echo "Starting loader $i"
	$SCRIPTPATH/start_loader.sh \
		"$SCRIPTPATH/../../megaload/rel/megaload" \
		"$SCRIPTPATH/../../megaload/rel/megaload$i" \
		"loader$i@127.0.0.1" \
		"loader"
	echo "Started loader $i"
done