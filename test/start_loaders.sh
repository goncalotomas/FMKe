#!/bin/bash

for i in `seq 1 $1`; do
	echo "Starting loader $i"
	./start_loader.sh \
		"../../megaload/rel/megaload" \
		"../../megaload/rel/megaload$i" \
		"loader$i@127.0.0.1" \
		"loader"
	echo "Started loader $i"
done