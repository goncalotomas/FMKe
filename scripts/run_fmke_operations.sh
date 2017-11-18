#!/bin/bash
set -e
echo "running FMKe unit tests with $1 back end..."
./scripts/config/change_db.sh $1
./scripts/config/change_db_ports.sh $1
./scripts/start_data_store.sh $1
make rel
./scripts/start_fmke.sh
set +e
rebar3 eunit
if [ $? -ne  0 ]; then
    set -e
    echo "fatal: one or more tests failed."
    ./scripts/stop_fmke.sh
    ./scripts/stop_data_store.sh $1
    exit 5
fi
./scripts/stop_fmke.sh
./scripts/stop_data_store.sh $1
echo "done"
