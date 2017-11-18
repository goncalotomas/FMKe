#!/bin/sh
set -e

reset="\033[0m"
red="\033[31m"
green="\033[32m"
yellow="\033[33m"
cyan="\033[36m"
white="\033[37m"

echo ""
printf "$cyan===> Running Common Test unit tests with $1 back end...$reset\n"
./scripts/config/change_db.sh $1
./scripts/config/change_db_ports.sh $1
./scripts/start_data_store.sh $1
printf "$cyan===> Generating FMKe release...$reset\n"
make rel
./scripts/start_fmke.sh
set +e
rebar3 ct
if [ $? -ne  0 ]; then
    set -e
    ./scripts/stop_fmke.sh
    ./scripts/stop_data_store.sh $1
    printf "$red===> One or more tests failed :(.$reset\n"
    exit 5
fi
./scripts/stop_fmke.sh
./scripts/stop_data_store.sh $1
printf "$green===> Successfully ran CT suite against $1!$reset\n"
