#!/bin/bash
set -e

pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

INSTANCE_COUNT=3

# Clean already running erlang procsses
killall beam.smp || true

# Start megaload UI
echo "Starting megaload-UI"
cd $SCRIPTPATH/../megaload-UI
# switch to right version for megaload-UI
. ~/apps/erlang/17.5/activate
make start

# switch back to original erlang version
. ~/apps/erlang/18.3/activate


# Start Antidote instances
echo "Starting Antidote"
cd $SCRIPTPATH/../antidote
./bin/all.sh $INSTANCE_COUNT

# Start FMK instances
echo "Starting FMK"
cd $SCRIPTPATH/../FMKe
./bin/all.sh $INSTANCE_COUNT

echo "Started FMK servers"

# Start megaload loaders
echo "Starting megaload loaders"
./test/start_loaders.sh $INSTANCE_COUNT



# Run benchmark
LOADERS=""
for i in `seq 1 $INSTANCE_COUNT`; do
    LOADERS="$LOADERS loader$i@127.0.0.1"
done
echo "LOADERS = $LOADERS"
echo $LOADERS | xargs ./test/run_megaload.py




