#!/bin/bash
set -e

# Argument 1: Path to megaload release
MEGALOAD_REL=${1:-../../megaload/rel/megaload}
# Argument 2: Target folder (where to copy the release)
TARGET_FOLDER=${2:-../../megaload/rel/megaload1}
# Argument 3: Erlang node name
NODE_NAME=${3:-loader1@127.0.0.1}
# ARgument 4: Erlang cookie
COOKIE=${4:-loader}

echo "MEGALOAD_REL = $MEGALOAD_REL"
echo "TARGET_FOLDER = $TARGET_FOLDER"
echo "NODE_NAME = $NODE_NAME"

# copy release to target folder
cp -r $MEGALOAD_REL $TARGET_FOLDER

# replace node name
sed -i "s/-name.*/-name $NODE_NAME/" "$TARGET_FOLDER/etc/vm.args"
# replae cookie
sed -i "s/-setcookie.*/-setcookie $COOKIE/" "$TARGET_FOLDER/etc/vm.args"

#start the loader node
$TARGET_FOLDER/bin/megaload start