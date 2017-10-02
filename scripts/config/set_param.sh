#!/bin/bash
set -e

PARAM_NAME=$1
PARAM_NEW_VAL=$2

PARAM_CUR_VAL=$(
  grep '{$1, .*}' config/fmke.config | awk '{print $2;}' | sed 's/}.$//'
)

echo "Changing ${PARAM_NAME} to ${PARAM_NEW_VAL}..."

if [[ "${PARAM_NEW_VAL}" == "${PARAM_CUR_VAL}" ]]; then
    echo "No changes needed, $PARAM_NAME already set to $PARAM_CUR_VAL."
    echo "success"
    exit 0
fi

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Making changes in the header file using Linux commands..."
    sed -i -e "s/{$PARAM_NAME, .*}\./{$PARAM_NAME, $PARAM_NEW_VAL}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Making changes in the header file using OS X commands..."
    sed -i '' -e "s/{$PARAM_NAME, .*}\./{$PARAM_NAME, $PARAM_NEW_VAL}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    echo "Making changes in the header file using FreeBSD commands..."
    sed -i -e "s/{$PARAM_NAME, .*}\./{$PARAM_NAME, $PARAM_NEW_VAL}\./g" ./config/fmke.config
fi

if [ "$?" -ne 0 ]; then
    echo "Error: error changing $PARAM_NAME"
    echo "error"
    exit 3
fi
echo "success"
