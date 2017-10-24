#!/bin/bash
set -e

PARAM_NAME=$1
PARAM_NEW_VAL=$2

PARAM_CUR_VAL=$(
  grep '{$1, .*}' config/fmke.config | awk '{print $2;}' | sed 's/}.$//'
)

if [[ "${PARAM_NEW_VAL}" == "${PARAM_CUR_VAL}" ]]; then
    echo "No changes needed, $PARAM_NAME already set to $PARAM_CUR_VAL."
    exit 0
fi

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    sed -i -e "s/{$PARAM_NAME, .*}\./{$PARAM_NAME, $PARAM_NEW_VAL}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "darwin"* ]]; then
    sed -i '' -e "s/{$PARAM_NAME, .*}\./{$PARAM_NAME, $PARAM_NEW_VAL}\./g" ./config/fmke.config
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    sed -i -e "s/{$PARAM_NAME, .*}\./{$PARAM_NAME, $PARAM_NEW_VAL}\./g" ./config/fmke.config
fi

if [ "$?" -ne 0 ]; then
    echo "Error: error changing ${PARAM_NAME} to ${PARAM_NEW_VAL}"
    exit 3
fi

echo "Changed ${PARAM_NAME} to ${PARAM_NEW_VAL}."
