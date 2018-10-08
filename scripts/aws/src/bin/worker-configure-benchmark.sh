FMK_HTTP_ADDRESSES="\"${FMK_HTTP_ADDRESSES}\""

sed -ie 's#{fmk_server_ips, \[\(\"\([0-9]\{1,3\}\.\)\{3\}\([0-9]\{1,3\}\)\{1\}\"\)\(,\(\"\([0-9]\{1,3\}\.\)\{3\}\([0-9]\{1,3\}\)\{1\}\"\)\)*\]}.#{fmk_server_ips, ['"${FMK_HTTP_ADDRESSES}"']}.#g' ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured FMK server addresses."
else
    echo "[SCRIPT]: Could not write FMK server addresses to node ${IP_ADDR}, aborting..."
    exit 1
fi

sed -ie 's#{fmk_server_ports, \[[0-9]\+[,[0-9]\+]*\]}.#{fmk_server_ports, ['"${FMK_HTTP_PORTS}"']}.#g' ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured FMK server ports."
else
    echo "[SCRIPT]: Could not write FMK server ports to node ${IP_ADDR}, aborting..."
    exit 1
fi

sed -ie 's#{concurrent, [0-9]\+}.#{concurrent, '"${NUM_CLIENTS}"'}.#g' ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured number of basho bench clients."
else
    echo "[SCRIPT]: Could not write number of basho bench clients in node ${IP_ADDR}, aborting..."
    exit 1
fi

sed -ie 's#{duration, [0-9]\+}.#{duration, '"${BENCHDURATION}"'}.#g' ${REMOTE_CONFIG_FILE}
if [ "$?" = 0  ]; then
    echo "[SCRIPT]: Configured benchmark duration."
else
    echo "[SCRIPT]: Could not write benchmark duration in node ${IP_ADDR}, aborting..."
    exit 1
fi
echo "[SCRIPT]: Node ${IP_ADDR} has been successfully configured."
