#!/usr/bin/env bash
cd basho_bench
Rscript --vanilla priv/summary.r -i ../tests/current
cd -
tar hczf "results-${WORKERID}.tar.gz" tests/current
echo "[SCRIPT]: Compiled and tar'd results for worker ${WORKERID}."
