#!/usr/bin/env bash
# Proposal 7 by hand on hledger: Posting's fields strict (except the
# knot-tying ptransaction/poriginal); after run-p67-build.sh.
set -u
cd "$(dirname "$0")"
until grep -q "kokav2\] build exit" run-p67-build.log; do sleep 30; done
ulimit -v 8000000
(cd large && cabal --store-dir=$PWD/../store build -j1 --project-file=hlv1.project --builddir=dist-hlv1 hledger:exe:hledger > results/hlv1-build.log 2>&1; echo "[hlv1] build exit code: $?")
