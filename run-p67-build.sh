#!/usr/bin/env bash
# Proposals 6/7 by hand: build the variants (shellcheck v1: strict fields;
# v2: strict, enumerations unpacked, value flattened into state; koka v2:
# positions flattened into ranges).  Base flags; dependencies from the store.
set -u
cd "$(dirname "$0")"
ulimit -v 4000000
for v in scv1 scv2; do
  (cd large && cabal --store-dir=$PWD/../store build -j1 --project-file=$v.project --builddir=dist-$v ShellCheck:exe:shellcheck > results/$v-build.log 2>&1; echo "[$v] build exit code: $?")
done
(cd compilers && cabal --store-dir=$PWD/../store build -j1 --project-file=kokav2.project --builddir=dist-kokav2 koka:exe:koka > results/kokav2-build.log 2>&1; echo "[kokav2] build exit code: $?")
