#!/usr/bin/env bash
# Proposal 3 (dictionary webs): how much does specialisation on dictionaries
# give the compilers group?  GHC's aggressive specialisation with every
# unfolding exposed (agg) against base and ulsl2; traced; code size.
set -u
cd "$(dirname "$0")"
A="-fspecialise-aggressively -fexpose-all-unfoldings -fspec-constr-count=6 -fspec-constr-threshold=4000"
export BENCH_GROUP=compilers BENCH_JOBS=2 BENCH_ULIMIT=8000000 BENCH_CABAL_ARGS=--keep-going
start=$(date +%s); ./build.sh agg "$A"; echo "agg build: $(( $(date +%s) - start )) s"
python3 time.py base ulsl2 agg --rounds 3 --label agg
python3 trace-apply.py agg --runs 1 --label agg
for c in base ulsl2 agg; do for e in cabal-install:exe:cabal futhark:exe:futhark purescript:exe:purs koka:exe:koka; do
  b=$(cd compilers && cabal --store-dir=$PWD/../store list-bin --project-file=$c.project --builddir=dist-$c $e 2>/dev/null)
  echo "$c $e $(size $b 2>/dev/null | tail -1 | awk '{print $1}')"; done; done
notify-send -a webs -i dialog-information "Webs: proposal 3 (compilers, agg) done" "compilers/results/timing-agg.md"
