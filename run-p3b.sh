#!/usr/bin/env bash
# Proposal 3, continued: purescript ran out of heap (exit 251) under the
# 8 GB limit with aggressive specialisation; one job, 14 GB.  Then purs
# timed and every program traced.
set -u
cd "$(dirname "$0")"
A="-fspecialise-aggressively -fexpose-all-unfoldings -fspec-constr-count=6 -fspec-constr-threshold=4000"
export BENCH_GROUP=compilers BENCH_JOBS=1 BENCH_ULIMIT=14000000
start=$(date +%s); /usr/bin/time -v ./build.sh agg "$A" 2> compilers/results/agg-time-v.txt; echo "agg rebuild: $(( $(date +%s) - start )) s"
grep "Maximum resident" compilers/results/agg-time-v.txt
python3 time.py base ulsl2 agg --rounds 3 --label agg
python3 trace-apply.py agg --runs 1 --label agg
notify-send -a webs -i dialog-information "Webs: proposal 3 (purs) done" "compilers/results/timing-agg.md"
