#!/usr/bin/env bash
# Proposal 8, compilers group, continued: xu-agg ran out of memory in
# purescript's Language.PureScript.Interactive at 14 GB.  One job, 18 GB,
# --keep-going (the other programs build even if purs fails); then timed
# (time.py now reads each configuration's store from results/CONFIG/store).
set -u
cd "$(dirname "$0")"
A="-fspecialise-aggressively -fexpose-all-unfoldings -fspec-constr-count=6 -fspec-constr-threshold=4000"
export BENCH_GROUP=compilers BENCH_STORE=store-xu
start=$(date +%s)
BENCH_JOBS=1 BENCH_ULIMIT=18000000 BENCH_CABAL_ARGS=--keep-going /usr/bin/time -v ./build.sh xu-agg "$A" 2> compilers/results/xu-agg-time-v.txt
echo "compilers xu-agg: $(( $(date +%s) - start )) s"
grep "Maximum resident" compilers/results/xu-agg-time-v.txt
unset BENCH_STORE
python3 time.py base agg xu-base xu-agg --rounds 3 --label p8
notify-send -a webs -i dialog-information "Webs: proposal 8, compilers group done" "compilers/results/timing-p8.md"
