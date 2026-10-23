#!/usr/bin/env bash
# PLDI'27 experiment C: the SML translations with the compiler of
# 2026-10-22 (ghc-webs), base vs the paper's configuration (webs22,
# = nofib early-ulsl8-scl) vs it with -fcore-webs-spec-max-keys=8
# -fcore-webs-raise-max-args=6 (tuned22).
#
#   sml/run-paper-c.sh
set -u
cd "$(dirname "$0")/.."
B="-fcore-webs-early -fcore-webs-inline -fcore-webs-const-prop -fcore-webs-arity-raise -fcore-webs-result-raise -fcore-webs-strictness -fcore-webs-data-split -fcore-webs-data-unbox -fcore-webs-data-again -fcore-webs-boundary -fcore-webs-useless -fcore-webs-spec-calls -fcore-webs-spec-lambdas -dcore-webs-lint"
export BENCH_GROUP=sml
./build.sh base22 ""
./build.sh webs22 "$B"
./build.sh tuned22 "$B -fcore-webs-spec-max-keys=8 -fcore-webs-raise-max-args=6"
python3 time.py base22 webs22 tuned22 --rounds 3 --label paper22
notify-send -a webs -i dialog-information "Webs: SML suite done" "sml/results/timing-*.md"
