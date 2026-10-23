#!/usr/bin/env bash
# PLDI'27 experiment C (Hackage, small group): the compiler of 2026-10-22,
# base22 vs webs22 (= nofib early-ulsl8-scl's flags).  base22 carries
# -fcore-webs-spec-max-keys=4 (the default, no effect without webs) only so
# that cabal hashes it apart from the store's older base builds.
#
#   ./run-paper-c.sh
set -u
cd "$(dirname "$0")"
B="-fcore-webs-early -fcore-webs-inline -fcore-webs-const-prop -fcore-webs-arity-raise -fcore-webs-result-raise -fcore-webs-strictness -fcore-webs-data-split -fcore-webs-data-unbox -fcore-webs-data-again -fcore-webs-boundary -fcore-webs-useless -fcore-webs-spec-calls -fcore-webs-spec-lambdas -dcore-webs-lint"
./build.sh base22 "-fcore-webs-spec-max-keys=4"
./build.sh webs22 "$B"
python3 time.py base22 webs22 --rounds 5 --label paper22
notify-send -a webs -i dialog-information "Webs: Hackage small group done" "results/timing-paper22.md"
