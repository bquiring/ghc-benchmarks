#!/usr/bin/env bash
# Experiment C, resumed after the ConstSpec fix (QuickCheck's
# Test.QuickCheck.Property panicked in unliftProgram): finish webs22, time.
set -u
cd "$(dirname "$0")"
B="-fcore-webs-early -fcore-webs-inline -fcore-webs-const-prop -fcore-webs-arity-raise -fcore-webs-result-raise -fcore-webs-strictness -fcore-webs-data-split -fcore-webs-data-unbox -fcore-webs-data-again -fcore-webs-boundary -fcore-webs-useless -fcore-webs-spec-calls -fcore-webs-spec-lambdas -dcore-webs-lint"
./build.sh webs22 "$B" || exit 1
python3 time.py base22 webs22 --rounds 5 --label paper22
notify-send -a webs -i dialog-information "Webs: Hackage small group done" "results/timing-paper22.md"
