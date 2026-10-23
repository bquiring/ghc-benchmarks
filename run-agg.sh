#!/usr/bin/env bash
# Feasibility of dictionary specialisation (proposal 1, WEBS-EXPERIMENTS.md
# §33): the small group with GHC's aggressive specialisation (agg22), and
# with webs too (wagg22); timed and traced against base22 and webs22.
set -u
cd "$(dirname "$0")"
W=$(sed -n 's/^  ghc-options: -O2 -rtsopts //p' webs22.project)
A="-fspecialise-aggressively -fexpose-all-unfoldings -fspec-constr-count=6 -fspec-constr-threshold=4000"
./build.sh agg22 "$A"
./build.sh wagg22 "$W $A"
python3 time.py base22 webs22 agg22 wagg22 --rounds 3 --label agg22
python3 trace-apply.py agg22 --runs 1 --label agg22
python3 trace-apply.py wagg22 --runs 1 --label wagg22
notify-send -a webs -i dialog-information "Webs: Hackage aggressive specialisation done" "results/timing-agg22.md"
