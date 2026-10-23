#!/usr/bin/env bash
# Where the time goes (profile.py), per part of each program: the small
# group in the final configuration (base22 vs webs22), the large and
# compilers groups in the configuration they were last built with (base vs
# ulsl2: early-ulsl's flags, no constant specialisation).
set -u
cd "$(dirname "$0")"
python3 profile.py base22 webs22 --runs 5 --label small
BENCH_GROUP=large python3 profile.py base ulsl2 --runs 5 --label large
BENCH_GROUP=compilers python3 profile.py base ulsl2 --runs 5 --label compilers
notify-send -a webs -i dialog-information "Webs: Hackage profiles done" "results/profile-*.md"
