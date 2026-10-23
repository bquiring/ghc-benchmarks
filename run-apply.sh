#!/usr/bin/env bash
# Who calls the generic application code (trace-apply.py), all groups,
# webs builds: small webs22 (final), large/compilers ulsl2 (earlier).
set -u
cd "$(dirname "$0")"
python3 trace-apply.py webs22 --runs 2 --label small
BENCH_GROUP=large python3 trace-apply.py ulsl2 --runs 2 --label large
BENCH_GROUP=compilers python3 trace-apply.py ulsl2 --runs 2 --label compilers
python3 trace-apply.py base22 --runs 2 --label small-base
notify-send -a webs -i dialog-information "Webs: apply traces done" "results/apply-*.md"
