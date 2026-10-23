#!/usr/bin/env bash
# Proposal 8 with 3: boot libraries with exposed unfoldings.
#  1. A second compiler tree, ghc-webs/_build-xu: the same sources and
#     flavour as _build, with -fexpose-all-unfoldings for the stage-1
#     libraries programs use (_build-xu/hadrian.settings).
#  2. Its own store (store-xu): its boot packages have the same unit ids
#     as _build's.  Configurations xu-base (-O2) and xu-agg (-O2 and
#     aggressive specialisation), compiler from xu-*.extra.
#  3. Timed against base22/agg22 (small group) and base/agg (compilers).
# The ghc-webs sources must not change while step 1 builds.
set -u
cd "$(dirname "$0")"
A="-fspecialise-aggressively -fexpose-all-unfoldings -fspec-constr-count=6 -fspec-constr-threshold=4000"
XU=/home/benjamin/projects/ghc-webs/_build-xu/stage1/bin/ghc

start=$(date +%s)
(cd /home/benjamin/projects/ghc-webs && ulimit -v 8000000 && hadrian/build -j4 --flavour=devel2 --build-root=_build-xu) \
  > /home/benjamin/projects/ghc-webs/build-xu.log 2>&1
echo "compiler tree: exit $?, $(( $(date +%s) - start )) s"
if [ ! -x "$XU" ]; then
  notify-send -a webs -i dialog-error "Webs: proposal 8 build failed" "ghc-webs/build-xu.log"; exit 1
fi
# lookup's unfolding must be in the interface now
"$XU" --show-iface /home/benjamin/projects/ghc-webs/_build-xu/stage1/libraries/ghc-internal/build/GHC/Internal/List.hi \
  | grep -A6 "^  lookup ::" | head -8

for g in . compilers; do
  printf -- '-- compiler tree with exposed boot-library unfoldings (run-p8.sh)\nwith-compiler: %s\n' "$XU" > "$g/xu-base.extra"
  cp "$g/xu-base.extra" "$g/xu-agg.extra"
done

export BENCH_STORE=store-xu
start=$(date +%s); ./build.sh xu-base "";   echo "small xu-base: $(( $(date +%s) - start )) s"
start=$(date +%s); ./build.sh xu-agg "$A";  echo "small xu-agg: $(( $(date +%s) - start )) s"
python3 time.py base22 agg22 xu-base xu-agg --rounds 3 --label p8
notify-send -a webs -i dialog-information "Webs: proposal 8, small group timed" "results/timing-p8.md"

export BENCH_GROUP=compilers
start=$(date +%s); BENCH_JOBS=2 BENCH_ULIMIT=8000000 ./build.sh xu-base ""; echo "compilers xu-base: $(( $(date +%s) - start )) s"
start=$(date +%s); BENCH_JOBS=1 BENCH_ULIMIT=14000000 ./build.sh xu-agg "$A"; echo "compilers xu-agg: $(( $(date +%s) - start )) s"
python3 time.py base agg xu-base xu-agg --rounds 3 --label p8
notify-send -a webs -i dialog-information "Webs: proposal 8 done" "results/timing-p8.md, compilers/results/timing-p8.md"
