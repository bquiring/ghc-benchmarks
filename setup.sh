#!/usr/bin/env bash
# Set up the Hackage benchmarks from the local cabal cache (no network):
# unpack the benchmark packages, pin every cached dependency to its cached
# version (cached.project), switch dependencies with doctest-only custom
# setups to build-type Simple, and make the inputs.  Then build.sh and
# time.py.  Separate from nofib.
set -eu
cd "$(dirname "$0")"
for p in happy-2.1.3 happy-lib-2.1.3 alex-3.5.2.0 cpphs-1.20.10 skylighting-0.13 pandoc-2.19.2; do
  [ -d "$p" ] || cabal unpack "$p"
done
python3 gen-constraints.py
python3 fix-custom-setups.py --listed
python3 fix-custom-setups.py $(grep -v '^#' benchmarks.txt | awk -F'\t' 'NF >= 3 { print "exe:" $2 }' | sort -u)
mkdir -p inputs out
cat "${GHC_TREE:-$HOME/projects/ghc-webs}"/docs/users_guide/*.rst > inputs/guide.rst
