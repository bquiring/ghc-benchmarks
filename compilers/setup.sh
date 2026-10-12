#!/usr/bin/env bash
# Set up the compilers group: unpack cabal-install, Futhark and PureScript
# from Hackage, clone Koka from GitHub, pin every dependency
# (frozen.project, from cabal freeze) and fetch the workloads' inputs.
# Then BENCH_GROUP=compilers ../build.sh and ../time.py.
set -eu
cd "$(dirname "$0")"
for p in cabal-install-3.18.1.0 futhark-0.27.1 purescript-0.15.15 happy-1.20.0 pattern-arrows-0.0.2 cheapskate-0.1.1.2; do
  [ -d "$p" ] || cabal unpack "$p"
done
if [ ! -d koka-3.2.9 ]; then
  git clone -q --depth 1 --branch v3.2.9 --recurse-submodules --shallow-submodules \
    https://github.com/koka-lang/koka koka-3.2.9
fi
# happy 1.20.0 (PureScript pins it): mtl no longer re-exports Control.Monad
grep -q 'import Control.Monad (when)' happy-1.20.0/src/Grammar.lhs ||
  sed -i 's/^> import Data.Maybe (fromMaybe)$/> import Data.Maybe (fromMaybe)\n> import Control.Monad (when)/' happy-1.20.0/src/Grammar.lhs
# pattern-arrows 0.0.2 (PureScript): the same, for fix
grep -q 'import Data.Function (fix)' pattern-arrows-0.0.2/src/Control/PatternArrows.hs ||
  sed -i '0,/^import /s//import Data.Function (fix)\nimport /' pattern-arrows-0.0.2/src/Control/PatternArrows.hs
# cheapskate 0.1.1.2 (PureScript): the same, for guard, replicateM, unless
grep -q 'import Control.Monad (guard, replicateM, unless)' cheapskate-0.1.1.2/Cheapskate/Parse.hs ||
  sed -i '0,/^import /s//import Control.Monad (guard, replicateM, unless)\nimport /' cheapskate-0.1.1.2/Cheapskate/Parse.hs
# PureScript 0.15.15 (written for mtl 2.2): the same, in its own sources
find purescript-0.15.15/src purescript-0.15.15/app -name '*.hs' -print0 | xargs -0 python3 patch-mtl.py
# PureScript binds type variables in constructor patterns (@f), which this
# GHC accepts only with TypeAbstractions
grep -q 'TypeAbstractions' purescript-0.15.15/purescript.cabal ||
  sed -i 's/^\(    default-extensions:\)$/\1\n        TypeAbstractions/' purescript-0.15.15/purescript.cabal
# Koka compiles its modules 8 at a time (ghc-options -j8): one at a time
# here, as every package, under the per-process memory limit
sed -i 's/^\(  ghc-options:.*\) -j8\b/\1/' koka-3.2.9/koka.cabal
if [ ! -s frozen.project ]; then
  : > frozen.project   # cabal.project imports it
  cabal --store-dir="$PWD/../store" freeze --builddir=dist-solve
  mv cabal.project.freeze frozen.project
fi
# Inputs.  cabal: inputs/cabal-solve (in the repo).  Futhark: the
# futhark-benchmarks suite (with its libraries), at a pinned commit
if [ ! -d inputs/futhark-benchmarks ]; then
  git clone -q https://github.com/diku-dk/futhark-benchmarks inputs/futhark-benchmarks
  git -C inputs/futhark-benchmarks checkout -q 4d43e06e9076631bbf06eb4134a17ed5b6a30f37
fi
# PureScript: 53 libraries of the package set, at their tagged versions
# (inputs/ps-closure.txt: name, repository, tag; the dependency closure of
# the libraries picked)
while read -r name repo tag; do
  [ -d "inputs/purescript-libs/$name" ] ||
    git clone -q --depth 1 --branch "$tag" "$repo" "inputs/purescript-libs/$name"
done < inputs/ps-closure.txt
