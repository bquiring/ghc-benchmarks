#!/usr/bin/env bash
# Set up the large group: unpack the programs (downloads), patch
# dependencies with doctest-only custom setups (../fix-custom-setups.py), pin
# every version (frozen.project, from cabal freeze), fetch agda-stdlib and
# make the inputs.  Then BENCH_GROUP=large ../build.sh and ../time.py.
set -eu
cd "$(dirname "$0")"
GHC_TREE=${GHC_TREE:-$HOME/projects/ghc-webs}
for p in ShellCheck-0.11.0 hledger-1.52.4 dhall-1.42.3 pandoc-cli-3.12 Agda-2.8.0.2 xml-conduit-1.10.1.0; do
  [ -d "$p" ] || cabal unpack "$p"
done
# xml-conduit: build-type Custom (cabal-doctest) to Simple
sed -i 's/^\([Bb]uild-[Tt]ype:[ \t]*\)Custom/\1Simple/' xml-conduit-1.10.1.0/xml-conduit.cabal
python3 - <<'PY'
import re
f = 'xml-conduit-1.10.1.0/xml-conduit.cabal'
s = open(f).read()
open(f, 'w').write(re.sub(r'(?im)^custom-setup\s*\n((?:[ \t]+.*\n|\s*\n)*)', '', s))
PY
if [ ! -d agda-stdlib ]; then
  curl -sL -o agda-stdlib-2.3.tar.gz https://github.com/agda/agda-stdlib/archive/refs/tags/v2.3.tar.gz
  mkdir agda-stdlib && tar -xzf agda-stdlib-2.3.tar.gz -C agda-stdlib --strip-components=1
fi
mkdir -p inputs out
python3 gen-journal.py 100000 inputs/journal.journal
cat "$GHC_TREE"/docs/users_guide/*.rst > inputs/guide.rst
