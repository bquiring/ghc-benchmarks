#!/usr/bin/env python3
"""The names that modules import from a set of modules, from their
-ddump-minimal-imports files (M.imports), for ghc-webs'
unused-exports.py --used-names (Note [Chunks] in GHC.WebCore.Chunk).

    python3 imported-names.py MODULES-FILE IMPORTS-FILE... > NAMES

MODULES-FILE lists the chunk's modules; only imports of those count.  A
name is given unqualified (a re-exported name is used wherever it is
defined); operators without parentheses."""
import re, sys

mods = set(open(sys.argv[1]).read().split())
names = set()
for f in sys.argv[2:]:
    txt = open(f).read()
    # each import declaration runs to the next one
    for decl in re.split(r'\n(?=import )', '\n' + txt):
        m = re.match(r'\s*import\s+(?:\{-# SOURCE #-\}\s+)?(?:qualified\s+)?(\S+)\s*(.*)', decl, re.S)
        if not m or m.group(1) not in mods:
            continue
        body = m.group(2)
        for tok in re.findall(r"\((?:[^\w\s(),]+)\)|[A-Za-z_][\w']*", body):
            names.add(tok.strip('()'))
print('\n'.join(sorted(names)))
