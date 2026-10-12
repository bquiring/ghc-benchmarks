#!/usr/bin/env python3
"""Copy SML integer-list literals into Haskell definitions:
    smllists.py FILE.sml NAME[=HSNAME]...
prints  hsname :: [Int]\\nhsname = [...]  for each SML  val NAME = [...]"""
import re, sys
src = open(sys.argv[1]).read()
for spec in sys.argv[2:]:
    name, _, hs = spec.partition('=')
    hs = hs or name
    m = re.search(r'val\s+' + re.escape(name) + r'\s*=\s*\[(.*?)\]', src, re.S)
    if not m:
        sys.exit('no list ' + name)
    nums = [int(x.replace('~', '-')) for x in re.findall(r'~?\d+', m.group(1))]
    lines, cur = [], []
    for n in nums:
        cur.append(str(n))
        if len(cur) == 16:
            lines.append(', '.join(cur)); cur = []
    if cur:
        lines.append(', '.join(cur))
    print('%s :: [Int]\n%s =\n  [ %s ]\n' % (hs, hs, '\n  , '.join(lines)))
