#!/usr/bin/env python3
"""Count Specialise's copies per function from -ddump-rules dumps.

    spec-rules.py DUMPDIR

A rule named "SPEC f ..." (local) or "SPEC/Mod f ..." (imported f
specialised in this module) is one copy; SpecConstr's "SC:..." rules are
counted apart.  Prints totals and the functions with the most copies."""
import collections, os, re, sys

root = sys.argv[1]
spec = collections.Counter()      # (kind, function) -> copies
per_module = collections.Counter()
sc = 0
files = 0
for d, _, fs in os.walk(root):
    for f in fs:
        if not f.endswith('.dump-rules'):
            continue
        files += 1
        mod = os.path.relpath(os.path.join(d, f), root)[:-len('.dump-rules')].replace('/', '.')
        for line in open(os.path.join(d, f), errors='replace'):
            m = re.match(r'^"(SPEC(?:/(\S+))?) ([^ "]+)', line)
            if m:
                fn = m.group(3)
                kind = 'imported' if m.group(2) else 'local'
                owner = m.group(2) or mod
                spec[(kind, owner + '.' + fn)] += 1
                per_module[mod] += 1
            elif line.startswith('"SC:'):
                sc += 1

total = sum(spec.values())
imp = sum(n for (k, _), n in spec.items() if k == 'imported')
print('%d dump files; %d SPEC copies (%d of imported functions) of %d functions; %d SpecConstr rules'
      % (files, total, imp, len(spec), sc))
counts = sorted(spec.values(), reverse=True)
for n in (1, 2, 4, 8):
    fns = sum(1 for c in counts if c > n)
    copies = sum(c - n for c in counts if c > n)
    print('  with at most %d copies per function: %d functions cut, %d copies (%.0f%%) fewer'
          % (n, fns, copies, 100.0 * copies / total if total else 0))
print('most copies:')
for (k, fn), n in spec.most_common(15):
    print('  %4d  %-8s %s' % (n, k, fn))
print('modules with most copies:')
for m, n in per_module.most_common(8):
    print('  %4d  %s' % (n, m))
