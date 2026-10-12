#!/usr/bin/env python3
"""Translate the generated Prolog clauses of MLton's logic.sml (move_horiz
.. solution2) into Haskell for progs/Logic.hs: fn X => -> \\x ->, fn () =>
dropped (the body is an IO action), ; -> >>, sc () -> sc, variables and
the CON_* constants lower-cased."""
import re, sys
src = open(sys.argv[1]).read()
start = src.index('fun move_horiz')
end = src.index('end (* local *)', start)
s = src[start:end]
s = re.sub(r'\(\*.*?\*\)', '', s, flags=re.S)
s = re.sub(r'\b(fun|and) (\w+) ', r'@@\2 ', s)       # definitions, marked
s = s.replace('fn () =>', '')
s = re.sub(r'fn (\w+) =>', lambda m: '\\' + m.group(1).lower() + ' ->', s)
s = s.replace('sc ()', 'sc')
s = s.replace(';', '>>')
s = s.replace('STR(', 'str(')
keep = {'STR', 'INT'}
s = re.sub(r'\b[A-Z][A-Za-z0-9_]*\b', lambda m: m.group(0) if m.group(0) in keep else m.group(0).lower(), s)
s = re.sub(r'\n\s*\n+', '\n', s)
# one top-level definition per clause, indented bodies
out = []
for line in s.splitlines():
    if line.lstrip().startswith('@@'):
        out.append('')
        out.append(line.lstrip()[2:])
    elif line.strip():
        out.append('  ' + line)
print('\n'.join(out))
