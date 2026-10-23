#!/usr/bin/env python3
"""Classify the call sites of generic application (trace-apply.py's JSON)
into the program patterns behind them; shares of all samples.

    python3 apply-patterns.py results/apply-small.json large/results/apply-large.json ...
"""
import json, re, sys

SELECTORS = re.compile(r'_(zgzgze|zgzg|return|pure|fmap|zlztzg|ztzg|zlzt|liftA2|mplus|mzero|zlzbzg|empty|'
                       r'some|many|mempty|mappend|zlzg|sconcat|traverse|mapM|sequenceA|foldr|foldMap|'
                       r'get|put|state|ask|local|reader|throwError|catchError|lift|liftIO|'
                       r'compare|zeze|zsze|zl|zlze|zg|zgze|max|min|show|showsPrec)_info$')
PARSERS = re.compile(r'Parsec|Megaparsec|Attoparsec|Text\.Parser|Distribution\.Parsec|Dhall\.Parser|ReadP')


def classify(sym, part, mod):
    if part == 'stg':
        if 'upd' in mod or 'upd_frame' in sym:
            return 'thunk result applied (update frame)'
        if 'sel' in mod:
            return 'selector thunk'
        return 'other RTS'
    if PARSERS.search(mod):
        return 'parser combinators (CPS)'
    if SELECTORS.search(sym):
        return 'class-method selector (dictionary call)'
    if '_zdf' in sym:
        return 'instance method (dictionary-polymorphic)'
    if mod.startswith('transformers:') or mod.startswith('mtl:'):
        return 'monad transformer code'
    return part + ' code (other)'


rows = {}
for f in sys.argv[1:]:
    d = json.load(open(f))
    for name, r in d['results'].items():
        T = r['total']
        c = {}
        for sym, n, part, mod in r['callers']:
            k = classify(sym, part, mod)
            c[k] = c.get(k, 0) + n
        rows[name] = (100 * r['apply'] / T, {k: 100 * v / T for k, v in c.items()})
cats = sorted({k for _, c in rows.values() for k in c}, key=lambda k: -sum(c.get(k, 0) for _, c in rows.values()))
print('| program | apply | ' + ' | '.join(cats) + ' |')
print('|---|---|' + '---|' * len(cats))
for name, (a, c) in rows.items():
    print('| %s | %.1f%% | %s |' % (name, a, ' | '.join('%.1f' % c.get(k, 0) for k in cats)))
