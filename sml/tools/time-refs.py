#!/usr/bin/env python3
"""Measure the MLton and OCaml originals next to the Haskell translations.

    python3 tools/time-refs.py CONFIG [--rounds N] [--cpu C] [--only NAME,...]

For each translation in tools/check-all.sh's lists, runs the original
(MLton or ocamlopt executable) and the Haskell build of CONFIG (as built by
BENCH_GROUP=sml ../build.sh CONFIG) at the same amount of work (the
iteration count check-all.sh uses), pinned to one CPU, and reports user-space
instructions and CPU time (perf stat) and allocation (GHC: +RTS -t; MLton:
@MLton gc-summary; OCaml: OCAMLRUNPARAM=v=0x400).  Medians of the rounds go
to results/refs-CONFIG.md.
"""
import argparse, os, re, statistics as st, subprocess, tempfile

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TOP = os.path.dirname(HERE)
LLVM = os.path.expanduser('~/projects/3cps/3cps-main/test/programs/llvm-sml')
T = os.path.join(HERE, 'tools')

# (Haskell executable, original, N, reference argv, kind): N is the
# translation's iteration count matching the original's own work
PAIRS = []
for name, sml, n in [
        ('ack', 'ack', 1), ('tak', 'tak', 1), ('cpstak', 'cpstak', 1), ('y-fact', 'y-fact', 100000),
        ('nqueens', 'nqueens', 1), ('mandelbrot', 'mandelbrot', 1), ('quicksort', 'quicksort', 1000),
        ('filter', 'filter', 10000), ('derivative', 'derivative', 10000), ('streams', 'streams', 100000),
        ('tardis', 'tardis', 10000), ('interpreter', 'interpreter', 10000),
        ('parser-comb', 'parser-comb', 10000), ('twenty-four', 'twenty-four', 10000),
        ('json-decode', 'json-decode', 100001), ('cps-convert', 'cps-convert', 100000),
        ('knuth-bendix', 'knuth-bendix', 2), ('raytracer', 'raytracer', 1),
        ('k-cfa', 'k-cfa', 10001), ('mc-ray', 'mc-ray', 1)]:
    PAIRS.append((name, n, [os.path.join(LLVM, sml + '.mlton.out')], 'mlton'))
PAIRS.append(('ratio-regions', 1, [os.path.join(T, 'mlton', 'ratio-regions.mlton')], 'mlton'))
for name in ['defunctionalization', 'returned-closures', 'function-composition', 'mutual-recursion-ho',
             'tree-fold-ho', 'curry-uncurry', 'closure-capture', 'higher-order-list', 'church-encoding',
             'cps-explicit', 'callback-state', 'environment-sharing', 'polymorphic-ho',
             'continuation-combinators', 'applicative-functor', 'tree-traversal-ho',
             'register-allocator', 'dpll-sat']:
    PAIRS.append((name, None, [os.path.join(T, 'mlton', 'ho', name + '.mlton')], 'mlton'))
for name in ['zebra', 'logic', 'smith-normal-form', 'tsp', 'count-graphs', 'zern', 'tyan',
             'barnes-hut', 'dlx-simulator', 'model-elimination']:
    PAIRS.append((name, 1, [os.path.join(T, 'mlton', name + '.mlton')], 'mlton'))
PAIRS.append(('vliw', 1, [os.path.join(T, 'mlton', 'vliw-run.sh')], 'mlton'))
for name in ['bdd', 'hamming', 'sieve', 'sorts']:
    PAIRS.append((name, 1, [os.path.join(T, 'ocaml', name + '.ocaml')], 'ocaml'))


def haskell_bin(config, name):
    r = subprocess.run(['cabal', '--store-dir=' + os.path.join(TOP, 'store'), 'list-bin',
                        '--project-file=%s.project' % config, '--builddir=dist-' + config,
                        'smlbench:exe:' + name], cwd=HERE, capture_output=True, text=True)
    return r.stdout.strip()


def measure(argv, cpu, env=None):
    fd, perf_out = tempfile.mkstemp(suffix='.perf'); os.close(fd)
    full = ['taskset', '-c', str(cpu), 'perf', 'stat', '-x,', '-o', perf_out,
            '-e', 'instructions:u,task-clock'] + argv
    r = subprocess.run(full, capture_output=True, text=True, env=env, timeout=600)
    s = {'exit': r.returncode, 'out': r.stdout, 'err': r.stderr}
    with open(perf_out) as f:
        for line in f:
            p = line.split(',')
            if len(p) > 2 and p[2].startswith('instructions'):
                s['instr'] = int(p[0])
            elif len(p) > 2 and p[2].startswith('task-clock'):
                s['secs'] = float(p[0]) / 1000.0
    os.remove(perf_out)
    return s


def alloc(kind, s):
    t = s['out'] + s['err']
    if kind == 'ghc':
        m = re.search(r'\("bytes allocated", "(\d+)"\)', t)
        return int(m.group(1)) if m else None
    if kind == 'mlton':
        m = re.search(r'bytes allocated:\s*([\d,]+)', t, re.I)
        return int(m.group(1).replace(',', '')) if m else None
    m = re.search(r'allocated_words:\s*(\d+)', t)
    return 8 * int(m.group(1)) if m else None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('config')
    ap.add_argument('--rounds', type=int, default=3)
    ap.add_argument('--cpu', type=int, default=2)
    ap.add_argument('--only')
    a = ap.parse_args()
    only = set(a.only.split(',')) if a.only else None
    rows = []
    for name, n, ref, kind in PAIRS:
        if only and name not in only:
            continue
        hs = haskell_bin(a.config, name)
        hargs = [hs] + ([str(n)] if n is not None else []) + ['+RTS', '-t', '--machine-readable', '-RTS']
        if kind == 'mlton':
            rargs = ref[:1] + ['@MLton', 'gc-summary', '--'] + ref[1:] if not ref[0].endswith('.sh') else ref
            renv = None
        else:
            rargs, renv = ref, dict(os.environ, OCAMLRUNPARAM='v=0x400')
        res = {'ghc': [], kind: []}
        for _ in range(a.rounds):
            for k, argv, env in [('ghc', hargs, None), (kind, rargs, renv)]:
                s = measure(argv, a.cpu, env)
                s['alloc'] = alloc(k, s)
                res[k].append(s)
        med = lambda k, f: (st.median([s[f] for s in res[k] if s.get(f) is not None])
                            if any(s.get(f) is not None for s in res[k]) else None)
        rows.append((name, kind, {k: {f: med(k, f) for f in ('instr', 'secs', 'alloc')} for k in res}))
        g, o = rows[-1][2]['ghc'], rows[-1][2][kind]
        print('%-24s %-5s ghc %6.3fs %14s instr  %s %6.3fs %14s instr' %
              (name, kind, g['secs'] or 0, g['instr'], kind, o['secs'] or 0, o['instr']), flush=True)
    os.makedirs(os.path.join(HERE, 'results'), exist_ok=True)
    fmt = lambda x: '-' if x is None else ('%.3f' % x if isinstance(x, float) else '{:,}'.format(x))
    ratio = lambda x, y: '-' if not x or not y else '%.2f' % (x / y)
    with open(os.path.join(HERE, 'results', 'refs-%s.md' % a.config), 'w') as f:
        f.write('# GHC (%s) against the MLton/OCaml originals, medians of %d rounds\n\n' % (a.config, a.rounds))
        f.write('| benchmark | ref | GHC s | ref s | GHC/ref time | GHC instr | ref instr | GHC alloc | ref alloc |\n')
        f.write('|---|---|---|---|---|---|---|---|---|\n')
        for name, kind, m in rows:
            g, o = m['ghc'], m[kind]
            f.write('| %s | %s | %s | %s | %s | %s | %s | %s | %s |\n' %
                    (name, kind, fmt(g['secs']), fmt(o['secs']), ratio(g['secs'], o['secs']),
                     fmt(g['instr']), fmt(o['instr']), fmt(g['alloc']), fmt(o['alloc'])))


main()
