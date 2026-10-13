#!/usr/bin/env python3
"""Time the Hackage benchmarks (benchmarks.txt) in several configurations
built by build.sh:

    python3 time.py CONFIG... [--rounds N] [--cpu C] [--timeout S]

Each round runs every benchmark in every configuration, in random order,
pinned to one core, under perf stat (user-space instructions) with the
RTS's machine-readable statistics (allocation, mutator and GC time).  The
outputs (stdout and the files under out/) must be identical across
configurations; a difference is reported as an error.  Writes
results/timing-<label>.json and .md: per benchmark, the median over rounds
of each configuration's ratio to the first configuration."""
import argparse, json, math, os, random, re, shlex, shutil, statistics as st, subprocess, sys, tempfile

TOP = os.path.dirname(os.path.abspath(__file__))
# A benchmark group: this directory, or a subdirectory with its own
# benchmarks.txt and projects (BENCH_GROUP=large); the store is shared
HERE = os.path.join(TOP, os.environ.get('BENCH_GROUP', '.'))
# The GHC source tree: its compiler builds the benchmarks, and its sources
# are some of the inputs
GHC_TREE = os.environ.get('GHC_TREE', os.path.expanduser('~/projects/ghc-webs'))


OK_CODES = {}


def benchmarks():
    out = []
    with open(os.path.join(HERE, 'benchmarks.txt')) as f:
        for line in f:
            if line.startswith('#') or not line.strip():
                continue
            # an optional 4th column: the accepted exit codes (ok=0,1)
            f4 = line.rstrip('\n').split('\t')
            name, exe, cmd = f4[:3]
            ok = [int(c) for c in f4[3][3:].split(',')] if len(f4) > 3 else [0]
            OK_CODES[name] = ok
            out.append((name, exe, cmd))
    return out


def binary(config, exe):
    r = subprocess.run(['cabal', '--store-dir=' + os.path.join(TOP, 'store'), 'list-bin',
                        '--project-file=%s.project' % config, '--builddir=dist-' + config, exe if ':' in exe else 'exe:' + exe],
                       cwd=HERE, capture_output=True, text=True)
    return r.stdout.strip()


def run_once(config, name, exe, cmd, cpu, timeout):
    prog = binary(config, exe)
    argv = shlex.split(cmd.replace('EXE', prog).replace('GHC_TREE', GHC_TREE))
    argv = [a.replace('out/' + name, 'out/%s-%s' % (name, config)) for a in argv]
    for a in argv:
        if a.startswith('out/') and os.path.exists(os.path.join(HERE, a)):
            # a directory (a compiler's output or build directory) is
            # deleted so that nothing is reused, but not compared (outputs)
            if os.path.isdir(os.path.join(HERE, a)):
                shutil.rmtree(os.path.join(HERE, a))
            else:
                os.remove(os.path.join(HERE, a))
        if a.startswith('out/'):
            for f in os.listdir(os.path.join(HERE, 'out')):
                if f.startswith(a[4:] + '.') and os.path.isfile(os.path.join(HERE, 'out', f)):
                    os.remove(os.path.join(HERE, 'out', f))
    fd, perf_out = tempfile.mkstemp(suffix='.perf'); os.close(fd)
    fd, rts_out = tempfile.mkstemp(suffix='.rts'); os.close(fd)
    full = (['taskset', '-c', str(cpu), 'perf', 'stat', '-x,', '-o', perf_out, '-e', 'instructions:u']
            + argv + ['+RTS', '-t' + rts_out, '--machine-readable', '-RTS'])
    try:
        r = subprocess.run(full, cwd=HERE, capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return {'error': 'timeout'}
    if r.returncode not in OK_CODES.get(name, [0]):
        return {'error': 'exit %d: %s' % (r.returncode, (r.stdout + r.stderr)[-300:])}
    # A run must have written its output files (a program that refuses its
    # RTS options, for instance, stops at once and exits 0)
    # (or files named after it: futhark -o out/X writes out/X.c, out/X.h, ...)
    for a in argv:
        if a.startswith('out/') and not os.path.exists(os.path.join(HERE, a)) \
                and not any(f.startswith(a[4:] + '.') for f in os.listdir(os.path.join(HERE, 'out'))):
            return {'error': 'no output %s: %s' % (a, (r.stdout + r.stderr)[-300:])}
    if 'RTS options are disabled' in r.stderr:
        return {'error': 'RTS options disabled (link with -rtsopts)'}
    s = {}
    with open(perf_out) as f:
        for line in f:
            p = line.split(',')
            if len(p) > 2 and p[0].strip().isdigit() and 'instructions' in p[2]:
                s['instructions'] = s.get('instructions', 0) + int(p[0])
    with open(rts_out) as f:
        txt = f.read()
    for key, field in [('alloc', 'bytes allocated'), ('mut', 'mut_cpu_seconds'), ('gc', 'GC_cpu_seconds')]:
        m = re.search(r'\("%s", "([0-9.]+)"\)' % field, txt)
        if m:
            s[key] = float(m.group(1))
    s['stdout'] = r.stdout
    os.remove(perf_out); os.remove(rts_out)
    return s


# Build stamps a program writes into its output: Futhark records the git
# commit of the checkout it was built in (this repository), which differs
# between configurations built at different commits
STAMPS = re.compile(rb'git: \S+ @ [0-9a-f]+ \([^)]*\)( \[modified\])?')


def outputs(name, config):
    """The files a run wrote, for comparing configurations"""
    d = os.path.join(HERE, 'out')
    return {f: STAMPS.sub(b'git: STAMP', open(os.path.join(d, f), 'rb').read())
            for f in sorted(os.listdir(d))
            if f.startswith('%s-%s' % (name, config)) and os.path.isfile(os.path.join(d, f))}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('configs', nargs='+')
    ap.add_argument('--rounds', type=int, default=3)
    ap.add_argument('--cpu', type=int, default=2)
    ap.add_argument('--timeout', type=int, default=120)
    ap.add_argument('--label', default='latest')
    a = ap.parse_args()
    os.makedirs(os.path.join(HERE, 'out'), exist_ok=True)
    os.makedirs(os.path.join(HERE, 'results'), exist_ok=True)
    benches = benchmarks()
    samples = {n: {c: [] for c in a.configs} for n, _, _ in benches}
    errors = {}
    for rnd in range(a.rounds):
        order = [(b, c) for b in benches for c in a.configs]
        random.shuffle(order)
        for (name, exe, cmd), c in order:
            if name in errors:
                continue
            s = run_once(c, name, exe, cmd, a.cpu, a.timeout)
            if 'error' in s:
                errors[name] = '%s [%s]' % (s['error'], c)
                continue
            samples[name][c].append(s)
        print('round %d/%d done' % (rnd + 1, a.rounds), flush=True)
    # outputs must agree with the first configuration
    for name, _, _ in benches:
        if name in errors:
            continue
        ref = samples[name][a.configs[0]][-1]['stdout']
        ref_files = {k.replace(a.configs[0], 'C'): v for k, v in outputs(name, a.configs[0]).items()}
        for c in a.configs[1:]:
            files = {k.replace(c, 'C'): v for k, v in outputs(name, c).items()}
            if samples[name][c][-1]['stdout'] != ref or files != ref_files:
                errors[name] = 'output differs [%s]' % c
    for n in samples:
        for c in samples[n]:
            for s in samples[n][c]:
                s.pop('stdout', None)
    data = {'configs': a.configs, 'rounds': a.rounds, 'cpu': a.cpu, 'samples': samples, 'errors': errors}
    jpath = os.path.join(HERE, 'results', 'timing-%s.json' % a.label)
    json.dump(data, open(jpath, 'w'), indent=1)

    def med(n, c, m):
        if m == 'time':
            v = [s['mut'] + s['gc'] for s in samples[n][c] if 'mut' in s and 'gc' in s]
        else:
            v = [s[m] for s in samples[n][c] if s.get(m)]
        return st.median(v) if v else None

    base = a.configs[0]
    lines = ['# Hackage benchmarks: %s' % ' vs '.join(a.configs), '',
             '%d rounds, CPU %d.  Each cell: the configuration\'s median against %s.' % (a.rounds, a.cpu, base), '',
             '| benchmark | %s time (s) | %s instructions (G) | %s |' % (
                 base, base, ' | '.join('%s instructions | %s time | %s allocation' % (c, c, c) for c in a.configs[1:])),
             '|---|---|---|%s' % ('---|' * (3 * (len(a.configs) - 1)))]
    ratios = {c: {m: [] for m in ['instructions', 'time', 'alloc']} for c in a.configs[1:]}
    for name, _, _ in benches:
        if name in errors:
            lines.append('| %s | error: %s |' % (name, errors[name]))
            continue
        cells = []
        for c in a.configs[1:]:
            for m in ['instructions', 'time', 'alloc']:
                b, x = med(name, base, m), med(name, c, m)
                if b and x:
                    ratios[c][m].append(x / b)
                    cells.append('%+.2f%%' % (100 * (x / b - 1)))
                else:
                    cells.append('—')
        lines.append('| %s | %.2f | %.2f | %s |' % (name, med(name, base, 'time') or 0,
                                                     (med(name, base, 'instructions') or 0) / 1e9, ' | '.join(cells)))
    geo = []
    for c in a.configs[1:]:
        for m in ['instructions', 'time', 'alloc']:
            r = ratios[c][m]
            geo.append('%+.2f%%' % (100 * (math.exp(sum(map(math.log, r)) / len(r)) - 1)) if r else '—')
    lines.append('| **geomean** | | | %s |' % ' | '.join(geo))
    open(os.path.join(HERE, 'results', 'timing-%s.md' % a.label), 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
