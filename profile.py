#!/usr/bin/env python3
"""Where the Hackage benchmarks spend their instructions, by part of the
program, in two configurations:

    BENCH_GROUP=large python3 profile.py BASECONFIG CONFIG [--runs N] [--label L]

Each benchmark runs N times (default 3) per configuration under
`perf record -e instructions:u -c 1000003` (under the machine's 20k
samples/s limit, so not throttled), pinned to one core.  Sample addresses
are resolved against `nm -n` of the executable (perf's own resolution is
2 MB off for these non-PIE binaries).  Each sample is attributed to:

  own      the benchmark's own packages (built in the project, "-inplace",
           with the configuration's flags, webs included)
  deps     non-boot dependencies from the store (also built with the flags)
  boot     GHC's boot packages (base, containers, text, ...: never rebuilt,
           so no webs)
  gc       the RTS storage manager (rts/sm: GC, block allocation)
  rts      the rest of the RTS (stg_* code, scheduler, ...) and C libraries
  ?        addresses outside every symbol

Writes results/profile-<label>.md and .json: per benchmark and part, the
instructions (sample count x period, summed over runs) in each
configuration and their ratio; and the own and deps symbols whose counts
change most.
"""
import argparse, bisect, collections, json, os, re, shlex, shutil, subprocess, sys, tempfile

import importlib.util
# this directory's time.py (not the standard library's time)
_spec = importlib.util.spec_from_file_location(
    'benchtime', os.path.join(os.path.dirname(os.path.abspath(__file__)), 'time.py'))
timepy = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(timepy)

PERIOD = 1000003
GHC = os.path.expanduser('~/projects/ghc-webs/_build/stage1/bin/ghc-pkg')
SM_DIR = os.path.expanduser('~/projects/ghc-webs/_build/stage1/rts/build/c/sm')

BOOT = set(subprocess.run([GHC, 'list', '--simple-output', '--names-only'],
                          capture_output=True, text=True).stdout.split())
# rts/sm's functions (any way: names are the same)
GC = set()
for f in os.listdir(SM_DIR):
    if f.endswith('.o') or f.endswith('_o'):
        out = subprocess.run(['nm', os.path.join(SM_DIR, f)], capture_output=True, text=True).stdout
        GC.update(l.split()[2] for l in out.splitlines() if len(l.split()) == 3 and l.split()[1] in 'tT')

ZDEC = {'zm': '-', 'zi': '.', 'zu': '_', 'zz': 'z', 'zh': '#', 'zc': ':', 'zd': '$', 'zp': '+',
        'zs': '/', 'zl': '<', 'zg': '>', 'za': '&', 'zv': '|', 'zt': '*', 'zq': "'", 'ze': '=',
        'zn': '^', 'zr': '%', 'ZL': '(', 'ZR': ')', 'ZM': '[', 'ZN': ']', 'ZC': ':', 'ZZ': 'Z'}


def zdecode(s):
    return re.sub(r'z[a-z]|Z[A-Z]', lambda m: ZDEC.get(m.group(0), m.group(0)), s)


def unit_of(sym):
    """The package of a Haskell symbol (None for RTS/C symbols): the
    z-encoded unit id before the first '_'"""
    if '_' not in sym:
        return None
    u = zdecode(sym.split('_', 1)[0])
    if u in ('ghc-internal', 'ghc-prim', 'ghc-bignum', 'base', 'main', 'rts'):
        return u
    m = re.match(r'^(.*?)-\d+(\.\d+)*(-|$)', u)
    return m.group(1) if m else None


def module_of(sym):
    """unit:Module of a Haskell symbol"""
    p = sym.split('_')
    return '%s:%s' % (unit_of(sym), zdecode(p[1])) if len(p) > 2 else sym


def part(sym, binary_units):
    if sym is None:
        return '?'
    if sym in GC:
        return 'gc'
    u = unit_of(sym)
    if u == 'main':
        return 'own'
    if u is None or u == 'rts':
        # stg_*: the RTS's Haskell-level entry code (generic apply, update
        # frames, primops); the rest is the RTS's C code (scheduler, ...)
        return 'stg' if sym.startswith('stg_') else 'rts'
    if u in BOOT:
        return 'boot'
    return 'own' if binary_units.get(u) == 'inplace' else 'deps'


def symbols(binary):
    out = subprocess.run(['nm', '-n', binary], capture_output=True, text=True).stdout
    addrs, names = [], []
    units = {}
    for l in out.splitlines():
        p = l.split()
        if len(p) == 3 and p[1] in 'tTwW':
            addrs.append(int(p[0], 16)); names.append(p[2])
            if '_' in p[2]:
                u = zdecode(p[2].split('_', 1)[0])
                pkg = unit_of(p[2])
                if pkg and pkg not in units:
                    units[pkg] = 'inplace' if '-inplace' in u else 'store'
    return addrs, names, units


def profile_run(config, name, exe, cmd, cpu, binary):
    argv = shlex.split(cmd.replace('EXE', binary).replace('GHC_TREE', timepy.GHC_TREE))
    argv = [a.replace('out/' + name, 'out/%s-%s' % (name, config)) for a in argv]
    for a in argv:
        if a.startswith('out/') and os.path.isdir(os.path.join(timepy.HERE, a)):
            shutil.rmtree(os.path.join(timepy.HERE, a))
    fd, data = tempfile.mkstemp(suffix='.perf.data'); os.close(fd)
    full = ['taskset', '-c', str(cpu), 'perf', 'record', '-q', '-N', '-e', 'instructions:u',
            '-c', str(PERIOD), '-o', data] + argv + ['+RTS', '-V0', '-RTS']
    r = subprocess.run(full, cwd=timepy.HERE, capture_output=True, text=True, timeout=600)
    if r.returncode not in timepy.OK_CODES.get(name, [0]):
        raise RuntimeError('%s %s: exit %d: %s' % (name, config, r.returncode, r.stderr[-300:]))
    s = subprocess.run(['perf', 'script', '-i', data, '-F', 'ip,dso'], capture_output=True, text=True).stdout
    os.remove(data)
    ips = collections.Counter()
    other = collections.Counter()
    for l in s.splitlines():
        p = l.split()
        if len(p) >= 2 and os.path.basename(p[1].strip('()')) == os.path.basename(binary):
            ips[int(p[0], 16)] += 1
        elif p:
            other[os.path.basename(p[-1].strip('()'))] += 1     # C libraries (libc, gmp, ...), vdso
    return ips, other


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('base'); ap.add_argument('config')
    ap.add_argument('--runs', type=int, default=3)
    ap.add_argument('--cpu', type=int, default=2)
    ap.add_argument('--label', default='latest')
    ap.add_argument('--only', default='')
    a = ap.parse_args()
    res = {}
    for name, exe, cmd in timepy.benchmarks():
        if a.only and name not in a.only.split(','):
            continue
        res[name] = {}
        for config in (a.base, a.config):
            binary = timepy.binary(config, exe)
            addrs, names, units = symbols(binary)
            parts = collections.Counter(); syms = collections.Counter()
            for _ in range(a.runs):
                ips, other = profile_run(config, name, exe, cmd, a.cpu, binary)
                parts['clib'] += sum(other.values())
                for dso, n in other.items():
                    syms[('clib', dso)] += n
                for ip, n in ips.items():
                    i = bisect.bisect_right(addrs, ip) - 1
                    sym = names[i] if i >= 0 else None
                    pt = part(sym, units)
                    parts[pt] += n
                    syms[(pt, sym)] += n
            res[name][config] = {'parts': {k: v * PERIOD for k, v in parts.items()},
                                 'syms': [[p, s, n * PERIOD] for (p, s), n in syms.most_common(300)
                                          if p in ('own', 'deps')],
                                 'all_syms': [[p, s, n * PERIOD] for (p, s), n in syms.items()],
                                 'own_units': sorted(u for u, k in units.items() if k == 'inplace' and u not in BOOT),
                                 'dep_units': sorted(u for u, k in units.items() if k == 'store' and u not in BOOT)}
            print('%s %s: %s' % (name, config, {k: '%.2fG' % (v * PERIOD / 1e9 / a.runs) for k, v in parts.items()}),
                  flush=True)
    out = os.path.join(timepy.HERE, 'results', 'profile-%s' % a.label)
    json.dump({'base': a.base, 'config': a.config, 'runs': a.runs, 'results': res}, open(out + '.json', 'w'))
    report(res, a.base, a.config, a.runs, out + '.md')


def report(res, base, config, runs, path):
    L = []
    w = L.append
    w('# Where the time goes: %s vs %s\n' % (base, config))
    w('Instructions (user space, perf record every %d, summed over %d runs, shown per run), by part of the program; '
      'own = the benchmark\'s own packages, deps = non-boot dependencies (both built with the configuration\'s flags), '
      'boot = GHC\'s boot packages (never rebuilt: no webs), gc = rts/sm (GC and block allocation), '
      'stg = the RTS\'s stg_* code (generic apply, update frames, primops), rts = the RTS\'s other C code, '
      'clib = C libraries (by shared object).\n'
      % (PERIOD, runs))
    parts = ['own', 'deps', 'boot', 'gc', 'stg', 'rts', 'clib', '?']
    w('| benchmark | ' + ' | '.join('%s (share, change)' % p for p in parts) + ' | total change |')
    w('|---|' + '---|' * (len(parts) + 1))
    for name, r in res.items():
        b, c = r[base]['parts'], r[config]['parts']
        tb, tc = sum(b.values()), sum(c.values())
        cells = []
        for p in parts:
            vb, vc = b.get(p, 0), c.get(p, 0)
            ch = '%+.1f%%' % (100 * (vc / vb - 1)) if vb else '-'
            cells.append('%.0f%%, %s' % (100 * vb / tb, ch))
        w('| %s | %s | %+.2f%% |' % (name, ' | '.join(cells), 100 * (tc / tb - 1)))
    w('')
    for name, r in res.items():
        w('## %s\n' % name)
        w('own packages: %s; dependencies built with the flags: %s\n'
          % (', '.join(r[base]['own_units']) or '-', ', '.join(r[base]['dep_units']) or '-'))
        # per module first: data splitting and specialisation rename
        # symbols (HappyCons becomes Con1_s59), modules stay
        mb, mc = collections.Counter(), collections.Counter()
        for cnt, cfg in ((mb, base), (mc, config)):
            for p, sym, n in r[cfg]['all_syms']:
                cnt[(p, module_of(sym) if p in ('own', 'deps', 'boot') else sym)] += n
        tot = sum(r[base]['parts'].values())
        # where the base build's time goes: the largest modules (Haskell)
        # and symbols (RTS, C) of every part
        w('Largest in %s: %s\n' % (base, '; '.join(
            '%s %s %.0f%%' % (k[0], k[1], 100 * v / tot) for k, v in mb.most_common(12))))
        mods = sorted([k for k in set(mb) | set(mc) if k[0] in ('own', 'deps')],
                      key=lambda k: -max(mb.get(k, 0), mc.get(k, 0)))[:10]
        w('| part | module | %s | %s | change | change (share of total) |' % (base, config))
        w('|---|---|---|---|---|---|')
        for k in mods:
            vb, vc = mb.get(k, 0), mc.get(k, 0)
            w('| %s | %s | %.3fG | %.3fG | %s | %+.2f%% |' % (k[0], k[1], vb / 1e9 / runs, vc / 1e9 / runs,
                                                             '%+.1f%%' % (100 * (vc / vb - 1)) if vb else 'new',
                                                             100 * (vc - vb) / tot))
        w('')
        sb = {(p, s): n for p, s, n in r[base]['syms']}
        sc = {(p, s): n for p, s, n in r[config]['syms']}
        keys = set(sb) | set(sc)
        tot = sum(r[base]['parts'].values())
        top = sorted([k for k in keys if k[0] in ('own', 'deps')],
                     key=lambda k: -abs(sc.get(k, 0) - sb.get(k, 0)))[:12]
        w('| part | symbol | %s | %s | change (share of total) |' % (base, config))
        w('|---|---|---|---|---|')
        for k in top:
            vb, vc = sb.get(k, 0), sc.get(k, 0)
            w('| %s | `%s` | %.3fG | %.3fG | %+.2f%% |' % (k[0], k[1][:70], vb / 1e9 / runs, vc / 1e9 / runs,
                                                          100 * (vc - vb) / tot))
        w('')
    open(path, 'w').write('\n'.join(L) + '\n')
    print('report:', path)


if __name__ == '__main__':
    main()
