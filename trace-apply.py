#!/usr/bin/env python3
"""Who calls GHC's generic application code: unknown calls, traced to
their call sites with the CPU's last-branch record.

    BENCH_GROUP=compilers python3 trace-apply.py CONFIG [--only NAME,..] [--runs N] [--label L]

Each benchmark runs under `perf record -b -e instructions:u -c 1000003`.
A sample whose address is in generic-application code (stg_ap_*,
stg_PAP_apply: the RTS code an unknown call goes through) is traced back
through its branch stack (most recent first) to the branch that entered
that code from outside it; that branch's source is the call site.
Samples whose entry lies beyond the 32 recorded branches count as
"untraced".  Call sites are reported by symbol and by module, with each
module's part (own, deps, boot: see profile.py).

Writes results/apply-<label>.md and .json.
"""
import argparse, bisect, collections, importlib.util, json, os, re, shlex, shutil, subprocess, tempfile

HERE_DIR = os.path.dirname(os.path.abspath(__file__))


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


timepy = load('benchtime', os.path.join(HERE_DIR, 'time.py'))
prof = load('benchprofile', os.path.join(HERE_DIR, 'profile.py'))
PERIOD = prof.PERIOD
APPLY = re.compile(r'^stg_(ap_|PAP_apply)')


def resolve(addrs, names, ip):
    i = bisect.bisect_right(addrs, ip) - 1
    return names[i] if i >= 0 else None


def trace_run(config, name, exe, cmd, cpu, binary, addrs, names, units):
    part_cache = {}

    def part_of(sym):
        if sym not in part_cache:
            part_cache[sym] = prof.part(sym, units) if sym else '?'
        return part_cache[sym]

    argv = shlex.split(cmd.replace('EXE', binary).replace('GHC_TREE', timepy.GHC_TREE))
    argv = [a.replace('out/' + name, 'out/%s-%s' % (name, config)) for a in argv]
    for a in argv:
        if a.startswith('out/') and os.path.isdir(os.path.join(timepy.HERE, a)):
            shutil.rmtree(os.path.join(timepy.HERE, a))
    fd, data = tempfile.mkstemp(suffix='.perf.data'); os.close(fd)
    full = ['taskset', '-c', str(cpu), 'perf', 'record', '-q', '-N', '-b', '-e', 'instructions:u',
            '-c', str(PERIOD), '-o', data] + argv + ['+RTS', '-V0', '-RTS']
    r = subprocess.run(full, cwd=timepy.HERE, capture_output=True, text=True, timeout=900)
    if r.returncode not in timepy.OK_CODES.get(name, [0]):
        raise RuntimeError('%s %s: exit %d: %s' % (name, config, r.returncode, r.stderr[-300:]))
    p = subprocess.Popen(['perf', 'script', '-i', data, '-F', 'ip,brstack'], stdout=subprocess.PIPE, text=True)
    total = 0
    apply_samples = 0
    callers = collections.Counter()      # call-site symbol -> samples
    origins = collections.Counter()      # first own/deps symbol further back -> samples
    kinds = collections.Counter()        # apply symbol -> samples
    untraced = 0
    is_apply = {}

    def apply_sym(addr):
        s = resolve(addrs, names, addr)
        if s not in is_apply:
            is_apply[s] = bool(s and APPLY.match(s))
        return s if is_apply[s] else None

    for line in p.stdout:
        f = line.split()
        if not f:
            continue
        total += 1
        ip = int(f[0], 16)
        s = apply_sym(ip)
        if not s:
            continue
        apply_samples += 1
        kinds[s] += 1
        # brstack: most recent first; FROM/TO/...
        found = None
        origin = None
        for j, br in enumerate(f[1:]):
            parts = br.split('/')
            if len(parts) < 2:
                continue
            frm, to = int(parts[0], 16), int(parts[1], 16)
            if found is None:
                if apply_sym(to) and not apply_sym(frm):
                    found = frm
                    fs = resolve(addrs, names, frm)
                    if part_of(fs) in ('own', 'deps'):
                        origin = fs
                        break
            else:
                # keep walking back (through class-method selectors, boot
                # code, update frames) to the program's own code
                fs = resolve(addrs, names, frm)
                if part_of(fs) in ('own', 'deps'):
                    origin = fs
                    break
        if found is None:
            untraced += 1
        else:
            callers[resolve(addrs, names, found) or '?'] += 1
            origins[origin or '(none in branch record)'] += 1
    p.wait()
    os.remove(data)
    return total, apply_samples, kinds, callers, origins, untraced


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('config')
    ap.add_argument('--runs', type=int, default=2)
    ap.add_argument('--cpu', type=int, default=2)
    ap.add_argument('--label', default='latest')
    ap.add_argument('--only', default='')
    a = ap.parse_args()
    res = {}
    for name, exe, cmd in timepy.benchmarks():
        if a.only and name not in a.only.split(','):
            continue
        binary = timepy.binary(a.config, exe)
        addrs, names, units = prof.symbols(binary)
        T = A = U = 0
        K, C, O = collections.Counter(), collections.Counter(), collections.Counter()
        for _ in range(a.runs):
            t, ap_, k, c, o, u = trace_run(a.config, name, exe, cmd, a.cpu, binary, addrs, names, units)
            T += t; A += ap_; U += u; K.update(k); C.update(c); O.update(o)
        res[name] = {'total': T, 'apply': A, 'untraced': U, 'kinds': K.most_common(),
                     'callers': [[s, n, prof.part(s, units) if s != '?' else '?',
                                  prof.module_of(s) if s != '?' else '?'] for s, n in C.most_common(400)],
                     'origins': [[s, n, prof.part(s, units) if s.endswith('_info') or '_' in s else '-',
                                  prof.module_of(s) if '_' in s else s] for s, n in O.most_common(400)]}
        print('%s: apply %.1f%% of samples, untraced %.1f%% of those' % (name, 100 * A / T, 100 * U / max(A, 1)),
              flush=True)
    out = os.path.join(timepy.HERE, 'results', 'apply-%s' % a.label)
    json.dump({'config': a.config, 'runs': a.runs, 'results': res}, open(out + '.json', 'w'))
    report(res, a.config, out + '.md')


def report(res, config, path):
    L = []
    w = L.append
    w('# Who calls the generic application code (%s)\n' % config)
    w('Samples in stg_ap_* / stg_PAP_apply, traced through the last-branch record to the branch that '
      'entered that code: its source is the call site.  Shares are of all samples of the program.\n')
    for name, r in res.items():
        T, A = r['total'], r['apply']
        w('## %s\n' % name)
        w('Generic application: %.1f%% of samples; traced %.0f%% of those.\n' % (100 * A / T, 100 * (A - r['untraced']) / max(A, 1)))
        w('By kind: %s\n' % ', '.join('%s %.1f%%' % (k, 100 * n / T) for k, n in r['kinds'][:8]))
        by_part, by_mod = collections.Counter(), collections.Counter()
        for s, n, pt, mod in r['callers']:
            by_part[pt] += n; by_mod[(pt, mod)] += n
        w('Call sites by part: %s\n' % ', '.join('%s %.1f%%' % (p, 100 * n / T) for p, n in by_part.most_common()))
        w('| part | module | share of all samples |')
        w('|---|---|---|')
        for (pt, mod), n in by_mod.most_common(12):
            w('| %s | %s | %.2f%% |' % (pt, mod, 100 * n / T))
        w('')
        by_omod = collections.Counter()
        for s, n, pt, mod in r['origins']:
            by_omod[(pt, mod)] += n
        w('Origin: the first own or deps code further back in the branch record (through selectors, '
          'boot code and update frames), by module:\n')
        w('| part | module | share of all samples |')
        w('|---|---|---|')
        for (pt, mod), n in by_omod.most_common(10):
            w('| %s | %s | %.2f%% |' % (pt, mod, 100 * n / T))
        w('')
        w('| part | call site | share of all samples |')
        w('|---|---|---|')
        for s, n, pt, mod in r['callers'][:15]:
            w('| %s | `%s` | %.2f%% |' % (pt, s[:90], 100 * n / T))
        w('')
    open(path, 'w').write('\n'.join(L) + '\n')
    print('report:', path)


if __name__ == '__main__':
    main()
