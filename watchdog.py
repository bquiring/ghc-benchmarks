#!/usr/bin/env python3
"""Watch a running build.sh and restart it when it hangs.

A hang: a process of the build (cabal, its Setup children, ghc, the linker)
that has used no CPU time for --idle minutes (default 10), or a build where
no process used any CPU for that long.  It happened once: the disk filled
during an install, and cabal and ghc then waited on each other for hours
with free disk again.  The watchdog stops the build (the whole process
tree), waits until the disk has --min-free GB free, and runs the build
command again; cabal resumes, rebuilding only units not yet in the store.
At most --restarts restarts; then it stops and says so in its output.

A package (a cabal unit, "Starting NAME (COMPONENT)" in the log) that takes
more than --slow minutes (default 20) while using CPU is only logged.

Everything goes to stdout (no desktop alerts).  Usage, beside the build:

  python3 watchdog.py compilers/results/nods/build.log -- \\
    env BENCH_GROUP=compilers ... systemd-run ... ./build.sh nods "..."

run from the directory the build command runs in (the repository's top).
"""
import argparse
import os
import re
import shutil
import signal
import subprocess
import sys
import time

TICK = os.sysconf('SC_CLK_TCK')


def log(msg):
    print('%s  %s' % (time.strftime('%F %T'), msg), flush=True)


def procs():
    """pid -> (ppid, cpu seconds, cmdline) for every process"""
    out = {}
    for p in os.listdir('/proc'):
        if not p.isdigit():
            continue
        try:
            with open('/proc/%s/stat' % p) as f:
                st = f.read()
            with open('/proc/%s/cmdline' % p, 'rb') as f:
                cmd = f.read().replace(b'\0', b' ').decode(errors='replace')
        except OSError:
            continue
        fields = st[st.rindex(')') + 2:].split()
        out[int(p)] = (int(fields[1]), (int(fields[11]) + int(fields[12])) / TICK, cmd)
    return out


def build_tree(ps, builddir):
    """The build's cabal process (by its --builddir) and its descendants"""
    roots = [p for p, (_, _, c) in ps.items()
             if ' build ' in c and '--builddir=%s ' % builddir in c + ' ' and 'cabal' in c.split()[0]]
    tree, todo = set(), list(roots)
    while todo:
        p = todo.pop()
        if p in tree:
            continue
        tree.add(p)
        todo.extend(q for q, (pp, _, _) in ps.items() if pp == p)
    return roots, tree


def free_gb(path):
    return shutil.disk_usage(path).free / 1e9


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('log', help='results/NAME/build.log of the build to watch')
    ap.add_argument('cmd', nargs=argparse.REMAINDER, help='-- the build command, to restart it')
    ap.add_argument('--idle', type=float, default=10, help='minutes without CPU that count as a hang')
    ap.add_argument('--slow', type=float, default=20, help='minutes per package before logging it')
    ap.add_argument('--restarts', type=int, default=3)
    ap.add_argument('--min-free', type=float, default=5, help='GB free on the store disk before restarting')
    ap.add_argument('--poll', type=float, default=60, help='seconds between checks')
    a = ap.parse_args()
    cmd = a.cmd[1:] if a.cmd[:1] == ['--'] else a.cmd
    results = os.path.dirname(os.path.abspath(a.log))
    name = os.path.basename(results)
    group = os.path.dirname(os.path.dirname(results))
    builddir = 'dist-%s' % name
    exit_code = os.path.join(results, 'exit-code')
    restarts = 0
    started, done, slow, failed = {}, set(), set(), set()
    cpu_seen = {}    # pid -> (cpu seconds, time it last changed)
    pos = 0
    child = None     # a build we restarted ourselves
    log('watching %s (builddir %s); hang = %g min without CPU' % (a.log, builddir, a.idle))
    while True:
        time.sleep(a.poll)
        now = time.time()
        # the log: units started, completed, failed
        try:
            with open(a.log, errors='replace') as f:
                if os.path.getsize(a.log) < pos:
                    pos = 0          # a restarted build truncated it
                f.seek(pos)
                text = f.read()
                pos = f.tell()
        except FileNotFoundError:
            text = ''
        for line in text.splitlines():
            m = re.match(r'(Starting|Completed)\s+(\S+ \(.*\))', line)
            if m:
                if m.group(1) == 'Starting':
                    started[m.group(2)] = now
                else:
                    done.add(m.group(2))
            m = re.match(r'Failed to build (\S+?)\.?(?: |$)', line)
            if m and m.group(1) not in failed:
                failed.add(m.group(1))
                log('FAILED %s' % m.group(1))
        for unit, t in started.items():
            if unit not in done and unit not in slow and now - t > a.slow * 60:
                slow.add(unit)
                log('slow: %s over %g min (still building)' % (unit, a.slow))
        # finished?
        ps = procs()
        roots, tree = build_tree(ps, builddir)
        if not roots:
            if child is not None and child.poll() is None:
                continue     # restarted build not up yet
            if os.path.exists(exit_code):
                with open(exit_code) as f:
                    log('build finished: %s' % f.read().strip())
            else:
                log('build process gone, no exit code')
            return
        # hung?  a process idle too long, or the whole tree idle
        idle = []
        for p in tree:
            cpu = ps[p][1]
            prev = cpu_seen.get(p)
            if prev is None or cpu > prev[0]:
                cpu_seen[p] = (cpu, now)
            elif now - prev[1] > a.idle * 60 and ps[p][2].split()[0].endswith(('ghc', 'ld', 'ld.gold', 'gcc', 'cc')):
                idle.append(p)
        tree_idle = all(now - cpu_seen[p][1] > a.idle * 60 for p in tree)
        if not idle and not tree_idle:
            continue
        what = ', '.join('%d %s' % (p, ps[p][2][:80]) for p in (idle or sorted(tree)[:3]))
        if restarts >= a.restarts or not cmd:
            log('HUNG (%s); %s; leaving it for a person' % (
                what, 'no restarts left' if cmd else 'no command to restart'))
            return
        restarts += 1
        log('HUNG: %s; stopping the build (restart %d of %d)' % (what, restarts, a.restarts))
        for p in tree:
            try:
                os.kill(p, signal.SIGTERM)
            except ProcessLookupError:
                pass
        time.sleep(20)
        for p in build_tree(procs(), builddir)[1]:
            try:
                os.kill(p, signal.SIGKILL)
            except ProcessLookupError:
                pass
        while free_gb(group) < a.min_free:
            log('waiting for disk: %.1f GB free' % free_gb(group))
            time.sleep(300)
        time.sleep(10)       # let build.sh write its exit code
        if os.path.exists(exit_code):
            os.rename(exit_code, exit_code + '.hung%d' % restarts)
        os.rename(a.log, a.log.replace('.log', '-hung%d.log' % restarts)) if os.path.exists(a.log) else None
        log('restarting: %s' % ' '.join(cmd))
        child = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        pos, started, done, slow, cpu_seen = 0, {}, set(), set(), {}


if __name__ == '__main__':
    sys.exit(main())
