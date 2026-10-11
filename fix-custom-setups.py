#!/usr/bin/env python3
"""Make the benchmarks' build plan resolve with the in-tree compiler: a
dependency with build-type Custom needs a Setup.hs built against Cabal <
3.14 (cabal-install 3.12's limit), but the compiler ships Cabal 3.15 and the
cached Cabal 3.12 does not build with it.  Such setups are almost always
there for doctests (cabal-doctest), so: unpack the package (its pinned,
cached version), switch it to build-type Simple, drop its custom-setup
stanza, and list it in cabal.project; repeat until the plan resolves.

    python3 fix-custom-setups.py exe:a exe:b ..."""
import os, re, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
targets = sys.argv[1:]
pins = dict(re.findall(r'^\s+(\S+) == ([^\s,]+),?$', open(os.path.join(HERE, 'cached.project')).read(), re.M))


def patch(d, pkg):
    """Unpack package-version d if needed and make its build simple"""
    if not os.path.isdir(os.path.join(HERE, d)):
        subprocess.run(['cabal', 'unpack', d], cwd=HERE, check=True, capture_output=True)
    cf = os.path.join(HERE, d, pkg + '.cabal')
    s = open(cf).read()
    s = re.sub(r'(?im)^(build-type:\s*)Custom', r'\1Simple', s)
    s = re.sub(r'(?im)^custom-setup\s*\n((?:[ \t]+.*\n|\s*\n)*)', '', s)
    open(cf, 'w').write(s)


# --listed: (re)create the packages cabal.project already lists as patched
if targets[:1] == ['--listed']:
    p = open(os.path.join(HERE, 'cabal.project')).read()
    block = p.split('-- build-type changed from Custom (cabal-doctest) to Simple\n', 1)[1]
    for line in block.split('\n'):
        m = re.match(r'\s+(([A-Za-z0-9-]+?)-[0-9][0-9.]*)/$', line)
        if not m:
            break
        patch(m.group(1), m.group(2))
        print('patched', m.group(1))
    sys.exit(0)

for _ in range(40):
    r = subprocess.run(['cabal', 'build', '--offline', '--dry-run'] + targets,
                       cwd=HERE, capture_output=True, text=True)
    out = r.stdout + r.stderr
    if r.returncode == 0:
        print('plan resolves'); print(out[-1500:]); break
    m = re.search(r'rejecting: ([A-Za-z0-9-]+):setup\.Cabal', out)
    if not m:
        print('plan fails for another reason:'); print(out[-3000:]); sys.exit(1)
    pkg = m.group(1)
    ver = pins.get(pkg)
    if ver is None:
        print('no cached version of', pkg); sys.exit(1)
    d = '%s-%s' % (pkg, ver)
    patch(d, pkg)
    pf = os.path.join(HERE, 'cabal.project')
    p = open(pf).read()
    if d + '/' not in p:
        p = p.replace('          -- build-type changed from Custom (cabal-doctest) to Simple\n',
                      '          -- build-type changed from Custom (cabal-doctest) to Simple\n          %s/\n' % d, 1)
        open(pf, 'w').write(p)
    print('patched', d, flush=True)
else:
    print('gave up after 40 rounds'); sys.exit(1)
