#!/usr/bin/env python3
"""Build a benchmark's own package as one chunk (Note [Chunks] in ghc-webs'
GHC.WebCore.Chunk): its library's and executable's modules compiled
together by ghc --make -fcore-webs-chunk, against the dependencies that
configuration CONFIG built (dist-CONFIG's plan, the shared store).

    python3 chunk.py GROUP CONFIG PKG EXE MAIN SRCDIR[,SRCDIR..] OUT "GHC OPTIONS"

e.g.  python3 chunk.py large base ShellCheck shellcheck ShellCheck-0.11.0/shellcheck.hs \\
          ShellCheck-0.11.0/src out/chunk-base/shellcheck ""

The package's own library (PKG-*-inplace) is left out of the dependencies:
its modules come from SRCDIR (plus the library's autogen directory, for
Paths_PKG).  -O2 -rtsopts as build.sh; the executable's default-language
and extensions must be given in the options (-XHaskell98 ...)."""
import glob, json, os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
GHC = os.path.expanduser('~/projects/ghc-webs/_build/stage1/bin/ghc')


def main():
    group, config, pkg, exe, main_file, srcdirs, out, opts = sys.argv[1:9]
    gdir = os.path.join(HERE, group)
    plan = json.load(open(os.path.join(gdir, 'dist-%s' % config, 'cache', 'plan.json')))
    units = [u for u in plan['install-plan'] if u.get('pkg-name') == pkg]
    deps, autogen = set(), []
    for u in units:
        if u.get('component-name') in ('lib', 'exe:%s' % exe):
            deps |= set(u.get('depends', []))
    deps = sorted(d for d in deps if not d.startswith(pkg + '-'))
    autogen = glob.glob(os.path.join(gdir, 'dist-%s' % config, 'build', '*', 'ghc-*',
                                     '%s-*' % pkg, 'build', 'autogen'))
    ver = os.path.basename(glob.glob(os.path.join(HERE, 'store', 'ghc-*'))[0])
    dbs = [os.path.join(HERE, 'store', ver, 'package.db'),
           os.path.join(gdir, 'dist-%s' % config, 'packagedb', ver)]
    odir = out + '.build'
    os.makedirs(odir, exist_ok=True)
    cmd = [GHC, '--make', '-fcore-webs-chunk', '-O2', '-rtsopts', '-package-env', '-',
           '-hide-all-packages', '-no-user-package-db']
    for db in dbs:
        cmd += ['-package-db', db]
    for d in deps:
        cmd += ['-package-id', d]
    for d in srcdirs.split(',') + autogen:
        cmd += ['-i' + os.path.join(gdir, d) if not os.path.isabs(d) else '-i' + d]
    if autogen:
        cmd += ['-optP-include', '-optP' + os.path.join(autogen[0], 'cabal_macros.h')]
    cmd += opts.split() + ['-outputdir', odir, '-o', out, os.path.join(gdir, main_file)]
    print(' '.join(cmd), flush=True)
    sys.exit(subprocess.call(cmd, cwd=gdir))


if __name__ == '__main__':
    main()
