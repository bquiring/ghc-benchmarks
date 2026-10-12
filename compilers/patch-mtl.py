#!/usr/bin/env python3
# mtl 2.3 no longer re-exports Control.Monad, Control.Monad.Fix and (from the
# Writer modules) Data.Monoid from its modules (Control.Monad.State etc.).
# For each Haskell file given, move the names such modules' import lists
# take from those into imports of their own.  Idempotent: run by setup.sh
# on PureScript's sources.
import re
import sys

CONTROL_MONAD = set("""
MonadPlus mplus mzero mapM mapM_ forM forM_ sequence sequence_ =<< >=> <=<
forever void join msum mfilter filterM mapAndUnzipM zipWithM zipWithM_ foldM
foldM_ replicateM replicateM_ guard when unless liftM liftM2 liftM3 liftM4
liftM5 ap <$!>
""".split())
FIX = {"fix": "Data.Function", "MonadFix": "Control.Monad.Fix"}
FIX.update((n, "Data.Monoid") for n in "Dual Endo All Any Sum Product First Last Alt Ap".split())
IMPORT = re.compile(r"^import (Control\.Monad\.(?:Reader|State|Writer|Except|RWS|Error|Cont)[\w.]*) \((.*)\)\s*$")


def split_items(lst):
    items, depth, cur = [], 0, ""
    for c in lst:
        if c == "," and depth == 0:
            items.append(cur.strip())
            cur = ""
            continue
        depth += c == "("
        depth -= c == ")"
        cur += c
    if cur.strip():
        items.append(cur.strip())
    return items


def base(item):
    name = re.sub(r"\(\.\.\)$", "", item).strip()
    return name[1:-1] if name.startswith("(") else name


for path in sys.argv[1:]:
    lines = open(path).read().split("\n")
    out, changed = [], False
    for line in lines:
        m = IMPORT.match(line)
        if not m:
            out.append(line)
            continue
        keep, moved = [], {}
        for item in split_items(m.group(2)):
            b = base(item)
            if b in CONTROL_MONAD:
                moved.setdefault("Control.Monad", []).append(item)
            elif b in FIX and (FIX[b] != "Data.Monoid" or ".Writer" in m.group(1) or ".RWS" in m.group(1)):
                moved.setdefault(FIX[b], []).append(item)
            else:
                keep.append(item)
        if not moved:
            out.append(line)
            continue
        changed = True
        if keep:
            out.append("import %s (%s)" % (m.group(1), ", ".join(keep)))
        for mod, items in moved.items():
            out.append("import %s (%s)" % (mod, ", ".join(items)))
    if changed:
        open(path, "w").write("\n".join(out))
        print("patched", path)
