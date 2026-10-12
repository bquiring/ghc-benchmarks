# Next benchmarks to write

Agreed with the user on 2026-10-11.  Priority order:

1. Finish the current large programs (`large/`: ShellCheck, hledger, dhall;
   pandoc set aside) -- built and timed (ghc-webs WEBS-EXPERIMENTS.md §18).
2. Translate the 3CPS SML benchmarks to Haskell: `llvm-sml/` and
   `higher-order/` (section 2) -- in progress in `sml/`.
3. Then the OCaml and MLton examples (section 2, Priority B, and section 3).
4. Then the other large programs: cabal-install, Futhark, PureScript, Koka
   (section 1).

The user also wants MLton's (and OCaml's) time, instructions and
allocation for the same programs, next to GHC's: compile the SML originals
with MLton (installed: ~/lib/mlton/build/bin/mlton; allocation from
`@MLton gc-summary --`) and the OCaml ones with ocamlopt 4.14 (opam;
allocation from OCAMLRUNPARAM=v=0x400), with the same iteration counts as
the Haskell translations, measured like time.py (perf stat, pinned CPU).

## 1. More Hackage programs (a third group)

| Program | Workload idea | Notes |
|---|---|---|
| cabal-install | solve a large build plan offline (`cabal build --dry-run` against a frozen index snapshot) | dependency solver: search-heavy; should build with 9.15 |
| Futhark | compile its benchmark suite to C (no GPU needed) | real optimising compiler, follows new GHCs quickly |
| PureScript (`purs`) | compile a fixed project (e.g. its core libraries) | large; pinned dependency ranges, expect `allow-newer` work |
| Koka | compile its standard library | research compiler, few dependencies |

## 2. Translations of SML benchmarks to Haskell

Source: `~/projects/3cps/3cps-main/test/programs/` (the user's 3CPS test
suite).  Most come with MLton reference outputs (`*.mlton.out`), so each
translation can be checked against the SML original.

Priority A: higher-order programs not already in nofib (what webs targets):

- `llvm-sml/`: cps-convert, derivative, filter, interpreter, json-decode,
  k-cfa, parser-comb, streams, tardis, twenty-four, y-fact, cpstak, mc-ray,
  raytracer, quicksort, knuth-bendix, ratio-regions (17 programs,
  30-900 lines each)
- `higher-order/`: applicative-functor, callback-state, church-encoding,
  closure-capture, continuation-combinators, cps-explicit, curry-uncurry,
  defunctionalization, dpll-sat, environment-sharing, function-composition,
  higher-order-list, mutual-recursion-ho, polymorphic-ho,
  register-allocator, returned-closures, tree-fold-ho, tree-traversal-ho
  (written to exercise higher-order features)

Priority B: mid-size MLton benchmarks (`from-mlton/`) not in nofib:
logic, model-elimination (CPS-heavy), vliw, tsp, zebra, count-graphs,
smith-normal-form ("snf"), DLXSimulator, barnes-hut, tyan, zern.

Skip: already in nofib in some form (boyer, life, mandelbrot, tak, queens,
simple, fft); too large or replaced by existing Haskell tools (hamlet, an
SML interpreter of ~20k lines; mlyacc and lexgen, where happy and alex are
already benchmarks; fxp); I/O or micro benchmarks (wc-*, output1,
string-concat, vector-*, md5, checksum, imp-for, tailfib, even-odd, fib).

Translation rules, so the Haskell versions measure the same program:

- SML is strict: use strict fields and bang patterns where the SML
  evaluates eagerly, so laziness does not change the algorithm (keep lazy
  only where the SML uses explicit delays, e.g. streams).
- `ref` -> `IORef`/`STRef`; arrays -> `Data.Array`/`Data.Array.ST`;
  exceptions used for control flow -> `Maybe`/`Either` or `Control.Exception`
  where the SML relies on unwinding.
- Only base, containers and array, so they build anywhere (and could later
  go into the webs testsuite or a nofib-style directory).
- Keep the SML input sizes, scaled so a run takes 1-10 s; check output
  against `*.mlton.out`.

## 3. Other functional-language suites (looked at, lower priority)

- OCaml Sandmark: its large programs (Coq, Alt-Ergo, Menhir, Frama-C) are
  not translatable; its small ones overlap nofib and the lists above.
- Larceny/Gabriel (Scheme, in `3cps/.../larceny/`): mostly already in nofib
  (boyer, paraffins, nucleic-like); dynamically typed, costlier to translate.
- Koka/Perceus benchmarks (rbtree, deriv, nqueens, cfold, binarytrees):
  small, and Haskell versions already exist in the Koka repository.
