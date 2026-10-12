# SML benchmarks translated to Haskell

Translations of the benchmarks in the user's 3CPS test suite
(`~/projects/3cps/3cps-main/test/programs/`): first `llvm-sml/` and
`higher-order/`, later the MLton and OCaml examples (see ../NEXT.md).

A benchmark group like `large/`: build with
`BENCH_GROUP=sml ../build.sh NAME "OPTS"` from the repository root, time with
`BENCH_GROUP=sml python3 time.py base ulsl`.  Each program is one executable
of the `smlbench` package (`progs/<Name>.hs`); it prints `pass` or `fail`
like the SML original, and the timing harness checks the outputs agree.

Translation rules (so the Haskell measures the same program):

- The same algorithm and data structures as the SML, function by function,
  written as ordinary lazy Haskell: no strictness annotations, so the
  compiler's strictness analysis (GHC's and the webs pass's) has to find
  what SML evaluates eagerly.  Only the harness forces each round's result
  completely (`Repeat.loopLast`), so no round's work is skipped.
- Each program repeats its work N times (default chosen for ~1-1.5 s with
  the base build; the first argument overrides it).  Each iteration's input
  goes through `Repeat.vary`, which the optimiser cannot see through, so the
  work cannot be floated out of the loop and shared.
- SML functions taking a tuple are curried (Haskell's calling convention);
  tuples that are values (stored, returned) stay tuples.
- `ref` -> `IORef`/`STRef` (impure SML code runs in `ST`, with `unit -> a`
  thunks as `ST` actions), arrays -> `Data.Array`.  Exceptions raised and
  handled inside pure code (knuth-bendix's Fail) -> `Maybe`; exceptions in
  effectful code (derivative) -> Haskell exceptions.  Code that prints as
  it computes runs in `IO`, writing to a per-round buffer.  Only base,
  containers, array and deepseq.
- Each program prints what its SML original prints; where the SML prints
  per round, the translation prints one round's lines (if every round's
  are the same).  `tools/check-all.sh` compares the outputs with the MLton
  executables' (`*.mlton.out` in the 3cps suite).

Programs and their defaults are listed in `tools/gen.py`, which writes
`smlbench.cabal` and `benchmarks.txt`.
