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

- The same algorithm and data structures as the SML, function by function.
  SML is strict: strict arithmetic, and strict fields and bang patterns
  where the SML evaluates eagerly; laziness only where the SML delays.
- Each program repeats its work N times (default chosen for ~1-1.5 s with
  the base build; the first argument overrides it).  Each iteration's input
  goes through `Repeat.vary`, which the optimiser cannot see through, so the
  work cannot be floated out of the loop and shared.
- SML functions taking a tuple are curried (Haskell's calling convention);
  tuples that are values (stored, returned) stay tuples.
- `ref` -> `IORef`/`STRef` (impure SML code runs in `ST`, with `unit -> a`
  thunks as `ST` actions), arrays -> `Data.Array`; exceptions as Haskell
  exceptions where the SML raises and handles them.  Only base,
  containers, array and deepseq.
- Each program prints exactly what its SML original prints.
  `tools/check.sh Name sml-name [N]` compares the output with the MLton
  executable's (`*.mlton.out` in the 3cps suite); programs repeated beyond
  the SML's own loop are compared at N = 1 (see check.sh's callers in
  tools/check-all.sh).

Programs and their defaults are listed in `tools/gen.py`, which writes
`smlbench.cabal` and `benchmarks.txt`.
