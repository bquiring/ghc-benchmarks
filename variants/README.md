# Hand-edited variants

Data representation by hand, to bound proposals 6 and 7 of ghc-webs's
WEBS-BACKLOG.md. Results are in ghc-webs's WEBS-EXPERIMENTS.md §34.

| patch | tree, made from the unpacked original | change |
|---|---|---|
| `ShellCheck-0.11.0-v1.patch` | `large/ShellCheck-0.11.0-v1` | VariableValue and VariableState fields strict |
| `ShellCheck-0.11.0-v2.patch` | `large/ShellCheck-0.11.0-v2` | v1, plus UNPACK on the enumeration fields and on `variableValue` |
| `hledger-lib-1.52.4-v1.patch` | `large/hledger-lib-1.52.4-v1` (from the Hackage tarball; the patch includes setup.sh's `.cabal` changes) | Posting fields strict, except `ptransaction` and `poriginal` |
| `koka-3.2.9-v2.patch` | `compilers/koka-3.2.9-v2` | `Range`: UNPACK on Pos and Bool |
| `happy-lib-2.1.3-v.patch` | `happy-lib-2.1.3-v` | a local `lookup` in Tabular/LALR.lhs and First.lhs (`import Prelude hiding (lookup)`), so it is specialised to Int |

To recreate a tree, copy the original to the variant's name, then apply the
patch with `patch -p1` inside it. The project files are `large/scv1.project`,
`large/scv2.project`, `large/hlv1.project`, `compilers/kokav2.project` and
`happyv.project`; git ignores them there,
so copies are in `projects/`. Put each back in its group's directory.
