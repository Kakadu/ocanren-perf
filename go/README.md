# OCanren in Go

A Go port of the [OCanren](https://github.com/PLTools/OCanren) (miniKanren) core
and its **thrines** benchmark. It mirrors the C++ port in [`../cpp`](../cpp):
the same data structures, the same search semantics, and the same test suite —
so results are directly comparable across OCaml, C++, and Go.

The logic-programming relations implemented here cover OCanren's own regression
tests: `appendo`/`reverso` (test001), simply-typed lambda-calculus type
inference and evaluation (test005–007), disequality (`=/=`), and the
quine/thrine synthesizer.

## Requirements

- Go **1.22+** (generics are used for the lazy `Stream[T]`).
- Dune (the build is driven by `dune`; see `dune` and `unif_count/dune`).

## Build & run

```sh
dune build                  # build all .exe targets + run the cram count test
dune build @go-build        # go build ./...
dune build @go-vet          # go vet ./...
dune build @go-bench        # run all five benchmark executables
```

`dune build` promotes the benchmark executables into this directory, so they can
be run directly (REPEAT=N to repeat, like OCanren's TimeHelper):

```sh
./test001_expo1.exe    # expo (build_num 3) (build_num 5) q
./test002_logo1.exe    # logo (build_num 243) (build_num 3) q (build_num 0)
./test005_thrines.exe  # find_thrines 2
./test006_twines.exe   # find_twines 30
./test007_quines.exe   # find_quines 200
```

The unification-count test is a standalone executable with a cram test
(`unif_count/unif_count.t`):

```sh
./unif_count/unif_count.exe         # check all 19 cases against ocanren01 counts.t
./unif_count/unif_count.exe -short  # skip the heavy cases
```

Or directly with the Go toolchain:

```sh
go run ./test              # -> "OK: all 146 checks passed"
REPEAT=5 go run ./thrinesbench
```

## Layout

```
mk/            core: SoA term arena, copy-on-write substitution, lazy streams,
               goals (conde/conj/disj/fresh/defer), unify/walk/reify/run
mk/stdx/       appendo, reverso  (OCanren Std.List)
mk/stlc/       simply-typed lambda-calculus constructors + ground names
mk/t5/         lookupo, infero   (regression/test005.ml)
mk/t6/         substo, evalo, a_la_quine  (regression/test006.ml)
mk/t7/         match_lam-based substo, evalo  (regression/test007.ml)
mk/thr/        thrines: Gterm/Gresult evaluator + thrineso synthesizer
test/          the test suite (same checks as ../cpp/test/main.cpp)
thrinesbench/  thrines benchmark (same protocol as ../cpp/test/thrines_bench.cpp)
```

## Design notes

- **Terms are Structure-of-Arrays.** A `Term` is an `int32` index into a global
  arena whose fields live in parallel columns (`kind`, `car`, `cdr`, `a64`,
  `b64`) plus a string pool. A term costs far less than a padded struct and
  same-kind data stays cache-local. Terms are never individually GC'd — they
  live for the whole program and are shared freely between search branches.
- **Substitution is copy-on-write.** A persistent AVL tree: `Extend` copies only
  the O(log n) search path and shares the rest, so every old version stays valid
  and a branching search keeps per-branch substitutions for free. In Go these
  nodes are ordinary heap allocations, so the GC reclaims them (unlike the C++
  bump arena, which never frees).
- **Lazy streams** (`Nil | Cons | Thunk`) with `mplus`/`bind` following
  OCanren's `Stream.ml` exactly.
- **Shared fresh-variable counter.** Each run's `Env` carries a *shared mutable*
  counter (OCanren's `mutable Env.next`), so fresh-variable indices keep
  increasing across all branches of a run. This is what makes the exact-index
  golden tests (e.g. the palindrome test) a strong end-to-end check of search
  order, laziness, and the counter.

## Test suite

`make run` executes **146 checks** — the same queries and expected output as the
C++ suite: copy-on-write substitution, the term arena, `appendo`/`reverso`
(including the exact fresh-variable indices of OCanren's palindrome test),
disequality, and the STLC `lookupo`/`infero`/`substo`/`evalo` relations.

## Performance (find 2 thrines, steady-state avg over 5 runs + warmup)

|          | OCaml | Go    | C++   |
|----------|-------|-------|-------|
| time/run | 0.98 s| 1.25 s| 5.03 s|
| vs OCaml | 1.0×  | 1.28× | 5.1×  |

Go and C++ allocate the **identical** number of terms (7,255,005 over the whole
process), confirming the port reproduces the same search. C++ is ~4× slower than
Go because most of its time is `std::function` type-erasure + `shared_ptr`
refcounting + branch-vector copies rather than logic-programming work; Go's
closures and GC avoid all three. Peak RSS is not directly comparable across the
three (C++ never frees; Go GCs the substitution trees; OCaml fully GCs).
