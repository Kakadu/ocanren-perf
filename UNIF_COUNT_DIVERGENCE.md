# OCanren vs Racket unification-count divergence: minimal example & root cause

## TL;DR

The unification-count residual on the quine/twine benchmarks is **not** a
disequality-logic bug and **cannot** be reproduced by any isolated relation. It is
an **emergent search-driver effect**: OCanren allocates more fresh logic variables
than faster-miniKanren (Racket) because it enters more `conde` branches — allocating
their `fresh` vars — that are later pruned before they contribute a single
unification. Those extra variables never unify, so the *unification* count can still
match while the *variable-allocation* count diverges.

## Minimal example: `twines n=1`

The smallest case that shows the divergence is the **first** answer of `twines`
(no backtracking past answer 1):

|          | unifications (U) | fresh vars (V) |
|----------|-----------------:|---------------:|
| OCanren  |            16583 |          28443 |
| Racket   |            16583 |          22629 |

- Same answer (structurally identical twine).
- **U matches exactly** (16583 = 16583).
- OCanren allocates **+5814 fresh vars** (28443 vs 22629) that never unify.

Reproduce:

```sh
# OCanren  (from ocanren01/)
./_build/default/unif_count/run_scheme.exe -twines -n 1 -q -dlog /tmp/oc.log
grep -c '^U$'   /tmp/oc.log    # 16583
grep -c '^V '   /tmp/oc.log    # 28443

# Racket   (from unif_count/)
racket ./run.rkt -q --dlog /tmp/rkt.log --twines 1
grep -cE '^U [0-9]' /tmp/rkt.log   # 16583
grep -c '^V '       /tmp/rkt.log   # 22629
```

Past the first answer the unification count also drifts (a search-order artifact):

| benchmark          | U (OC / RKT)      | V (OC / RKT)     |
|--------------------|-------------------|------------------|
| twines n=1         | 16583 / 16583     | 28443 / 22629    |
| twines n=2         | 55721 / 55724     | 94277 / 76345    |
| nodiseq n=1 (pure) | — / 3490          | 4996 / 4304      |

`nodiseq` uses only pure unification plus a relational `neq` (no built-in
disequality), so it isolates the **goal scheduler**: OCanren still allocates +692
vars. This rules out the diseq solver as the cause of the residual.

## Why no smaller hand-written goal reproduces it

A harness (`ocanren01/unif_count/minex.ml`, `ocanren01/unif_count/minex2.ml`,
`unif_count/minex.rkt`) tests candidate goals covering every suspected trigger.
**All match exactly** (same answers, same V count):

| goal                | ingredients                                   | OC V / RKT V |
|---------------------|-----------------------------------------------|:------------:|
| g1–g6               | conde / fresh / `==`                          | equal        |
| `member` (g7)       | recursion + branching                         | 17 / 17      |
| Gterm template (g8) | nested term-template unification              | 2 / 2        |
| `not_in_envo` (g9)  | Gterm + diseq + recursion                     | 14 / 14      |
| `filter` (g10)      | diseq + branching + multi-answer              | 25 / 25      |
| `lookupo` (g3)      | Gterm + diseq + recursion + multi-answer      | 11 / 11      |
| `ma`/`mb` (g12)     | **mutual** recursion + branching + multi-ans. | 17 / 17      |

The divergence appears only in the full `evalo` evaluator — a 4-way **mutual**
recursion (`evalo` ↔ `not_in_envo` ↔ `lookupo` ↔ `proper_listo`) with 5-way `conde`
and specific Gterm template shapes, driven over a large search tree. It is emergent
and cannot be reduced to any single relation.

## Where the extra variables come from

Tracing variable allocation (tagged `fresh` sites) shows OCanren batches fresh
allocations at `conde`-branch entry: it allocates a branch's `fresh` vars as soon as
the branch is entered, even when that branch is subsequently pruned (fails a
unification or a disequality recheck) before producing any unification. Racket's lazy
`mplus*`/`suspend` driver allocates each var right before first use, so pruned
branches cost fewer allocations. Over the large quine/twine search tree this compounds
into thousands of extra (never-unifying) variables in OCanren.

## Notes / dependencies

- The V-logging instrumentation (`Env.fresh_tagged`, tagged `fresh`/`runarg`/`lift`
  sites) and the faithful `=/=*` disequality logic live in the `ocanren01/OCanren`
  **submodule** (separate from this commit). Reproducing the OCanren V-logs requires
  that submodule state.
- The Racket harness (`unif_count/minex.rkt`) is self-contained: it uses
  `mk.rkt` + `src_lisps/q.scm`.
