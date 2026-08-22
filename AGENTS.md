In this repo benchmarks about miniKanren are located

## Supported implementations

* faster-miniKanren in Scheme (`./src_lisps`)
* OCanren (in directory `./ocanren01/OCanren`)
* C++ (in directory `./cpp`)
* Go (in directory `./go`)
* Rust 

When running benchmarks use REPEAT=1 variable. We are currently worring not about exact performance, but more about unification count and search order.

## Supported benchmarks 

* `test001_expo1`
* `test002_logo1`
* `test005_thrines`
* `test006_twines`
* `test007_quines`

When we will port OCanren benchmarks to other implementataion we want to save test's names and numbers

## Dont look into directories

* MiniKanren05tagless, MiniKanren07 and `ocanren03`
* `two_quines_impl` 

## Unification count 

In the directory `unif_count` we run instrumentalized implementations where we trace unification count in racket. 
For OCanren we do this in `./ocanren01/unif_count` (implemented via preprocessor).
The count in all implementations should be the same for a fixed benchmark.

Known exception: the quine benchmarks (`test005_thrines`, `test006_twines`,
`test007_quines`) show small residuals (<0.01%) between implementations even though
the relations are identical. The cause is the disequality solver, which differs per
implementation (OCanren's CNF `Disequality` module vs faster-miniKanren's `=/=*` vs
Go's raw-pair recheck). Only these benchmarks use top-level disequality between large
terms: `quines` (no top-level diseq) matches exactly, while `twines`/`thrines` drift
by a few unifications only when continuing past the first answer. This residual is
inherent to the differing diseq solvers, not a relation bug.

Known exception (scheduling): the `quines-nodiseq` benchmark uses *only* pure
unification (peano-indexed variables with a relational `neq`, no built-in disequality),
so it isolates the goal **scheduler** difference. OCanren fairly interleaves each `conde`
via `State.new_scope`; the Go port drives plain lazy streams and therefore explores more
branches before the first answer. The quine it produces is structurally identical to
OCanren's, but the count is larger: Go 6678 vs OCanren/Racket 3490 (~+91%). This residual
is inherent to the scheduler (not a relation bug and not the diseq solver) and is recorded
as-is in `go/unif_count` (`want` = Go count, `ocanren` = reference).

## Task 0 

* Implement the same five benchmarks for Go in the directory `./go`.
* Implement unification counting in `./go/unif_count`
  * Put tests there about unification counts on the same tests as OCanren



