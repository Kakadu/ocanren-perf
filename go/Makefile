# Go port of the OCanren (miniKanren) core + the five ocanren-bench benchmarks.
# `make run` runs the 146-check test suite, `make count` runs the unification-
# count tests (vs OCanren's counts.t), `make bench` runs the thrines benchmark.
# The named benchmarks live in ./cmd/testNNN_* (REPEAT=N to repeat).

GO ?= go

all: build

build:
	$(GO) build ./...

vet:
	$(GO) vet ./...

run:
	$(GO) run ./test

# Unification-count tests against ocanren01/unif_count/counts.t. Add -short to
# skip the heavy searches (exp3x5, exp7x2, logo243base3, twines10, thrines2).
count:
	$(GO) test ./unif_count

bench:
	$(GO) run ./thrinesbench

clean:
	$(GO) clean ./...

.PHONY: all build vet run count bench clean
