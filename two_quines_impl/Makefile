.PHONY: bench clean all
RACO_FLAGS ?=

all: new-quines.log old-quines.log
	grc diff -u old-quines.log new-quines.log

new-quines.log: mk.rkt q_ocanren.scm
	racket q.rkt > $@

old-quines.log: mk.rkt q_vanilla.scm vanilla.rkt
	racket vanilla.rkt > $@

## Benchmarking

bench.exe: bench.rkt q_ocanren.scm q_vanilla.scm wrap-ocanren.rkt wrap-vanilla.rkt
	/usr/bin/time -f '%U seconds' raco exe --cs $(RACO_FLAGS) -o $@ bench.rkt

bench-precompiled: bench.exe
	./$@

bench: bench.rkt q_ocanren.scm q_vanilla.scm wrap-ocanren.rkt wrap-vanilla.rkt
	racket bench.rkt

clean:
	$(RM) *.log *.sexp-* *.exe
