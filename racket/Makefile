quines.exe: quines.rkt
	raco exe -v --cs -o $@ $^

.PHONY: quines.sexp
quines.sexp: quines.exe
	@$(RM) $@
	BENCH_MODE=1 ./$^

clean:
	$(RM) quines.sexp-* quines.exe