.PHONY: counters counting noncounting

all:

counting:
	$(MAKE) -C ../scheme2 clean counting

noncounting:
	$(MAKE) -C ../scheme2 clean noncounting

counters:
	mkdir -p count
	@$(RM) count/*
	scheme test001_pow.chez.scm && \
	scheme test005_thrines.chez.scm && \
	true

report_counters:
	more count/* | cat
	#https://unix.stackexchange.com/questions/185072
