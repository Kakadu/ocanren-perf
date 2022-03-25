.PHONY: all clean

all:

define XXX
SCM_FILE_$(1) = $(wildcard test$(1)*.chez.scm)
TEST$(1)_NAME := $$(SCM_FILE_$(1):test$(1)_%.chez.scm=%)

SCM_NATIVE_$(1) = $$(SCM_FILE_$(1):.scm=).so
SCM_FILE_$(1)_BASENAME = $$(shell basename $$(SCM_FILE_$(1)))

all: work$(1).chez.so
work$(1).chez.so:
	cat `basename $$(SCM_FILE_$(1))` > work$(1).chez.scm
	@cat hack.scm | sed 's/FILENAME/$$(SCM_FILE_$(1)_BASENAME)/' >> work$(1).chez.scm
	echo '(compile-file "work$(1).chez.scm")' | scheme -q
	#@$(RM) -v work$(1).chez.scm
endef

TESTS=001 002 003 005 006 007 011
$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

clean:
	$(RM) -r *.rkt.native *.so ./compiled ../faster-miniKanren/compiled
