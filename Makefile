print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 005 006 007 011
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=printf "%10.3f\t" 0.0

MEASURE_OC9   ?= y
MEASURE_OC10  ?= y
MEASURE_SCM   ?= y
MEASURE_RKT   ?=
MEASURE_MUSCM ?=

.DEFAULT_GOAL := all
.PHONY: compile_rkt compile_ml compile_scm compile_muscm \
	measure_rkt measure_ml measure_scm measure_muscm

####### AVG_MEASURE(what-to-run,where-to-put-avg)
####### we need to measure 20 times to have 1-2% error
define AVG_MEASURE
	$(RM) .avg
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	#OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	#OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	@sh avg.awk .avg | xargs echo -n >> $(2)
	@echo -n " " >> $(2)
	@$(RM)  .avg
endef

########## DOUBLE_IF(NATIVE_FILE,IS_ENABLED_FLAG,DATA_FILE,MEASURE_ACTION)
define DOUBLE_IF
ifeq "$(1)" ""
	$(DUMMY_MEASURE) >> $(3)
else
ifeq "$(2)" ""
	$(DUMMY_MEASURE) >> $(3)
else
	$(4)
endif
endif
endef

########## DOUBLE_IF_RKT_AVG(SOURCE,FLAG,DATA_FILE,EXE_FILE)
define DOUBLE_IF_RKT_AVG
ifeq "$(1)" ""
	$(DUMMY_MEASURE) >> $(3)
else
ifeq "$(2)" ""
	$(DUMMY_MEASURE) >> $(3)
else
	$(call AVG_MEASURE,$(4),$(3))
endif
endif
endef

# TEST_OCAMLRUNPARAM=OCAMLRUNPARAM='s=250M,h=250M'
define DOUBLE_IF_OC
$(call DOUBLE_IF,$1,$2,$3,$$(TEST_OCAMLRUNPARAM) $$(MEASURE) --append -o $(3) $(1) )
endef
define DOUBLE_IF_OC_AVG
$(call DOUBLE_IF,$1,$2,$3,$(call AVG_MEASURE,$(1),$(3)))
endef

# https://www.gnu.org/software/make/manual/html_node/Conditional-Functions.html
define XXX

# miniKanren in Scheme
SCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.chez.scm)
TEST$(1)_NAME := $$(SCM_FILE_$(1):src_lisps/test$(1)_%.chez.scm=%)

SCM_NATIVE_$(1) = $$(SCM_FILE_$(1):.scm=).so
SCM_FILE_$(1)_BASENAME = $$(shell basename $$(SCM_FILE_$(1)))

.PHONY: compile$(1)_scm measure$(1)_scm
ifeq "$$(SCM_FILE_$(1))" ""
compile$(1)_scm:
else
compile_scm: compile$(1)_scm
compile$(1)_scm: $$(SCM_NATIVE_$(1))
$$(SCM_NATIVE_$(1)):
	(cd src_lisps && cat `basename $$(SCM_FILE_$(1))` > work$(1).chez.scm && cat hack.scm | sed 's/FILENAME/$$(SCM_FILE_$(1)_BASENAME)/' >> work$(1).chez.scm && echo '(compile-file "work$(1).chez.scm")' | scheme -q && rm -v work$(1).chez.scm)
endif

measure$(1)_scm:
	(cd src_lisps && DONT_RUN_CHEZ=y scheme --program "work$(1).chez.so" >> ../.$(1).data)

# ML OCanren 9 = 6 + 4 + 5
MLOC9_NATIVE_$(1) := $$(wildcard src_ocanren9same-steams+2opts/test$(1)*.native)

measure$(1)_MLOC9:
$(call DOUBLE_IF_OC_AVG,$$(MLOC9_NATIVE_$(1)),$(MEASURE_OC9),.$(1).data)

# ML OCanren 10 = 9 + tagfull
MLOC10_NATIVE_$(1) := $$(wildcard src_ocanren10tagful/test$(1)*.native)

measure$(1)_MLOC10:
$(call DOUBLE_IF_OC_AVG,$$(MLOC10_NATIVE_$(1)),$(MEASURE_OC10),.$(1).data)

###################################### finish measuring ocanren ################


# simple-miniKanren in Scheme
compile_simple_scm:
SIMPLESCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.simplechez.scm)
SIMPLESCM_NATIVE_$(1) = $$(SIMPLESCM_FILE_$(1):.scm=).so
SIMPLESCM_FILE_$(1)_BASENAME = $$(shell basename $$(SIMPLESCM_FILE_$(1)))

.PHONY: compile$(1)_simple_scm measure$(1)_simple_scm
ifeq "$$(SIMPLESCM_FILE_$(1))" ""
compile$(1)_simple_scm:
else
compile_simple_scm: compile$(1)_simple_scm
compile$(1)_simple_scm: $$(SIMPLESCM_NATIVE_$(1))
$$(SIMPLESCM_NATIVE_$(1)): $$(SIMPLESCM_FILE_$(1))
	(cd src_lisps && echo '(compile-file "$$(SIMPLESCM_FILE_$(1)_BASENAME)")' | scheme -q)
endif

measure$(1)_simple_scm:
$(call DOUBLE_IF_RKT_AVG,$$(SIMPLESCM_FILE_$(1)),$(MEASURE_SCM),.$(1).data,scheme --program $$(SIMPLESCM_NATIVE_$(1)))

################################################################################
.PHONY: measure$(1) measure$(1)_prepare do_measure
measure$(1)_prepare:
	$(RM) .$(1).data .$(1).name

do_measure: measure$(1)

measure$(1): measure$(1)_prepare \
	measure$(1)_MLOC9 measure$(1)_MLOC10 measure$(1)_scm

	printf "$$(TEST$(1)_NAME) " >> $(DATAFILE)
	tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	printf "\n" >> $(DATAFILE)
endef

#$(info $(call XXX,002))
$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

.PHONY: prepare_header do_measure
prepare_header:
	echo "x      ocanren9 ocanren10=9+tagful faster-miniKanren/Scheme" \
		> $(DATAFILE)

prepare_ocanren9same-steams+2opts:
	$(MAKE) -C ocanren9same-steams+2opts all
	$(MAKE) -C ocanren9same-steams+2opts bundle

prepare_ocanren10tagful:
	$(MAKE) -C ocanren10tagful all
	$(MAKE) -C ocanren10tagful bundle


.PHONY: \
	prepare_ocanren9same-steams+2opts \
	prepare_ocanren10tagful \
	prepare_ocanren

prepare_ocanren: \
	prepare_ocanren9same-steams+2opts \
	prepare_ocanren10tagful \


.PHONY: \
	compile_ocanren9tests \
	compile_ocanren10tests \

compile_ocanren9tests:
	$(MAKE) -C src_ocanren9same-steams+2opts all

compile_ocanren10tests:
	$(MAKE) -C src_ocanren10tagful all

compile: prepare_ocanren \
	compile_ocanren9tests \
	compile_ocanren10tests \
	compile_scm \

format_as_column:
	@column -t $(DATAFILE) > .datafile.temp
	@mv .datafile.temp $(DATAFILE)

measure: prepare_header do_measure format_as_column
all:
	$(MAKE) compile
	$(MAKE) measure graph
graph:
	gnuplot script.gnuplot && xdg-open graph.pdf

perf:
	$(MAKE) -C ../../ -f Makefile.ob plugin

.PHONY: clean
clean:
	$(RM) *~ .*.data
	$(MAKE) -C ocanren9same-steams+2opts clean
	$(MAKE) -C ocanren10tagful           clean
	$(MAKE) -C src_lisps                 clean
	$(MAKE) -C src_ocanren9same-steams+2opts clean
	$(MAKE) -C src_ocanren10tagful     clean
