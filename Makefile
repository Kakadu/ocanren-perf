print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 005 006 007 011
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=printf "%10.3f\t" 0.0

MEASURE_OC1   ?=
MEASURE_OC2   ?= y
MEASURE_OC3   ?=
MEASURE_OC4   ?=
MEASURE_OC5   ?=
MEASURE_OC11  ?= y

MEASURE_SCM   ?= y
MEASURE_RKT   ?=
MEASURE_MUSCM ?=

.DEFAULT_GOAL := all
.PHONY: compile_rkt compile_ml compile_scm compile_muscm \
	measure_rkt measure_ml measure_scm measure_muscm

OCAML_GC_CFG=OCAMLRUNPARAM='s=250M,h=250M'
#OCAML_GC_CFG=
####### AVG_MEASURE(what-to-run,where-to-put-avg)
####### we need to measure 20 times to have 1-2% error
define AVG_MEASURE
	$(RM) .avg
	DONT_RUN_CHEZ=y OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
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

# ******************************************************************************
MLOC1_NATIVE_$(1) := $$(wildcard src_ocanren01*/test$(1)*.native)

measure$(1)_MLOC1:
	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC1_NATIVE_$(1)) && cat /tmp/ocanren_time >> .$(1).data

MLOC2_NATIVE_$(1) := $$(wildcard src_ocanren02*/test$(1)*.native)

measure$(1)_MLOC2:
	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC2_NATIVE_$(1)) && cat /tmp/ocanren_time >> .$(1).data

MLOC3_NATIVE_$(1) := $$(wildcard src_ocanren03*/test$(1)*.native)

measure$(1)_MLOC3:
	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC3_NATIVE_$(1)) && cat /tmp/ocanren_time >> .$(1).data

MLOC4_NATIVE_$(1) := $$(wildcard src_ocanren04*/test$(1)*.native)

measure$(1)_MLOC4:
	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC4_NATIVE_$(1)) && cat /tmp/ocanren_time >> .$(1).data

MLOC5_NATIVE_$(1) := $$(wildcard src_ocanren05*/test$(1)*.native)

measure$(1)_MLOC5:
	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC5_NATIVE_$(1)) && cat /tmp/ocanren_time >> .$(1).data

MLOC11_NATIVE_$(1) := $$(wildcard src_ocanren11*/test$(1)*.native)

measure$(1)_MLOC11:
	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC11_NATIVE_$(1)) && cat /tmp/ocanren_time>> .$(1).data

#MLOC6_NATIVE_$(1) := $$(wildcard src_ocanren06*/test$(1)*.native)

#measure$(1)_MLOC6:
#	DONT_RUN_CHEZ=y $(OCAML_GC_CFG) $$(MLOC6_NATIVE_$(1)) && cat /tmp/ocanren_time >> .$(1).data

###################################### finish measuring ocanren ################
.PHONY: measure$(1) measure$(1)_prepare do_measure
measure$(1)_prepare:
	$(RM) .$(1).data .$(1).name

do_measure: measure$(1)

measure$(1): measure$(1)_prepare \
	measure$(1)_MLOC2  \
	measure$(1)_MLOC11 \
	measure$(1)_scm

	printf "$$(TEST$(1)_NAME) " >> $(DATAFILE)
	tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	printf "\n" >> $(DATAFILE)
endef

#$(info $(call XXX,002))
$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

.PHONY: prepare_header do_measure
prepare_header:
	echo "x   2master 11cunif faster-miniKanren/Scheme" \
		> $(DATAFILE)

.PHONY: clean
clean:
	$(RM) *~ .*.data
	$(MAKE) -C src_lisps                 clean

.PHONY: \
	prepare_ocanren

compile: prepare_ocanren

define DO_PREPARE
.PHONY: prepare_ocanren$(1) compile_ocanren$(1)tests clean$(1)
prepare_ocanren$(1):
	$$(MAKE) -C $$(shell echo ocanren$(1)*) all

prepare_ocanren: prepare_ocanren$(1)

compile: compile_ocanren$(1)tests
compile_ocanren$(1)tests:
	$$(MAKE) -C $$(shell echo src_ocanren$(1)*) all

clean$(1):
	$$(MAKE) -C $$(shell echo     ocanren$(1)*) clean
	$$(MAKE) -C $$(shell echo src_ocanren$(1)*) clean
clean: clean$(1)
endef

#$(eval $(call DO_PREPARE,01))
$(eval $(call DO_PREPARE,02))
$(eval $(call DO_PREPARE,11))


compile: compile_scm

format_as_column:
	@column -t $(DATAFILE) > .datafile.temp
	@mv .datafile.temp $(DATAFILE)

measure: prepare_header do_measure format_as_column
all:
	$(MAKE) compile
	$(MAKE) measure graph
graph:
	gnuplot script.gnuplot && xdg-open graph.pdf
