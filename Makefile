print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 003 005 006 007
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=printf "%10.3f\t" 0.0

#MEASURE_OC1   ?=
#MEASURE_OC2   ?=
MEASURE_OC3   ?= y
#MEASURE_OC4   ?=
MEASURE_OC5   ?= y
#MEASURE_RKT   ?= y
MEASURE_SCM   ?= y
MEASURE_MUSCM ?= y

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
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	sh avg.awk .avg | xargs echo -n >> $(2)
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
# ML OCanren 3 Fancy-speedup
MLOC3F_NATIVE_$(1) := $$(wildcard src_ocanrenfancy3/test$(1)*.native)
TEST$(1)_NAME := $$(MLOC3F_NATIVE_$(1):src_ocanrenfancy3/test$(1)_%.native=%)

measure$(1)_MLOC3F:
$(call DOUBLE_IF_OC_AVG,$$(MLOC3F_NATIVE_$(1)),$(MEASURE_OC3),.$(1).data)

# ML OCanren 5 default + speedup
MLOC5_NATIVE_$(1) := $$(wildcard src_ocanren5def_opt/test$(1)*.native)

measure$(1)_MLOC5:
$(call DOUBLE_IF_OC_AVG,$$(MLOC5_NATIVE_$(1)),$(MEASURE_OC5),.$(1).data)

###################################### finish measuring ocanren ################

# miniKanren in Scheme
SCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.chez.scm)
$$(info SCM_FILE_$(1) $$(SCM_FILE_$(1)) )
SCM_NATIVE_$(1) = $$(SCM_FILE_$(1):.scm=).so
SCM_FILE_$(1)_BASENAME = $$(shell basename $$(SCM_FILE_$(1)))

.PHONY: compile$(1)_scm measure$(1)_scm
ifeq "$$(SCM_FILE_$(1))" ""
compile$(1)_scm:
else
compile_scm: compile$(1)_scm
compile$(1)_scm: $$(SCM_NATIVE_$(1))
$$(SCM_NATIVE_$(1)):
	(cd src_lisps && echo '(compile-file "$$(SCM_FILE_$(1)_BASENAME)")' | scheme -q)
endif

measure$(1)_scm:
$(info SCM_NATIVE_$(1) $$$(SCM_NATIVE_$(1)) )
$(call DOUBLE_IF_RKT_AVG,$$(SCM_FILE_$(1)),$(MEASURE_SCM),.$(1).data,scheme --program $$(SCM_NATIVE_$(1)))

# microKanren in Scheme
MUSCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.mu.scm)
MUSCM_NATIVE_$(1) = $$(MUSCM_FILE_$(1):.scm=).so
MUSCM_FILE_$(1)_BASENAME = $$(shell basename $$(MUSCM_FILE_$(1)))

.PHONY: compile$(1)_muscm measure$(1)_muscm
ifeq "$$(MUSCM_FILE_$(1))" ""
compile$(1)_muscm:
else
compile_muscm: compile$(1)_muscm
compile$(1)_muscm: $$(MUSCM_NATIVE_$(1))
$$(MUSCM_NATIVE_$(1)): $$(MUSCM_FILE_$(1))
	(cd src_lisps  && echo '(compile-file "$$(MUSCM_FILE_$(1)_BASENAME)")' | scheme -q)
endif

measure$(1)_muscm:
$(call DOUBLE_IF_RKT_AVG,$$(MUSCM_FILE_$(1)),$(MEASURE_MUSCM),.$(1).data,scheme --program $$(MUSCM_NATIVE_$(1)))

################################################################################
.PHONY: measure$(1) measure$(1)_prepare do_measure
measure$(1)_prepare:
	$(RM) .$(1).data .$(1).name

do_measure: measure$(1)

measure$(1): measure$(1)_prepare \
	measure$(1)_MLOC3F measure$(1)_MLOC5 measure$(1)_scm measure$(1)_muscm
							# measure$(1)_MLOC1D  measure$(1)_MLOC2F  measure$(1)_MLOC4F  measure$(1)_scm
	printf "$$(TEST$(1)_NAME) " >> $(DATAFILE)
	tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	printf "\n" >> $(DATAFILE)
endef

#$(info $(call XXX,002))
$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

.PHONY: prepare_header do_measure
prepare_header:
	#echo "x     OCanren OCanren2Fancy OCanren3Fancy OCanren4Fancy OCanren5def_opt mini/Racket mini/Scheme micro/Scheme"
	echo "x      OCanren3flat OCanren5nonflat faster-miniKanren/Scheme microKanren/Scheme" \
		> $(DATAFILE)

# prepare_ocanren1default:
# 	$(MAKE) -C ocanren1default -f Makefile.ob all compile_tests
# 	$(MAKE) -C ocanren1default -f Makefile.ob bundle
#
# prepare_ocanren2fancy:
# 	$(MAKE) -C ocanren2fancy -f Makefile.ob all compile_tests
# 	$(MAKE) -C ocanren2fancy -f Makefile.ob bundle

prepare_ocanren3fancy:
	$(MAKE) -C ocanren3fancy -f Makefile.ob all compile_tests
	$(MAKE) -C ocanren3fancy -f Makefile.ob bundle

# prepare_ocanren4fancy:
# 	$(MAKE) -C ocanren4fancy -f Makefile.ob all bundle

prepare_ocanren5def_opt:
	$(MAKE) -C ocanren5def_opt -f Makefile.ob all compile_tests
	$(MAKE) -C ocanren5def_opt -f Makefile.ob bundle

.PHONY: prepare_ocanren1default \
	prepare_ocanren2fancy \
	prepare_ocanren3fancy \
	prepare_ocanren4fancy \
	prepare_ocanren5def_opt \
	prepare_ocanren

prepare_ocanren: \
	prepare_ocanren3fancy \
	prepare_ocanren5def_opt \
	#prepare_ocanren1default \
	#prepare_ocanren2fancy \
	#prepare_ocanren4fancy \


.PHONY: \
	compile_ocanren3fancy_tests \
	compile_ocanren5_tests \
	#compile_ocanren1def_tests \
	#compile_ocanren2fancy_tests \
	#compile_ocanren4fancy_tests \

compile_ocanren1def_tests:
	$(MAKE) -C src_ocanrendefault
compile_ocanren2fancy_tests:
	$(MAKE) -C src_ocanrenfancy
compile_ocanren3fancy_tests:
	$(MAKE) -C src_ocanrenfancy3
compile_ocanren4fancy_tests:
	$(MAKE) -C src_ocanrenfancy4
compile_ocanren5def_tests:
	$(MAKE) -C src_ocanren5def_opt

.PHONY: check_submodules
check_submodules:
#	if [ -d "faster-miniKanren" ]; then (git submodule init && git submodule update --remote); fi
#	if [ -d "microKanren" ];       then (git submodule init && git submodule update --remote); fi
#	if [ -d "ocanrendefault" ];    then (git submodule init && git submodule update --remote); fi
#	if [ -d "ocanrenflat" ];       then (git submodule init && git submodule update --remote); fi

compile: check_submodules
compile: prepare_ocanren \
	compile_ocanren3fancy_tests \
	compile_ocanren5def_tests \
	compile_scm \
	compile_muscm \
	# compile_rkt \
	#compile_ocanren1def_tests \
	#compile_ocanren2fancy_tests \
	#compile_ocanren4fancy_tests \


format_as_column:
	@column -t $(DATAFILE) > .datafile.temp
	@mv .datafile.temp $(DATAFILE)

measure: prepare_header do_measure format_as_column
#$(info $(call XXX,002))
#$(eval $(call XXX,001))
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
	$(MAKE) -C ocanren1default -f Makefile.ob clean
	$(MAKE) -C ocanren2fancy   -f Makefile.ob clean
	$(MAKE) -C ocanren3fancy   -f Makefile.ob clean
	$(MAKE) -C ocanren4fancy   -f Makefile.ob clean
	$(MAKE) -C ocanren5def_opt -f Makefile.ob clean
	$(MAKE) -C src_lisps          clean
	$(MAKE) -C src_ocanrendefault clean
	$(MAKE) -C src_ocanrenfancy   clean
	$(MAKE) -C src_ocanrenfancy3  clean
	$(MAKE) -C src_ocanrenfancy4  clean
