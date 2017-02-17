OB=ocamlbuild -use-ocamlfind -plugin-tag "package(str)"
print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 004 003 005 006 007
EXPRTESTS=$(addprefix expr, $(TESTS))
CTESTS=$(addprefix c, $(TESTS))
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=printf "%10.3f" 0.001

MEASURE_OC1   ?= y
MEASURE_OC2   ?= y
MEASURE_OC3   ?= y
MEASURE_RKT   ?= y
MEASURE_SCM   ?=
MEASURE_MUSCM ?=

.DEFAULT_GOAL := all
#ML_TESTS_TO_COMPILE :=
.PHONY: compile_rkt compile_ml compile_scm compile_muscm\
	measure_rkt measure_ml measure_scm measure_muscm

define XXX
#ocanren 1 default
MLOD_NATIVE_$(1) := $$(wildcard src_ocanrendefault/test$(1)*.native)
TEST$(1)_NAME = $$(MLOD_NATIVE_$(1):src_ocanrendefault/test$(1)_%.native=%)
measure$(1)_ML1OD:
ifneq "$(and $$(MLOD_NATIVE_$(1)),$(MEASURE_OC1))" ""
	export OCAMLRUNPARAM='s=250M,h=250M' &&  $(MEASURE) --append -o .$(1).data $$(MLOD_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif
# ocanren 2 fancy
MLOF_NATIVE_$(1) := $$(wildcard src_ocanrenfancy/test$(1)*.native)
measure$(1)_ML2OF:
ifneq "$(and $$(MLOF_NATIVE_$(1)),$(MEASURE_OC2))" ""
	export OCAMLRUNPARAM='s=250M,h=250M' && $(MEASURE) --append -o .$(1).data $$(MLOF_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif

# ocanren 3 fancy-speedup
MLOF_NATIVE_$(1) := $$(wildcard src_ocanrenfancy3/test$(1)*.native)
measure$(1)_ML3OF:
ifneq "$(and $$(MLOF_NATIVE_$(1)),$(MEASURE_OC3))" ""
	export OCAMLRUNPARAM='s=250M,h=250M' && $(MEASURE) --append -o .$(1).data $$(MLOF_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif


#racket
RKT_FILE_$(1) = $$(wildcard src_lisps/test$(1)*.rkt)
RKT_NATIVE_$(1) = $$(RKT_FILE_$(1)).native
.PHONY: compile$(1)_rkt
ifneq ($$(RKT_FILE_$(1)),)
compile$(1)_rkt: $$(RKT_NATIVE_$(1))
$$(RKT_NATIVE_$(1)):
	raco exe -o $$(RKT_NATIVE_$(1)) $$(RKT_FILE_$(1))
compile_rkt: compile$(1)_rkt
else
compile$(1)_rkt:

endif

measure$(1)_rkt:
ifneq "$(and $$(RKT_FILE_$(1)),$(MEASURE_RKT))" ""
	$(MEASURE) --append -o .$(1).data ./$$(RKT_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif

# miniKanren in Scheme
SCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.chez)
SCM_NATIVE_$(1) = $$(SCM_FILE_$(1):.chez=).so
SCM_FILE_$(1)_BASENAME = $$(shell basename $$(SCM_FILE_$(1)))

ifneq ("$$(SCM_FILE_$(1))","")
.PHONY: compile$(1)_scm
compile$(1)_scm: $$(SCM_NATIVE_$(1))
$$(SCM_NATIVE_$(1)):
	(cd src_lisps && echo '(compile-file "$$(SCM_FILE_$(1)_BASENAME)")' | scheme -q)
compile_scm: compile$(1)_scm
else
compile$(1)_scm:
endif

measure$(1)_scm:
ifneq "$(and $$(SCM_FILE_$(1)),$(MEASURE_SCM))" ""
	$(MEASURE) --append -o .$(1).data scheme --program $$(SCM_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif

# muKanren in Scheme
MUSCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.mu.scm)
MUSCM_NATIVE_$(1) = $$(MUSCM_FILE_$(1):.scm=).so
MUSCM_FILE_$(1)_BASENAME = $$(shell basename $$(MUSCM_FILE_$(1)))

ifneq ("$$(MUSCM_FILE_$(1))","")
.PHONY: compile$(1)_muscm
compile$(1)_muscm: $$(MUSCM_NATIVE_$(1))
$$(MUSCM_NATIVE_$(1)): $$(MUSCM_FILE_$(1))
	(cd src_lisps  && echo '(compile-file "$$(MUSCM_FILE_$(1)_BASENAME)")' | scheme -q)
compile_muscm: compile$(1)_muscm
else
compile$(1)_muscm:
endif

measure$(1)_muscm:
ifneq "$(and $$(MUSCM_FILE_$(1)),$(MEASURE_MUSCM))" ""
	$(MEASURE) --append -o .$(1).data scheme --program $$(MUSCM_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif
################################################################################
.PHONY: measure$(1) measure$(1)_prepare
measure$(1)_prepare:
	$(RM) .$(1).data .$(1).name
measure$(1): measure$(1)_prepare \
							measure$(1)_ML1OD measure$(1)_ML2OF measure$(1)_ML3OF \
							measure$(1)_rkt measure$(1)_scm measure$(1)_muscm
	@printf "$$(TEST$(1)_NAME) " >> $(DATAFILE)
	@tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	@printf "\n" >> $(DATAFILE)
do_measure: measure$(1)

endef

.PHONY: prepare_header do_measure
prepare_header:
	echo "x     OCanren OCanren2Fancy OCanren3Fancy mini/Racket mini/Scheme micro/Scheme" > data.gnuplot

prepare_ocanren1default:
	$(MAKE) -C ocanrendefault -f Makefile.ob all compile_tests bundle
	$(MAKE) -C ocanrendefault -f Makefile.ob bundle

prepare_ocanren2flat:
	$(MAKE) -C ocanrenflat -f Makefile.ob all compile_tests bundle
	$(MAKE) -C ocanrenflat -f Makefile.ob bundle

prepare_ocanren3flat:
	$(MAKE) -C ocanren_fancy2 -f Makefile.ob all compile_tests bundle
	$(MAKE) -C ocanren_fancy2 -f Makefile.ob bundle

.PHONY: prepare_ocanren1default prepare_ocanren2flat prepare_ocanren3flat \
	prepare_ocanren
prepare_ocanren: prepare_ocanren1default prepare_ocanren2flat prepare_ocanren3flat

.PHONY: compile_ocanrendef_tests compile_ocanrenfancy_tests
compile_ocanrendef_tests:
	$(MAKE) -C src_ocanrendefault
compile_ocanrenfancy_tests:
	$(MAKE) -C src_ocanrenfancy

.PHONY: check_submodules
check_submodules:
#	if [ -d "faster-miniKanren" ]; then (git submodule init && git submodule update --remote); fi
#	if [ -d "microKanren" ];       then (git submodule init && git submodule update --remote); fi
#	if [ -d "ocanrendefault" ];    then (git submodule init && git submodule update --remote); fi
#	if [ -d "ocanrenflat" ];       then (git submodule init && git submodule update --remote); fi

compile: check_submodules
compile: prepare_ocanren compile_ocanrendef_tests compile_ocanrenfancy_tests compile_rkt compile_scm compile_muscm
measure: prepare_header do_measure
#$(info $(call XXX,002))
#$(eval $(call XXX,001))
all: compile measure graph
graph:
	gnuplot script.gnuplot && xdg-open graph.pdf

$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

perf:
	$(MAKE) -C ../../ -f Makefile.ob plugin

.PHONY: clean
clean:
	$(RM) *~
	$(MAKE) -C ocanrenflat    -f Makefile.ob clean
	$(MAKE) -C ocanrendefault -f Makefile.ob clean
	$(MAKE) -C src_ocanrendefault clean
	$(MAKE) -C src_ocanrenfancy   clean
