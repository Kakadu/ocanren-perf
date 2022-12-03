print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 005 #006 007 011
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=printf "%10.3f\t" 0.0

MEASURE_OC1   ?= y
MEASURE_OC2   ?=
MEASURE_OC3   ?=
MEASURE_OC4   ?=
MEASURE_OC5   ?=
MEASURE_OC9   ?=
MEASURE_SCM   ?= y
MEASURE_RKT   ?=
MEASURE_MUSCM ?=

.DEFAULT_GOAL := all
.PHONY: celan do_measure
.PHONY: compile_rkt compile_ml compile_scm compile_muscm \
	measure_rkt measure_ml measure_scm measure_muscm

celan: clean

OCAML_GC_CFG=OCAMLRUNPARAM='s=250M,h=250M'
#OCAML_GC_CFG=
####### AVG_MEASURE(what-to-run,where-to-put-avg)
####### we need to measure 20 times to have 1-2% error
# define AVG_MEASURE
# 	$(RM) .avg
# 	BENCH_MODE=y OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
# 	#OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
# 	#OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
# 	#OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
# 	#OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
# 	@sh avg.awk .avg | xargs echo -n >> $(2)
# 	@echo -n " " >> $(2)
# 	@$(RM)  .avg
# endef

########## DOUBLE_IF(NATIVE_FILE,IS_ENABLED_FLAG,DATA_FILE,MEASURE_ACTION)
# define DOUBLE_IF
# ifeq "$(1)" ""
# 	$(DUMMY_MEASURE) >> $(3)
# else
# ifeq "$(2)" ""
# 	$(DUMMY_MEASURE) >> $(3)
# else
# 	$(4)
# endif
# endif
# endef

# define DOUBLE_IF_OC_AVG
# $(call DOUBLE_IF,$1,$2,$3,$(call AVG_MEASURE,$(1),$(3)))
# endef

# dir info
define ADD_RACKET_TEST # test_name Racket_dir
SCM_$(2)_FILE_$(1) := $$(wildcard $(2)/test$(1)*.chez.scm)
SCM_$(2)_NAME_$(1) := $$(SCM_$(2)_FILE_$(1):$(2)/test$(1)_%.chez.scm=%)

SCM_$(2)_NATIVE_$(1)   := $$(SCM_$(2)_NAME_$(1):.scm=).so
SCM_$(2)_BASENAME_$(1) := $$(shell basename $$(SCM_$(2)_FILE_$(1)))

# TODO: Support cases when some tests may not exist for OCanren/Racket/Scheme
# .PHONY: compile$(1)_scm measure$(1)_scm
# ifeq "$$(SCM_FILE_$(1))" ""
# compile$(1)_scm:
# else
compile_$(2): compile$(2)_FILE_$(1)
compile_$(2)_FILE_$(1): $$(SCM_$(2)_NATIVE_$(1))
$$(SCM_$(2)_NATIVE_$(1)):
	$(MAKE) -C $(2) work$(1).chez.so
	(cd $(2) && cat `basename $$(SCM_$(2)_FILE_$(1))` > work$(1).chez.scm && \
		cat hack.scm | sed 's/FILENAME/$$(SCM_FILE_$(1)_BASENAME)/' >> work$(1).chez.scm && \
		echo '(compile-file "work$(1).chez.scm")' | scheme -q && rm -v work$(1).chez.scm)

measure_scheme_$(2)_test_$(1):
	@echo measure_scheme_$(2)_test_$(1)
	(cd $(2) && BENCH_MODE=y scheme --program "work$(1).chez.so" >> ../.$(1).data)
	cat .$(1).data

measure_scheme_$(2)_targets := $$(measure_scheme_$(2)_targets) measure_scheme_$(2)_test_$(1)
#$$(info $$(measure_scheme_$(2)_targets) )
endef

define ADD_RACKET # dir Scheme_name
ALL_GNUPLOT_HEADERS += " $(2)"

measure_scheme_$(1)_targets :=
$(foreach i, $(TESTS), $(eval $(call ADD_RACKET_TEST,$(i),$(1)) ) )

.PHONY: measure_scheme_$(1)
measure_scheme_$(1): $(measure_scheme_$(1)_targets)
	@#echo "Measuring of $(2) in $(1) finished"
do_measure: measure_scheme_$(1)
endef

define ADD_OCANREN_TEST # test_name OCanren_name
MLOC_$(2)_NATIVE_$(1) := $$(wildcard $(2)/test$(1)*.ml)
MLOC_$(2)_NATIVE_EXE_$(1) := $$(MLOC_$(2)_NATIVE_$(1):.ml=.exe)
MLOC_$(2)_NATIVE_EXE_$(1) := $$(shell echo $$(MLOC_$(2)_NATIVE_EXE_$(1)) | cut -f2- -d/)

.PHONY: measure_MLOC_$(2)_$(1)
measure_MLOC_$(2)_$(1):
	cd `realpath $$(MLOC_$(2)_DIR)` && BENCH_MODE=y $(OCAML_GC_CFG) dune exec ./$$(MLOC_$(2)_NATIVE_EXE_$(1)) && cat /tmp/ocanren_time >> ../.$(1).data
	cat .$(1).data
	@echo measure_MLOC_$(1)_$(2) finished

measure_MLOC_$(2)_targets := $$(measure_MLOC_$(2)_targets) measure_MLOC_$(2)_$(1)
endef

define ADD_OCANREN # dir OCanren_name
ALL_GNUPLOT_HEADERS += " $(2)"
MLOC_$(1)_DIR := $(2)

measure_MLOC_$(1)_targets :=
$(foreach i, $(TESTS), $(eval $(call ADD_OCANREN_TEST,$(i),$(2)) ) )
#$$(info ==== OCanren tests targets $(measure_MLOC_$(2)_targets) )

.PHONY: measure_ocanren_$(1) measure_ocanren_$(1)_prepare do_measure
measure_ocanren_$(1)_prepare:
	@$(RM) .$(1).data .$(1).name
measure_ocanren_$(1): $(measure_MLOC_$(2)_targets)
	@#echo "measure_ocanren_$(1) finished"
do_measure: measure_ocanren_$(1)

endef

.PHONY: prepare_header do_measure
prepare_header:
	echo "x\t$(ALL_GNUPLOT_HEADERS)" > $(DATAFILE)

.PHONY: clean
clean:
	$(RM) *~ .*.data

.PHONY: prepare_ocanren
compile: prepare_ocanren

define DO_PREPARE_OCANREN # dirname
.PHONY: prepare_ocanren$(1) compile_$(1)tests clean$(1)
prepare_ocanren$(1):
	cd $$(shell echo $(1)*) && \
		dune build @all_tests --profile=release -j2

prepare_ocanren: prepare_ocanren$(1)

compile: compile_ocanren$(1)tests
compile_ocanren$(1)tests:
	cd $$(MLOC_$(1)_DIR) && dune build @all_tests --profile=release

clean_MLOC_$(1):
	cd $(1) && dune clean

clean: clean_MLOC_$(1)
endef

define DO_PREPARE_SCHEME # dirname
.PHONY: prepare_scheme$(1) compile_scheme$(1)_tests clean_scheme_$(1)
prepare_scheme$(1):
	$(MAKE) -C $(1)

compile: prepare_scheme$(1)
clean: clean_scheme_$(1)
clean_scheme_$(1):
	$(MAKE) -C $(1) clean
endef

$(eval $(call ADD_OCANREN,ocanren01,ocanren01))
#$(eval $(call ADD_OCANREN,ocanren02,ocanren02))
$(eval $(call ADD_RACKET,src_lisps,faster-miniKanren))

$(eval $(call DO_PREPARE_OCANREN,ocanren01))
#$(eval $(call DO_PREPARE_OCANREN,ocanren02))
$(eval $(call DO_PREPARE_SCHEME,src_lisps))
#$(eval $(call DO_PREPARE,02))
#$(eval $(call DO_PREPARE,03))
#$(eval $(call DO_PREPARE,04))
#$(eval $(call DO_PREPARE,5))

compile: compile_scm

define PREPARE_DATAFILE # testname
.PHONY: process_datafile_$(1) clean_datafile_$(1)
process_datafile_$(1): .$(1).data
	@printf "$(1)\t" >> $(DATAFILE)
	@tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	@printf "\n" >> $(DATAFILE)
$$(DATAFILE): process_datafile_$(1)
clean_datafile_$(1):
	@$(RM) .$(1).data
clean_datafiles: clean_datafile_$(1)
endef

$(DATAFILE): prepare_header

$(foreach i, $(TESTS), $(eval $(call PREPARE_DATAFILE,$(i)) ) )

clean_datafiles:
	$(RM) $(DATAFILE)

format_as_column: $(DATAFILE)
	@column -t $(DATAFILE) > .datafile.temp
	@mv .datafile.temp $(DATAFILE)
	@cat $(DATAFILE)

measure: clean_datafiles prepare_header do_measure format_as_column
all:
	$(MAKE) compile
	$(MAKE) measure graph

# TODO: autogenerate script.gnuplot
graph:
	gnuplot script.gnuplot && xdg-open graph.pdf
