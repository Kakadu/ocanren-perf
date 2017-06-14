print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 003 005 006 007
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=printf "%10.3f\t" 0.0

MEASURE_OC1   ?= y
MEASURE_OC2   ?=
MEASURE_OC3   ?= y
MEASURE_OC4   ?=
MEASURE_OC5   ?= y
MEASURE_OC6   ?=
MEASURE_OC7   ?= y
MEASURE_OC8   ?=
MEASURE_OC9   ?= y
MEASURE_RKT   ?=
MEASURE_SCM   ?= y
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
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
	OCAMLRUNPARAM='s=250M,h=250M' $(MEASURE) --append -o .avg $(1)
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
# ML OCanren 1 Fancy-speedup
MLOC1_NATIVE_$(1) := $$(wildcard src_ocanren1master/test$(1)*.native)
TEST$(1)_NAME := $$(MLOC1_NATIVE_$(1):src_ocanren1master/test$(1)_%.native=%)

measure$(1)_MLOC1:
$(call DOUBLE_IF_OC_AVG,$$(MLOC1_NATIVE_$(1)),$(MEASURE_OC1),.$(1).data)

# ML OCanren 2 patricia trees
MLOC2_NATIVE_$(1) := $$(wildcard src_ocanren2patTree/test$(1)*.native)

measure$(1)_MLOC2:
$(call DOUBLE_IF_OC_AVG,$$(MLOC2_NATIVE_$(1)),$(MEASURE_OC2),.$(1).data)

# ML OCanren 3 MK streams
MLOC3_NATIVE_$(1) := $$(wildcard src_ocanren3MKstreams/test$(1)*.native)

measure$(1)_MLOC3:
$(call DOUBLE_IF_OC_AVG,$$(MLOC3_NATIVE_$(1)),$(MEASURE_OC3),.$(1).data)

# ML OCanren 4 fastDiseq
MLOC4_NATIVE_$(1) := $$(wildcard src_ocanren4fastDiseq/test$(1)*.native)

measure$(1)_MLOC4:
$(call DOUBLE_IF_OC_AVG,$$(MLOC4_NATIVE_$(1)),$(MEASURE_OC4),.$(1).data)

# ML OCanren 5 set-var-val!
MLOC5_NATIVE_$(1) := $$(wildcard src_ocanren5set-var-val/test$(1)*.native)

measure$(1)_MLOC5:
$(call DOUBLE_IF_OC_AVG,$$(MLOC5_NATIVE_$(1)),$(MEASURE_OC5),.$(1).data)

# ML OCanren 6 same-streams
MLOC6_NATIVE_$(1) := $$(wildcard src_ocanren6same-streams/test$(1)*.native)

measure$(1)_MLOC6:
$(call DOUBLE_IF_OC_AVG,$$(MLOC6_NATIVE_$(1)),$(MEASURE_OC6),.$(1).data)

# ML OCanren 7 same-streams + more-inline
MLOC7_NATIVE_$(1) := $$(wildcard src_ocanren7more-inline/test$(1)*.native)

measure$(1)_MLOC7:
$(call DOUBLE_IF_OC_AVG,$$(MLOC7_NATIVE_$(1)),$(MEASURE_OC7),.$(1).data)
###################################### finish measuring ocanren ################

# ML OCanren 8 = 3 + 4 + 5 with some bugs
MLOC8_NATIVE_$(1) := $$(wildcard src_ocanren8dumb_and2opts/test$(1)*.native)

measure$(1)_MLOC8:
$(call DOUBLE_IF_OC_AVG,$$(MLOC8_NATIVE_$(1)),$(MEASURE_OC8),.$(1).data)

# ML OCanren 9 = 6 + 4 + 5
MLOC9_NATIVE_$(1) := $$(wildcard src_ocanren9same-steams+2opts/test$(1)*.native)

measure$(1)_MLOC9:
$(call DOUBLE_IF_OC_AVG,$$(MLOC9_NATIVE_$(1)),$(MEASURE_OC9),.$(1).data)

###################################### finish measuring ocanren ################

# miniKanren in Scheme
SCM_FILE_$(1) = $(wildcard src_lisps/test$(1)*.chez.scm)
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
$(call DOUBLE_IF_RKT_AVG,$$(SCM_FILE_$(1)),$(MEASURE_SCM),.$(1).data,scheme --program $$(SCM_NATIVE_$(1)))

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
	measure$(1)_MLOC1 measure$(1)_MLOC3 measure$(1)_MLOC4 measure$(1)_MLOC5 measure$(1)_MLOC6 measure$(1)_MLOC7 \
	measure$(1)_MLOC8 measure$(1)_MLOC9 measure$(1)_scm #measure$(1)_simple_scm

	printf "$$(TEST$(1)_NAME) " >> $(DATAFILE)
	tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	printf "\n" >> $(DATAFILE)
endef

#$(info $(call XXX,002))
$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

.PHONY: prepare_header do_measure
prepare_header:
	echo "x      OCanren1master Ocanren3MKstreams ocanren4fastDiseq Ocanren5Mset-var-val ocanren6streamsAgain ocanren7streamsAgain+more-inline ocanren8MKstreams+set-var-val+fastDiseq ocanren9 faster-miniKanren/Scheme" \
		> $(DATAFILE)

prepare_ocanren1master:
	$(MAKE) -C ocanren-master all
	$(MAKE) -C ocanren-master bundle

prepare_ocanren2patTree:
	$(MAKE) -C ocanren2patTree all
	$(MAKE) -C ocanren2patTree bundle

prepare_ocanren3MKstreams:
	$(MAKE) -C ocanren3MKstreams all
	$(MAKE) -C ocanren3MKstreams bundle

prepare_ocanren4fastDiseq:
	$(MAKE) -C ocanren4fastDiseq all
	$(MAKE) -C ocanren4fastDiseq bundle

prepare_ocanren5setvarval:
	$(MAKE) -C ocanren5set-var-val all
	$(MAKE) -C ocanren5set-var-val bundle

prepare_ocanren6same-streams:
	$(MAKE) -C ocanren6same-streams all
	$(MAKE) -C ocanren6same-streams bundle

prepare_ocanren7more-inline:
	$(MAKE) -C ocanren7more-inline all
	$(MAKE) -C ocanren7more-inline bundle

prepare_ocanren8dumb_and2opts:
	$(MAKE) -C ocanren8dumb_and2opts all
	$(MAKE) -C ocanren8dumb_and2opts bundle

prepare_ocanren9same-steams+2opts:
	$(MAKE) -C ocanren9same-steams+2opts all
	$(MAKE) -C ocanren9same-steams+2opts bundle


.PHONY: prepare_ocanren1master \
	prepare_ocanren2patTree \
	prepare_ocanren3MKstreams \
	prepare_ocanren4fastDiseq \
	prepare_ocanren5setvarval \
	prepare_ocanren6same-streams \
	prepare_ocanren7more-inline \
	prepare_ocanren8dumb_and2opts \
	prepare_ocanren9same-steams+2opts \
	prepare_ocanren

prepare_ocanren: \
	prepare_ocanren1master \
	prepare_ocanren2patTree \
	prepare_ocanren3MKstreams \
	prepare_ocanren4fastDiseq \
	prepare_ocanren5setvarval \
	prepare_ocanren6same-streams \
	prepare_ocanren7more-inline \
	prepare_ocanren8dumb_and2opts \
	prepare_ocanren9same-steams+2opts \


.PHONY: \
	compile_ocanren1tests \
	compile_ocanren2tests \
	compile_ocanren3tests \
	compile_ocanren4tests \
	compile_ocanren5tests \
	compile_ocanren6tests \
	compile_ocanren7tests \
	compile_ocanren8tests \
	compile_ocanren9tests \

compile_ocanren1tests:
	$(MAKE) -C src_ocanren1master all
compile_ocanren2tests:
	$(MAKE) -C src_ocanren2patTree all
compile_ocanren3tests:
	$(MAKE) -C src_ocanren3MKstreams all
compile_ocanren4tests:
	$(MAKE) -C src_ocanren4fastDiseq all
compile_ocanren5tests:
	$(MAKE) -C src_ocanren5set-var-val all
compile_ocanren6tests:
	$(MAKE) -C src_ocanren6same-streams all
compile_ocanren7tests:
	$(MAKE) -C src_ocanren7more-inline all
compile_ocanren8tests:
	$(MAKE) -C src_ocanren8dumb_and2opts all
compile_ocanren9tests:
	$(MAKE) -C src_ocanren9same-steams+2opts all

.PHONY: check_submodules
check_submodules:
#	if [ -d "faster-miniKanren" ]; then (git submodule init && git submodule update --remote); fi
#	if [ -d "microKanren" ];       then (git submodule init && git submodule update --remote); fi
#	if [ -d "ocanrendefault" ];    then (git submodule init && git submodule update --remote); fi
#	if [ -d "ocanrenflat" ];       then (git submodule init && git submodule update --remote); fi

compile: check_submodules
compile: prepare_ocanren \
	compile_ocanren1tests \
	compile_ocanren2tests \
	compile_ocanren3tests \
	compile_ocanren4tests \
	compile_ocanren5tests \
	compile_ocanren6tests \
	compile_ocanren7tests \
	compile_ocanren8tests \
	compile_ocanren9tests \
	compile_scm \
	#compile_simple_scm \
	#compile_muscm \
	#compile_rkt \


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
	$(MAKE) -C ocanren-master        clean
	$(MAKE) -C ocanren2patTree       clean
	$(MAKE) -C ocanren3MKstreams     clean
	$(MAKE) -C ocanren4fastDiseq     clean
	$(MAKE) -C ocanren5set-var-val   clean
	$(MAKE) -C ocanren6same-streams  clean
	$(MAKE) -C ocanren7more-inline   clean
	$(MAKE) -C ocanren8dumb_and2opts clean
	$(MAKE) -C ocanren9same-steams+2opts clean
	$(MAKE) -C src_lisps               clean
	$(MAKE) -C src_ocanren1master        clean
	$(MAKE) -C src_ocanren2patTree       clean
	$(MAKE) -C src_ocanren3MKstreams     clean
	$(MAKE) -C src_ocanren4fastDiseq     clean
	$(MAKE) -C src_ocanren5set-var-val   clean
	$(MAKE) -C src_ocanren6same-streams  clean
	$(MAKE) -C src_ocanren7more-inline   clean
	$(MAKE) -C src_ocanren8dumb_and2opts     clean
	$(MAKE) -C src_ocanren9same-steams+2opts clean
