.DEFAULT_GOAL: all
all:

.SUFFIX: .native .byte .ml

ALL_TEST_MLS = $(shell ls test*.ml -1 | sort -n)
ALL_TEST_MLS := $(filter-out test900_trees.ml,$(ALL_TEST_MLS))
# $(info $(ALL_TEST_MLS))

define XXX
IDX  :=
IDX  := $(shell echo $(1) | sed 's/^test\([0-9]*\)_\([a-zA-Z0-9]*\).ml/\1/')

ifneq "$$(IDX)" ""
NAME :=
NAME := $(shell echo $(1) | sed 's/^test\([0-9]*\)_\([a-zA-Z0-9]*\).ml/\2/')

NATIVE_$(1) = $$(patsubst %.ml,%.native,$(1))
ALL_NATIVES += $$(NATIVE_$(1))

.PHONY: test$$(IDX) $(1)
$$(NATIVE_$(1)):
	OCAMLPATH=`pwd`/../ocanren6same-streams/_build/bundle \
	ocamlbuild -use-ocamlfind $$@

test$$(IDX): $$(NATIVE_$(1))
endif
endef

#$(info $(call XXX,test900_trees.ml) )
#$(info $(call XXX,test011_quinesNoDiseq.ml) )
$(foreach i,$(ALL_TEST_MLS), $(eval $(call XXX,$(i)) ) )

.PHONY: all clean $(ALL_NATIVES)

all:
	OCAMLPATH=`pwd`/../ocanren6same-streams/_build/bundle \
	ocamlbuild -use-ocamlfind $(ALL_NATIVES)

# .PHONY: testXXX_evalo.native
# testXXX_evalo.native:
# 	OCAMLPATH=`pwd`/../ocanren6same-streams/_build/bundle \
# 	ocamlbuild -use-ocamlfind $@

clean:
	$(RM) -fr _build test*.native
