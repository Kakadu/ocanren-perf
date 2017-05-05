ALL_TEST_MLS = $(shell ls test*.ml -1 | sort -n)

define XXX
IDX  :=
IDX  := $(shell echo $(1) | sed 's/^test\([0-9]*\)_\([a-zA-Z0-9]*\).ml/\1/')

ifneq "$$(IDX)" ""
NAME :=
NAME := $(shell echo $(1) | sed 's/^test\([0-9]*\)_\([a-zA-Z0-9]*\).ml/\2/')

ALL_NATIVES += $$(patsubst %.ml,%.native,$(1))
.PHONY: test$$(IDX)
test$$(IDX): $(1)
	OCAMLPATH=`pwd`/../ocanren4fastDiseq/_build/bundle \
	ocamlbuild -use-ocamlfind $$(patsubst %.ml,%.native,$(1))
endif
endef

#$(info $(call XXX,test900_trees.ml) )
#$(info $(call XXX,test011_quinesNoDiseq.ml) )
$(foreach i,$(ALL_TEST_MLS), $(eval $(call XXX,$(i)) ) )

.SUFFIX: .native
.PHONY: all clean $(natives)

all:
	OCAMLPATH=`pwd`/../ocanren4fastDiseq/_build/bundle \
	ocamlbuild -use-ocamlfind $(ALL_NATIVES)

clean:
	$(RM) -fr _build test*.native
