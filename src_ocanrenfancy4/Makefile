TESTS=001 002 003 004 005 006 007 008 009 900
mls := $(foreach n,$(TESTS),$(wildcard test$(n)*))
natives := $(patsubst %.ml,%.native,$(mls) )

#$(info $(natives))
#SUFFIXES: .native
.PHONY: all clean $(natives)

all:
	OCAMLPATH=`pwd`/../ocanren4fancy/_build/bundle \
	ocamlbuild -use-ocamlfind $(natives)

clean:
	$(RM) -fr _build test*.native

test900_trees.native:
	OCAMLPATH=`pwd`/../ocanren4fancy/_build/bundle \
	ocamlbuild -use-ocamlfind $@
