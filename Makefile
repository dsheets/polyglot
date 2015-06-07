PRODUCTS=polyglot.cma polyglot.cmxa polyglot.cmxs polyglot.cmi
.PHONY: all lib tool install clean polyglot_main.native $(PRODUCTS) test

OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y -Is cli,unix,lib

DESCRN=$(shell git describe --tags --always || cat polyglot.version)
DESCR=$(shell echo "$(DESCRN)" | tr -d '\n')

DIRTY_FLAG=$(shell git diff-index --quiet HEAD || echo "dirty")
ifeq ($(DIRTY_FLAG),dirty)
DIRTY=$(shell [ -d .git ] && echo "true" || echo "false")
else
DIRTY=false
endif

all: lib tool
lib: $(PRODUCTS)
tool: polyglot

install: lib lib/META
	ocamlfind install polyglot lib/META lib/polyglot.mli \
		$(addprefix _build/lib/,$(PRODUCTS))

lib/META: lib/META.in
	sed -e "s/%{polyglot:version}%/$(DESCR)/" < lib/META.in > lib/META

polyglot_main.native: polyglotVersion.ml
	$(OCAMLBUILD) polyglot_main.native

polyglot.cma:
	$(OCAMLBUILD) polyglot.cma

polyglot.cmxa:
	$(OCAMLBUILD) polyglot.cmxa

polyglot.cmxs:
	$(OCAMLBUILD) polyglot.cmxs

polyglot: polyglot_main.native
	ln -f polyglot_main.native polyglot

polyglotVersion.ml:
	printf "let git_descr = \""              > polyglotVersion.ml
	printf "$(DESCR)"                       >> polyglotVersion.ml
	printf "\"\nlet git_dirty = $(DIRTY)\n" >> polyglotVersion.ml

test:
	$(MAKE)
	$(MAKE) -C test
	$(MAKE) -C test test

clean:
	ocamlbuild -clean
	rm -f polyglotVersion.ml polyglot lib/META
	$(MAKE) -C test clean
