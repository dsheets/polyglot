.PHONY: all test polyglot_tree.native clean

all: test.native

test.native: test.ml
	ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y test.native

polyglot_tree.native:
	ocamlbuild -use-ocamlfind -cflags -w,@f@p@u@y -Is lib polyglot_tree.native

test: test.native polyglot_tree.native
	./test.native

clean:
	ocamlbuild -clean
