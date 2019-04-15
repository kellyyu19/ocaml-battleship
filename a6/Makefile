MODULES=battleship command state main textgrid
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean
	