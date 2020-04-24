MODULES= author example command datatable iohandler main test
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) 

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

run:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip database.zip *.ml* *.txt *.csv _tags Makefile