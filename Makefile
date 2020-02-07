CMO=lexer.cmo parser.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=clang
FLAGS=

all: $(BIN)

$(BIN):$(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c $<

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc -v $<

clean:
	rm -f *.cm[io] *.o

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
