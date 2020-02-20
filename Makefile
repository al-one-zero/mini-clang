CMO=ast.cmo lexer.cmo parser.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli ast.mli
SRC= lexer.mll parser.mly main.ml ast.mli ast.ml
BIN=clang
ARCHIVE=jean-marc_fares
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
	echo "exception Parsing_error of string * Lexing.position * Lexing.position" >> parser.mli

clean:
	rm -f *.cm[io] *.o *.output parser.mli parser.ml lexer.ml lexer.mli

archive:
	tar -cvzf $(ARCHIVE).tar.gz --transform 's#^#$(ARCHIVE)/#' $(SRC) README.md tests/

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
