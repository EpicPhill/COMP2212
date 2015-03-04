OBJS = parser.cmo lexer.cmo  operations.cmo expr.cmo main.cmo

DEPEND += expr.ml lexer.ml parser.ml

all: $(DEPEND) $(OBJS) mysplinterpreter

include .depend

mysplinterpreter: $(OBJS) main.cmo
	@echo Linking $@
	ocamlc -o $@ $(OBJS)

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
	   c TAGS *~

depend:: $(DEPEND)
	ocamldep $(INCLUDE) *.mli *.ml > .depend
