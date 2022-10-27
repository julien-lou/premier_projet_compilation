INTERFACE := x86_64.cmi lexer.cmi asyntax.cmi parser.cmi typecheck.cmi asmeditor.cmi
MAIN_OBJS := x86_64.cmo lexer.cmo asyntax.cmo parser.cmo typecheck.cmo asmeditor.cmo main.cmo


all: aritha

test: executeOcaml createExe
	./expression

createExe:
	gcc -no-pie expression.s -o expression

executeOcaml:
	./aritha

aritha: $(INTERFACE) $(MAIN_OBJS)
	ocamlc -o aritha $(MAIN_OBJS)

%.cmi: %.mli
	ocamlc -c $< -o $@
%.cmo: %.ml
	ocamlc -c $< -o $@

x86_64.cmi: x86_64.mli
lexer.cmi: lexer.mli
asyntax.cmi: asyntax.mli
parser.cmi: parser.mli
typecheck.cmi: typecheck.mli
asmeditor.cmi: asmeditor.mli

x86_64.cmo: x86_64.ml
lexer.cmo: lexer.ml
asyntax.cmo: asyntax.ml
parser.cmo: parser.ml
typecheck.cmo: typecheck.ml
asmeditor.cmo: asmeditor.ml
main.cmo: main.ml

clean:
	rm -f aritha expression *.cmi *.cmo *.s

